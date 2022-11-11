package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators, PropertyNames}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{NodeOps, jIteratortoTraversal}

import scala.collection.mutable

/** The Javascript specific call linker links static call sites (by full name) and call sites to methods in the same
  * file (by name).
  */
class JavascriptCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private type MethodsByNameAndFileType = mutable.HashMap[(String, String), Method]
  private type MethodsByFullNameType    = mutable.HashMap[String, Method]

  private val JS_EXPORT_NAMES  = IndexedSeq("module.exports", "exports")
  private val JS_EXPORT_PREFIX = "<export>::"

  private def isStaticSingleAssignmentLocal(ident: Identifier): Boolean =
    ident.refsTo
      .collectAll[Local]
      .referencingIdentifiers
      .argumentIndex(1)
      .inAssignment
      .size == 1

  private def createMethodsByNameAndFile(): (MethodsByNameAndFileType, MethodsByFullNameType) = {
    val methodsByNameAndFile = new MethodsByNameAndFileType()
    val methodsByFullName    = new MethodsByFullNameType()

    cpg.method.foreach { method =>
      methodsByNameAndFile.put((method.filename, method.name), method)
      methodsByFullName.put(method.fullName, method)

      // also find anonymous functions assigned to a global variable by the var name
      // (i.e. code that does `var foo = function() {}`)
      method.start
        .fullName(".*::program:anonymous\\d*")
        .flatMap(_._methodRefViaRefIn)
        .argumentIndex(2)
        .inCall
        .nameExact(Operators.assignment)
        .argument(1)
        .flatMap {
          case assignee: Identifier if isStaticSingleAssignmentLocal(assignee) && assignee.method.name == ":program" =>
            Some(assignee.name)
          case assignee: Call
              if assignee.methodFullName == Operators.fieldAccess &&
                (JS_EXPORT_NAMES.contains(assignee.argument(1).code) || assignee.code.startsWith("_tmp_")) =>
            Some(assignee.argument(2).asInstanceOf[FieldIdentifier].canonicalName)
          case _ => None
        }
        .headOption
        .foreach { name => methodsByNameAndFile.put((method.filename, name), method) }
    }

    (methodsByNameAndFile, methodsByFullName)
  }

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val (methodsByNameAndFileType, methodsByFullName) = createMethodsByNameAndFile()
    linkCallSites(diffGraph, methodsByNameAndFileType, methodsByFullName)
  }

  private def linkCallSites(
    diffGraph: DiffGraphBuilder,
    methodsByNameAndFile: MethodsByNameAndFileType,
    methodsByFullName: MethodsByFullNameType
  ): Unit = {
    cpg.call.foreach { call =>
      if (call.dispatchType == DispatchTypes.STATIC_DISPATCH) {
        // Case 1: This call is static so the methodFullName points us to our callee, typically reserved for <operator>
        methodsByFullName
          .get(call.methodFullName)
          .foreach { method =>
            diffGraph.addEdge(call, method, EdgeTypes.CALL)
          }
      } else {
        val imports = receiverImports(call)
        if (imports.nonEmpty) {
          // Case 2: This call's receiver is associated with a "require" statement's module.exports
          val callees = imports.flatMap(rp => methodsByNameAndFile.get((rp, call.name)))
          callees.foreach { method =>
            diffGraph.addEdge(call, method, EdgeTypes.CALL)
            diffGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, method.fullName)
          }
          // If there are more than one call edges then it is unsound to set METHOD_FULL_NAME to something
          if (callees.size > 1)
            diffGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, "<unknownFullName>")
        } else {
          // Case 3: We look for a definition of the function within the scope of the caller's file
          getReceiverIdentifierName(call).foreach { name =>
            for (
              file   <- call.file.headOption;
              method <- methodsByNameAndFile.get((file.name, name))
            ) {
              diffGraph.addEdge(call, method, EdgeTypes.CALL)
              diffGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, method.fullName)
            }
          }
        }
      }
    }
  }

  private def callReceiverOption(callNode: Call): Option[Expression] =
    callNode._receiverOut.nextOption().map(_.asInstanceOf[Expression])

  /** If a call is made on a receiver that contains the result of a <code>require</code> then the
    * [[io.joern.jssrc2cpg.passes.RequirePass]] should have marked the ref [[Local]] and [[Identifier]] nodes with the
    * file name under <code>TYPE_FULL_NAME</code> (if immutable) or <code>DYNAMIC_TYPE_HINT_FULL_NAME</code> (if
    * mutable).
    * @param call
    *   the call to investigate for a <code>require</code> reference.
    * @return
    *   the argument given to the declaring require(s) if present.
    */
  private def receiverImports(call: Call): Seq[String] = {
    callReceiverOption(call)
      .flatMap {
        case c: Call if c.methodFullName == Operators.fieldAccess =>
          c.argument(1).collectAll[Identifier].collectFirst {
            case receiver: Identifier if receiver.dynamicTypeHintFullName.exists(_.startsWith(JS_EXPORT_PREFIX)) =>
              receiver.dynamicTypeHintFullName
                .filter(_.startsWith(JS_EXPORT_PREFIX))
                .map(_.stripPrefix(JS_EXPORT_PREFIX))
            case receiver: Identifier if receiver.typeFullName.startsWith(JS_EXPORT_PREFIX) =>
              Seq(receiver.typeFullName.stripPrefix(JS_EXPORT_PREFIX))
          }
        case _ => None
      }
      .toSeq
      .flatten
  }

  private def fromFieldAccess(c: Call): Option[String] =
    if (c.methodFullName == Operators.fieldAccess && JS_EXPORT_NAMES.contains(c.argument(1).code)) {
      Some(c.argument(2).code)
    } else {
      None
    }

  // Obtain method name for dynamic calls where the receiver is an identifier.
  private def getReceiverIdentifierName(call: Call): Option[String] = {
    callReceiverOption(call).flatMap {
      case identifier: Identifier => Some(identifier.name)
      case block: Block =>
        block.astChildren.lastOption.flatMap {
          case c: Call => fromFieldAccess(c)
          case _       => None
        }
      case call: Call =>
        // TODO: remove this if, once we no longer care about compat with CPGs from January 2022 (comma operator is now a block)
        if (call.methodFullName == "<operator>.commaright") {
          call.argumentOption(2).flatMap {
            case c: Call => fromFieldAccess(c)
            case _       => None
          }
        } else {
          fromFieldAccess(call)
        }
      case _ =>
        None
    }
  }

}
