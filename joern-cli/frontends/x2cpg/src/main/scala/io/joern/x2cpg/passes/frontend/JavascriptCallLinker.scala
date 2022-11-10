package io.joern.x2cpg.passes.frontend

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators, PropertyNames, nodes}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Declaration, Identifier, Local}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.NodeOps
import overflowdb.traversal.jIteratortoTraversal

import scala.collection.mutable

/** The Javascript specific call linker links static call sites (by full name) and call sites to methods in the same
  * file (by name).
  */
class JavascriptCallLinker(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private type MethodsByNameAndFileType = mutable.HashMap[(String, String), nodes.Method]
  private type MethodsByFullNameType    = mutable.HashMap[String, nodes.Method]

  private val JS_EXPORT_NAMES  = IndexedSeq("module.exports", "exports")
  private val JS_EXPORT_PREFIX = "<export>::"

  private def isStaticSingleAssignmentLocal(ident: nodes.Identifier): Boolean =
    ident.refsTo
      .collectAll[nodes.Local]
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
          case assignee: nodes.Identifier
              if isStaticSingleAssignmentLocal(assignee) && assignee.method.name == ":program" =>
            Some(assignee.name)
          case assignee: nodes.Call
              if assignee.methodFullName == Operators.fieldAccess &&
                (JS_EXPORT_NAMES.contains(assignee.argument(1).code) || assignee.code.startsWith("_tmp_")) =>
            Some(assignee.argument(2).asInstanceOf[nodes.FieldIdentifier].canonicalName)
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
        methodsByFullName
          .get(call.methodFullName)
          .foreach { method =>
            diffGraph.addEdge(call, method, EdgeTypes.CALL)
          }
      } else {
        callOffOfRequire(call) match {
          case Some(requirePaths: Seq[String]) =>
            val originalMethodFullName = call.methodFullName
            requirePaths.flatMap(rp => methodsByNameAndFile.get((rp, call.name))).foreach { method =>
              diffGraph.addEdge(call, method, EdgeTypes.CALL)
              diffGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, method.fullName)
            }
            // If there are more than one call edges then it is unsound to set METHOD_FULL_NAME to something
            if (call.outE(EdgeTypes.CALL).size > 1)
              diffGraph.setNodeProperty(call, PropertyNames.METHOD_FULL_NAME, originalMethodFullName)
          case None =>
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

  private def callReceiverOption(callNode: nodes.Call): Option[nodes.Expression] =
    callNode._receiverOut.nextOption().map(_.asInstanceOf[nodes.Expression])

  /** If a call is made on a receiver that contains the result of a <code>require</code> then the
    * [[io.joern.jssrc2cpg.passes.RequirePass]] should have marked the ref [[Local]] node with the file name under
    * <code>DYNAMIC_TYPE_HINT_FULL_NAME</code>.
    * @param call
    *   the call to investigate for a <code>require</code> reference.
    * @return
    *   the argument given to the declaring require if present.
    */
  private def callOffOfRequire(call: nodes.Call): Option[Seq[String]] = {
    callReceiverOption(call).flatMap {
      case c: Call if c.methodFullName == Operators.fieldAccess =>
        c.argument(1).collectAll[Identifier].refsTo.collectFirst {
          case local: Local if local.dynamicTypeHintFullName.exists(_.startsWith(JS_EXPORT_PREFIX)) =>
            local.dynamicTypeHintFullName.filter(_.startsWith(JS_EXPORT_PREFIX)).map(_.stripPrefix(JS_EXPORT_PREFIX))
        } match {
          case Some(requirePaths: Seq[String]) => Some(requirePaths)
          case _                               => None
        }
      case _ => None
    }
  }

  private def fromFieldAccess(c: nodes.Call): Option[String] =
    if (c.methodFullName == Operators.fieldAccess && JS_EXPORT_NAMES.contains(c.argument(1).code)) {
      Some(c.argument(2).code)
    } else {
      None
    }

  // Obtain method name for dynamic calls where the receiver is an identifier.
  private def getReceiverIdentifierName(call: nodes.Call): Option[String] = {
    callReceiverOption(call).flatMap {
      case identifier: nodes.Identifier => Some(identifier.name)
      case block: nodes.Block =>
        block.astChildren.lastOption.flatMap {
          case c: nodes.Call => fromFieldAccess(c)
          case _             => None
        }
      case call: nodes.Call =>
        // TODO: remove this if, once we no longer care about compat with CPGs from January 2022 (comma operator is now a block)
        if (call.methodFullName == "<operator>.commaright") {
          call.argumentOption(2).flatMap {
            case c: nodes.Call => fromFieldAccess(c)
            case _             => None
          }
        } else {
          fromFieldAccess(call)
        }
      case _ =>
        None
    }
  }

}
