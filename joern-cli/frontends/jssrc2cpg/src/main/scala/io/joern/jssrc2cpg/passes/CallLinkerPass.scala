package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators, nodes}
import io.shiftleft.passes.SimpleCpgPass
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.{NodeOps, jIteratortoTraversal, toNodeTraversal}

import scala.collection.mutable

/** The Javascript specific call linker links static call sites (by full name) and call sites to methods in the same
  * file (by name).
  */
class CallLinkerPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private type MethodsByNameAndFileType = mutable.HashMap[(String, String), nodes.Method]
  private type MethodsByFullNameType    = mutable.HashMap[String, nodes.Method]

  private val JS_EXPORT_NAMES = IndexedSeq("module.exports", "exports")

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
        .in(EdgeTypes.REF)
        .collectAll[nodes.MethodRef]
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
                JS_EXPORT_NAMES.contains(assignee.argument(1).code) =>
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
          .foreach(diffGraph.addEdge(call, _, EdgeTypes.CALL))
      } else {
        getReceiverIdentifierName(call).foreach { name =>
          for (
            file   <- call.file.headOption;
            method <- methodsByNameAndFile.get((file.name, name))
          ) { diffGraph.addEdge(call, method, EdgeTypes.CALL) }
        }
      }
    }
  }

  private def callReceiverOption(callNode: nodes.Call): Option[nodes.Expression] =
    callNode._receiverOut.nextOption().map(_.asInstanceOf[nodes.Expression])

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
