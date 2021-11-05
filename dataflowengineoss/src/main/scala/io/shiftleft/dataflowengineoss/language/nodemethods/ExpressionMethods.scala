package io.shiftleft.dataflowengineoss.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Expression, Method}
import io.shiftleft.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class ExpressionMethods[NodeType <: Expression](val node: NodeType) extends AnyVal {

  /**
    * Determine whether evaluation of the call this argument is a part of results
    * in usage of this argument.
    * */
  def isUsed(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg
    s.isEmpty || s.exists(_.mappings.exists { case (srcIndex, _) => srcIndex == node.order })
  }

  /**
    * Determine whether evaluation of the call this argument is a part of results
    * in definition of this argument.
    * */
  def isDefined(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg
    s.isEmpty || s.exists { semantic =>
      semantic.mappings.exists {
        case (_, dstIndex) =>
          dstIndex == node.order
      }
    }
  }

  /**
    * Retrieve flow semantic for the call this argument is a part of.
    * */
  def semanticsForCallByArg(implicit semantics: Semantics): Traversal[FlowSemantic] = {
    argToMethods(node).flatMap { method =>
      semantics.forMethod(method.fullName)
    }
  }

  private def argToMethods(arg: Expression): Traversal[Method] = {
    arg.inCall.flatMap { call =>
      NoResolve.getCalledMethods(call)
    }
  }

}
