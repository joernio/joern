package io.joern.dataflowengineoss.language.nodemethods

import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, Method}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

class ExpressionMethods[NodeType <: Expression](val node: NodeType) extends AnyVal {

  /** Determine whether evaluation of the call this argument is a part of results in usage of this argument.
    */
  def isUsed(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg
    s.isEmpty || s.exists(_.mappings.exists { case (srcIndex, _) => srcIndex == node.argumentIndex })
  }

  /** Determine whether evaluation of the call this argument is a part of results in definition of this argument.
    */
  def isDefined(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg.l
    s.isEmpty || s.exists { semantic =>
      semantic.mappings.exists { case (_, dstIndex) =>
        dstIndex == node.argumentIndex
      }
    }
  }

  /** Determines if this node and the given target node are arguments to the same call.
    * @param other
    *   the node to compare
    * @return
    *   true if these nodes are arguments to the same call, false if otherwise.
    */
  def isArgToSameCallWith(other: Expression): Boolean =
    node.astParent.collectAll[Call].headOption.equals(other.astParent.collectAll[Call].headOption)

  /** Determines if this node has a flow to the given target node in the defined semantics.
    * @param tgt
    *   the target node to check.
    * @param semantics
    *   the pre-defined flow semantics.
    * @return
    *   true if there is flow defined between the two nodes, false if otherwise.
    */
  def hasDefinedFlowTo(tgt: Expression)(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg.l
    s.isEmpty || s.exists { semantic =>
      semantic.mappings.exists { case (srcIndex, dstIndex) =>
        srcIndex == node.argumentIndex && dstIndex == tgt.argumentIndex
      }
    }
  }

  /** Retrieve flow semantic for the call this argument is a part of.
    */
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
