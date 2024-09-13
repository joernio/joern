package io.joern.dataflowengineoss.language.nodemethods

import io.joern.dataflowengineoss.semanticsloader.*
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Expression, Method}
import io.shiftleft.semanticcpg.language.*

class ExpressionMethods[NodeType <: Expression](val node: NodeType) extends AnyVal {

  /** Determine whether evaluation of the call this argument is a part of results in usage of this argument.
    */
  def isUsed(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg
    s.isEmpty || s.exists(_.mappings.exists {
      case FlowMapping(ParameterNode(_, Some(srcName)), _) if node.argumentName.isDefined =>
        srcName == node.argumentName.get
      case FlowMapping(ParameterNode(srcIndex, _), _)    => srcIndex == node.argumentIndex
      case PassThroughMapping if node.argumentIndex != 0 => true
      case _                                             => false
    })
  }

  /** Determine whether evaluation of the call this argument is a part of results in definition of this argument.
    */
  def isDefined(implicit semantics: Semantics): Boolean = {
    val s = semanticsForCallByArg.l
    s.isEmpty || s.exists { semantic =>
      semantic.mappings.exists {
        case FlowMapping(_, ParameterNode(_, Some(dstName))) if node.argumentName.isDefined =>
          dstName == node.argumentName.get
        case FlowMapping(_, ParameterNode(dstIndex, _))    => dstIndex == node.argumentIndex
        case PassThroughMapping if node.argumentIndex != 0 => true
        case _                                             => false
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
    node.astParent.start.collectAll[Call].headOption.equals(other.astParent.start.collectAll[Call].headOption)

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
      semantic.mappings.exists {
        case FlowMapping(ParameterNode(_, Some(srcName)), ParameterNode(_, Some(dstName)))
            if node.argumentName.isDefined && tgt.argumentName.isDefined =>
          srcName == node.argumentName.get && dstName == tgt.argumentName.get
        case FlowMapping(ParameterNode(_, Some(srcName)), ParameterNode(dstIndex, _)) if node.argumentName.isDefined =>
          srcName == node.argumentName.get && dstIndex == tgt.argumentIndex
        case FlowMapping(ParameterNode(srcIndex, _), ParameterNode(_, Some(dstName))) if tgt.argumentName.isDefined =>
          srcIndex == node.argumentIndex && dstName == tgt.argumentName.get
        case FlowMapping(ParameterNode(srcIndex, _), ParameterNode(dstIndex, _)) =>
          srcIndex == node.argumentIndex && dstIndex == tgt.argumentIndex
        case PassThroughMapping if tgt.argumentIndex == node.argumentIndex || tgt.argumentIndex == -1 => true
        case _                                                                                        => false
      }
    }
  }

  /** Retrieve flow semantic for the call this argument is a part of.
    */
  def semanticsForCallByArg(implicit semantics: Semantics): Iterator[FlowSemantic] = {
    argToMethods(node).flatMap(semantics.forMethod)
  }

  private def argToMethods(arg: Expression): Iterator[Method] = {
    arg.inCall.flatMap { call =>
      NoResolve.getCalledMethods(call)
    }
  }

}
