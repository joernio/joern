package io.joern.dataflowengineoss.semanticsloader

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.Method

trait Semantics {

  /** Useful for `Semantics` that benefit from having some kind of internal state tailored to the current CPG.
    */
  def initialize(cpg: Cpg): Unit

  def forMethod(method: Method): Option[FlowSemantic]
}

/** The empty Semantics */
object NoSemantics extends Semantics {

  override def initialize(cpg: Cpg): Unit = {}

  override def forMethod(method: Method): Option[FlowSemantic] = None
}

case class FlowSemantic(methodFullName: String, mappings: List[FlowPath] = List.empty, regex: Boolean = false)

object FlowSemantic {

  def from(methodFullName: String, mappings: List[?], regex: Boolean = false): FlowSemantic = {
    FlowSemantic(
      methodFullName,
      mappings.map {
        case (src: Int, dst: Int)                                 => FlowMapping(src, dst)
        case (srcIdx: Int, src: String, dst: Int)                 => FlowMapping(srcIdx, src, dst)
        case (src: Int, dstIdx: Int, dst: String)                 => FlowMapping(src, dstIdx, dst)
        case (srcIdx: Int, src: String, dstIdx: Int, dst: String) => FlowMapping(srcIdx, src, dstIdx, dst)
        case x: FlowMapping                                       => x
      },
      regex
    )
  }

}

abstract class FlowNode

/** Collects parameters and return nodes under a common trait. This trait acknowledges their argument index which is
  * relevant when a caller wants to coordinate relevant tainted flows through specific arguments and the return flow.
  */
trait ParamOrRetNode extends FlowNode {

  /** Temporary backward compatible idx field.
    *
    * @return
    *   the argument index.
    */
  def index: Int
}

/** A parameter where the index of the argument matches the position of the parameter at the callee. The name is used to
  * match named arguments if used instead of positional arguments.
  *
  * @param index
  *   the position or argument index.
  * @param name
  *   the name of the parameter.
  */
case class ParameterNode(index: Int, name: Option[String] = None) extends ParamOrRetNode

object ParameterNode {
  def apply(index: Int, name: String): ParameterNode = ParameterNode(index, Option(name))
}

/** Represents explicit mappings or special cases.
  */
sealed trait FlowPath

/** Maps flow between arguments based on how they interact as parameters at the callee.
  *
  * @param src
  *   source of the flow.
  * @param dst
  *   destination of the flow.
  */
case class FlowMapping(src: FlowNode, dst: FlowNode) extends FlowPath

object FlowMapping {
  def apply(from: Int, to: Int): FlowMapping = FlowMapping(ParameterNode(from), ParameterNode(to))

  def apply(fromIdx: Int, from: String, toIdx: Int, to: String): FlowMapping =
    FlowMapping(ParameterNode(fromIdx, from), ParameterNode(toIdx, to))

  def apply(fromIdx: Int, from: String, toIdx: Int): FlowMapping =
    FlowMapping(ParameterNode(fromIdx, from), ParameterNode(toIdx))

  def apply(from: Int, toIdx: Int, to: String): FlowMapping = FlowMapping(ParameterNode(from), ParameterNode(toIdx, to))

}

/** Represents an instance where parameters are not sanitized, may affect the return value, and do not cross-taint. e.g.
  * foo(1, 2) = 1 -> 1, 2 -> 2, 1 -> -1, 2 -> -1
  *
  * The main benefit is that this works for unbounded parameters e.g. VARARGS. Note this does not taint 0 -> 0.
  */
object PassThroughMapping extends FlowPath
