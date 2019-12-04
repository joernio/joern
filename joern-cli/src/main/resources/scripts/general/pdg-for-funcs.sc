import ammonite.main.Router.main
import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Direction

import io.shiftleft.codepropertygraph.generated.nodes.Expression
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, NodeTypes, nodes}
import io.shiftleft.dataflowengine.language._
import io.shiftleft.joern.console.Console.cpg
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.expressions.{Call, Return}

import scala.jdk.CollectionConverters._

case class FunctionPDG(functionName: String, reachableNodes: List[PDGNode])
case class PDGNode(id: Long,
                   code: String,
                   edgesIn: Map[String, Int],
                   edgesOut: Map[String, Int],
                   properties: Map[String, AnyRef])

private def getEdgesCount(expr: Expression, direction: Direction): Map[String, Int] = {
  expr.edges(Direction.IN).asScala.toSeq.groupBy(_.label()).map { case (k, v) =>
    k -> v.size
  }
}

private def getReachableNodes[T <: nodes.Expression, S <: nodes.Expression](sink: Steps[T],
                                                                            source: Steps[S]): List[PDGNode] = {
  sink.reachableBy(source).dedup.l.map { node =>
    val inEdges = getEdgesCount(node, Direction.IN)
    val outEdges = getEdgesCount(node, Direction.OUT)

    PDGNode(node.getId, node.code, inEdges, outEdges, node.valueMap.asScala.toMap)
  }
}

cpg.method.internal.map { method =>
  val sink = method.local.referencingIdentifiers.dedup
  val callSource = new Call(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.CALL).cast[nodes.Call]).dedup
  val retSource = new Return(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.RETURN).cast[nodes.Return]).dedup

  val reachableCallNodes = getReachableNodes(sink, callSource)
  val reachableReturnNodes = getReachableNodes(sink, retSource)

  FunctionPDG(method.name, reachableCallNodes ++ reachableReturnNodes)
}.l

