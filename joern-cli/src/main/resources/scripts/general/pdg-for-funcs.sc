import gremlin.scala._
import io.circe.Encoder
import io.circe.generic.semiauto._
import io.circe.syntax._
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
                   properties: Map[String, String])

implicit val pdgNodeEncoder: Encoder[PDGNode] = deriveEncoder[PDGNode]
implicit val functionPdgEncoder: Encoder[FunctionPDG] = deriveEncoder[FunctionPDG]

private def getEdgesCount(expr: Expression, direction: Direction): Map[String, Int] = {
  expr.edges(direction).asScala.toSeq.groupBy(_.label()).map { case (k, v) =>
    k -> v.size
  }
}

private def getReachableNodes[T <: nodes.Expression, S <: nodes.Expression](sink: Steps[T],
                                                                            source: Steps[S]): List[PDGNode] = {
  sink.reachableBy(source).dedup.l.map { node =>
    val inEdges = getEdgesCount(node, Direction.IN)
    val outEdges = getEdgesCount(node, Direction.OUT)
    val properties = node.valueMap.asScala.map { case (k, v) =>
      k -> v.toString
    }.toMap

    PDGNode(node.getId, node.code, inEdges, outEdges, properties)
  }
}

cpg.method.internal.map { method =>
  val sink = method.local.referencingIdentifiers.dedup
  val callSource = new Call(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.CALL).cast[nodes.Call]).dedup
  val retSource = new Return(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.RETURN).cast[nodes.Return]).dedup

  val reachableCallNodes = getReachableNodes(sink, callSource)
  val reachableReturnNodes = getReachableNodes(sink, retSource)

  FunctionPDG(method.name, reachableCallNodes ++ reachableReturnNodes)
}.l.asJson
