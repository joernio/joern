/* graph-for-funcs.scala

   This script returns a Json representation of the graph resulting in combining the
   AST, CGF, and PDG for each method contained in the currently loaded CPG.

   Input: A valid CPG
   Output: Json

   Running the Script
   ------------------
   see: README.md

   The JSON generated has the following keys:

   "functions": Array of all methods contained in the currently loaded CPG
     |_ "function": Method name as String
     |_ "id": Method id as String (String representation of the underlying Method node)
     |_ "AST": see ast-for-funcs script
     |_ "CFG": see cfg-for-funcs script
     |_ "PDG": see pdg-for-funcs script
 */

import scala.jdk.CollectionConverters._

import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.semanticcpg.language.types.expressions.generalizations.CfgNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.dataflowengine.language._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.expressions.Call
import io.shiftleft.semanticcpg.language.types.structure.Local
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty

final case class GraphForFuncsFunction(function: String,
                                       id: String,
                                       AST: List[nodes.AstNode],
                                       CFG: List[nodes.AstNode],
                                       PDG: List[nodes.AstNode])
final case class GraphForFuncsResult(functions: List[GraphForFuncsFunction])

implicit val encodeEdge: Encoder[Edge] =
  (edge: Edge) =>
    Json.obj(
      ("id", Json.fromString(edge.toString)),
      ("in", Json.fromString(edge.inVertex().toString)),
      ("out", Json.fromString(edge.outVertex().toString))
    )

implicit val encodeNode: Encoder[nodes.AstNode] =
  (node: nodes.AstNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues((node.inE("AST", "CFG").l ++ node.outE("AST", "CFG").l).map(_.asJson))),
      ("properties", Json.fromValues(node.properties().asScala.toList.map { p: VertexProperty[_] =>
        Json.obj(
          ("key", Json.fromString(p.key())),
          ("value", Json.fromString(p.value().toString))
        )
      }))
    )

implicit val encodeFuncFunction: Encoder[GraphForFuncsFunction] = deriveEncoder
implicit val encodeFuncResult: Encoder[GraphForFuncsResult] = deriveEncoder

@main def main(): Json = {
  GraphForFuncsResult(
    cpg.method.map { method =>
      val methodName = method.fullName
      val methodId = method.toString

      val astChildren = method.astMinusRoot.l

      val cfgChildren = new NodeSteps(
        method.out(EdgeTypes.CONTAINS).filterOnEnd(_.isInstanceOf[nodes.CfgNode]).cast[nodes.CfgNode]
      ).l

      val local = new NodeSteps(
        method
          .out(EdgeTypes.CONTAINS)
          .hasLabel(NodeTypes.BLOCK)
          .out(EdgeTypes.AST)
          .hasLabel(NodeTypes.LOCAL)
          .cast[nodes.Local])
      val sink = local.evalType(".*").referencingIdentifiers.dedup
      val source = new NodeSteps(method.out(EdgeTypes.CONTAINS).hasLabel(NodeTypes.CALL).cast[nodes.Call]).nameNot("<operator>.*").dedup

      val pdgChildren = sink
        .reachableByFlows(source)
        .l
        .flatMap { path =>
          path
            .map {
              case trackingPoint @ (_: MethodParameterIn) => trackingPoint.start.method.head
              case trackingPoint                          => trackingPoint.cfgNode
            }
        }
        .filter(_.toString != methodId)

      GraphForFuncsFunction(methodName, methodId, astChildren, cfgChildren, pdgChildren.distinct)
    }.l
  ).asJson
}
