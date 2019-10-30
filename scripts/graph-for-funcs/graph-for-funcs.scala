/* graph-for-funcs.scala

   This script returns a Json representation of the graph resulting in combining the
   AST, CGF, and PDG for each method contained in the currently loaded CPG.

   Input: A valid CPG
   Output: Json

   Running the Script
   ------------------
   see: README.md

   The JSON generated has the following keys:

    "file": The file (as full path) the CPG was generated from
    "functions": Array of all methods contained in the currently loaded CPG
      |_ "function": Method name as String
      |_ "id": Method id as String (String representation of the underlying Method node)
      |_ "AST": see ast-for-funcs script
      |_ "CFG": see cfg-for-funcs script
      |_ "PDG": see pdg-for-funcs script

 */

import scala.collection.JavaConverters._

import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn

import gremlin.scala._
import io.shiftleft.dataflowengine.language._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty

val edges = cpg.graph.E.hasLabel("AST", "CFG").l

final case class GraphForFuncsFunction(function: String,
                                       id: String,
                                       AST: List[AstNode],
                                       CFG: List[AstNode],
                                       PDG: List[AstNode])
final case class GraphForFuncsResult(file: String, functions: List[GraphForFuncsFunction])

implicit val encodeEdge: Encoder[Edge] =
  (edge: Edge) =>
    Json.obj(
      ("id", Json.fromString(edge.toString)),
      ("in", Json.fromString(edge.inVertex().toString)),
      ("out", Json.fromString(edge.outVertex().toString))
    )

implicit val encodeNode: Encoder[AstNode] =
  (node: AstNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues(
          edges.collect {
            case e if e.inVertex == node  => e
            case e if e.outVertex == node => e
          }.map(_.asJson))),
      ("properties", Json.fromValues(node.properties().asScala.toList.map { p: VertexProperty[_] =>
        Json.obj(
          ("key", Json.fromString(p.key())),
          ("value", Json.fromString(p.value().toString))
        )
      }))
    )

implicit val encodeFuncFunction: Encoder[GraphForFuncsFunction] = deriveEncoder
implicit val encodeFuncResult: Encoder[GraphForFuncsResult] = deriveEncoder

GraphForFuncsResult(
  cpg.file.name.l.head,
  cpg.method.name.l.map { methodName =>
    val methodId = cpg.method.nameExact(methodName).l.head.toString

    val astChildren = cpg.method.nameExact(methodName).astChildren.l
    val cfgChildren = cpg.method.nameExact(methodName).cfgNode.l

    val sink = cpg.method.nameExact(methodName).local.evalType(".*").referencingIdentifiers
    val source = cpg.method.parameter
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

    GraphForFuncsFunction(methodName, methodId, astChildren, cfgChildren, pdgChildren)
  }
).asJson
