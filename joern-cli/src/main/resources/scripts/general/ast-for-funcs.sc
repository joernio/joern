/* ast-for-funcs.scala

   This script returns a Json representation of the AST for each method contained in the currently loaded CPG.

   Input: A valid CPG
   Output: Json

   Running the Script
   ------------------
   see: README.md

   The JSON generated has the following keys:

    "functions": Array of all methods contained in the currently loaded CPG
      |_ "function": Method name as String
      |_ "id": Method id as String (String representation of the underlying Method node)
      |_ "AST": Array of all nodes connected via AST edges
          |_ "id": Node id as String (String representation of the underlying AST node)
          |_ "properties": Array of properties of the current node as key-value pair
          |_ "edges": Array of all AST edges where the current node is referenced as inVertex or outVertex
              |_ "id": Edge id as String (String representation of the AST edge)
              |_ "in": Node id as String of the inVertex node (String representation of the inVertex node)
              |_ "out": Node id as String of the outVertex node (String representation of the outVertex node)

   Sample Output
   -------------
   {
    "functions" : [
      {
        "function" : "free_list",
        "id" : "io.shiftleft.codepropertygraph.generated.nodes.Method@b",
        "AST" : [
          {
            "id" : "io.shiftleft.codepropertygraph.generated.nodes.MethodParameterOut@46",
            "edges" : [
              {
                "id" : "io.shiftleft.codepropertygraph.generated.edges.Ast@27adc",
                "in" : "io.shiftleft.codepropertygraph.generated.nodes.MethodParameterOut@46",
                "out" : "io.shiftleft.codepropertygraph.generated.nodes.Method@b"
              }
            ],
            "properties" : [
              {
                "key" : "NAME",
                "value" : "head"
              },
              {
                "key" : "CODE",
                "value" : "struct node *head"
              },
              {
                "key" : "LINE_NUMBER",
                "value" : "7"
              },
              {
                "key" : "EVALUATION_STRATEGY",
                "value" : "BY_VALUE"
              },
              {
                "key" : "TYPE_FULL_NAME",
                "value" : "struct node *"
              },
              {
                "key" : "COLUMN_NUMBER",
                "value" : "15"
              },
              {
                "key" : "ORDER",
                "value" : "1"
              },
              // ...
 */

import gremlin.scala._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Encoder, Json}
import org.apache.tinkerpop.gremlin.structure.{Edge, VertexProperty}

import io.shiftleft.codepropertygraph.generated.nodes.AstNode
import io.shiftleft.joern.console.Console.cpg
import io.shiftleft.semanticcpg.language._

import scala.jdk.CollectionConverters._

final case class AstForFuncsFunction(function: String, id: String, AST: List[AstNode])
final case class AstForFuncsResult(functions: List[AstForFuncsFunction])

implicit val encodeFuncResult: Encoder[AstForFuncsResult] = deriveEncoder
implicit val encodeFuncFunction: Encoder[AstForFuncsFunction] = deriveEncoder

implicit val encodeEdge: Encoder[Edge] =
  (edge: Edge) =>
    Json.obj(
      ("id", Json.fromString(edge.toString)),
      ("in", Json.fromString(edge.inVertex().toString)),
      ("out", Json.fromString(edge.outVertex().toString))
    )

implicit val encodeVertex: Encoder[AstNode] =
  (node: AstNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues((node.inE("AST").l ++ node.outE("AST").l).map(_.asJson))),
      ("properties", Json.fromValues(node.properties().asScala.toList.map { p: VertexProperty[_] =>
        Json.obj(
          ("key", Json.fromString(p.key())),
          ("value", Json.fromString(p.value().toString))
        )
      }))
    )

AstForFuncsResult(
  cpg.method.map { method =>
    val methodName = method.fullName
    val methodId = method.toString
    val astChildren = method.astMinusRoot.l
    AstForFuncsFunction(methodName, methodId, astChildren)
  }.l
).asJson
