/* ast-for-funcs.scala

   This script returns a Json representation of the AST for each method contained in the currently loaded CPG.

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
    "file" : "/path/to/free/free.c",
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

import scala.collection.JavaConverters._

import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty

val astEdges = cpg.graph.E.hasLabel("AST").l

final case class AstForFuncsFunction(function: String, id: String, AST: List[AstNode])
final case class AstForFuncsResult(file: String, functions: List[AstForFuncsFunction])

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
        Json.fromValues(
          astEdges.collect {
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

AstForFuncsResult(
  cpg.file.name.l.head,
  cpg.method.name.l.map { methodName =>
    val method = cpg.method.nameExact(methodName)
    val methodId = cpg.method.nameExact(methodName).l.head.toString
    AstForFuncsFunction(methodName, methodId, method.astChildren.l)
  }
).asJson
