import scala.collection.JavaConverters._

import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty


final case class AstForFuncsFunction(function: String, id: String, AST: List[AstNode])
final case class AstForFuncsResult(file: String, functions: List[AstForFuncsFunction])

implicit val encodeFuncResult: Encoder[AstForFuncsResult] = deriveEncoder
implicit val encodeFuncFunction: Encoder[AstForFuncsFunction] = deriveEncoder
implicit val encodeVertex: Encoder[AstNode] =
  (node: AstNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues(
          node.graph.E
            .hasLabel("AST")
            .l
            .collect {
              case e if e.inVertex == node  => e
              case e if e.outVertex == node => e
            }
            .map { edge: Edge =>
              Json.obj(
                ("id", Json.fromString(edge.toString)),
                ("in", Json.fromString(edge.inVertex().toString)),
                ("out", Json.fromString(edge.outVertex().toString))
              )
            })),
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
    val method = cpg.method.name(methodName)
    AstForFuncsFunction(methodName, cpg.method.name(methodName).l.head.toString, method.astChildren.l)
  }
).asJson
