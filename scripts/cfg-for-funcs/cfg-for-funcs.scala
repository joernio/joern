import scala.collection.JavaConverters._

import io.circe.syntax._
import io.circe.generic.semiauto._
import io.circe.{Encoder, Json}

import io.shiftleft.codepropertygraph.generated.nodes.CfgNode

import gremlin.scala._
import org.apache.tinkerpop.gremlin.structure.Edge
import org.apache.tinkerpop.gremlin.structure.VertexProperty


final case class CfgForFuncsFunction(function: String, id: String, CFG: List[CfgNode])
final case class CfgForFuncsResult(file: String, functions: List[CfgForFuncsFunction])

implicit val encodeFuncResult: Encoder[CfgForFuncsResult] = deriveEncoder
implicit val encodeFuncFunction: Encoder[CfgForFuncsFunction] = deriveEncoder
implicit val encodeVertex: Encoder[CfgNode] =
  (node: CfgNode) =>
    Json.obj(
      ("id", Json.fromString(node.toString)),
      ("edges",
        Json.fromValues(
          node.graph.E
            .hasLabel("CFG")
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

CfgForFuncsResult(
  cpg.file.name.l.head,
  cpg.method.name.l.map { methodName =>
    val method = cpg.method.name(methodName)
    CfgForFuncsFunction(methodName, cpg.method.name(methodName).l.head.toString, method.cfgNode.l)
  }
).asJson
