import scala.collection.JavaConverters._

import io.circe.syntax._
import io.circe.{Encoder, Json}
import io.shiftleft.codepropertygraph.generated.nodes.AstNode

import org.apache.tinkerpop.gremlin.structure.VertexProperty

implicit val encodeVertex: Encoder[AstNode] = (node: AstNode) =>
  Json.obj(
    ("id", Json.fromString(node.toString)),
    ("properties", Json.fromValues(node.properties().asScala.toList.map { p: VertexProperty[_] =>
      Json.obj(
        ("key", Json.fromString(p.key())),
        ("value", Json.fromString(p.value().toString))
      )
    }))
  )

Json.obj(
  ("file", Json.fromString(cpg.file.name.l.head)),
  ("functions", Json.fromValues(cpg.method.name.l.map { methodName =>
    val method = cpg.method.name(methodName)
    Json.obj(("function", Json.fromString(methodName)),
      ("AST", Json.fromValues(method.astChildren.l.map(_.asJson))))
  }))
)
