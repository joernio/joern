package io.shiftleft.joern.server

import javax.servlet.http.HttpServletRequest
import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats, JValue}

import scala.util.Try

class JoernController(implicit val swagger: Swagger)
    extends ScalatraServlet
    with NativeJsonSupport
    with SwaggerSupport {

  protected implicit val jsonFormats: Formats = DefaultFormats
  protected val applicationDescription = "Joern-Server REST API"

  override def readJsonFromBody(bd: String): JValue = {
    val json = Try(super.readJsonFromBody(bd))
    if (json.isFailure) {
      halt(400, "error parsing json")
    } else {
      json.get
    }
  }

  before() {
    contentType = formats("json")
  }

  val create =
    (apiOperation[List[Flower]]("create")
      summary "Create a code property graph"
      tags ""
      parameter queryParam[List[String]]("filenames").description("File/Directory names"))

  post("/create" //, operation(create)
  ) {
    val filenames = Try[List[String]](parsedBody.extract[List[String]]).getOrElse(List())
    if (filenames.isEmpty) {
      halt(400, "`filenames` not given or invalid")
    }
  }

  val findBySlug =
    (apiOperation[Flower]("findBySlug")
      summary "Find by a flower by its slug"
      tags "Flowers"
      parameters pathParam[String]("slug").description("Slug of flower that needs to be fetched")
      responseMessage ResponseMessage(404, "Slug Not Found"))

  get("/:slug", operation(findBySlug)) {
    FlowerData.all find (_.slug == params("slug")) match {
      case Some(b) => b
      case None    => halt(404)
    }
  }
}

case class Flower(slug: String, name: String)

object FlowerData {

  /**
    * Some fake flowers data so we can simulate retrievals.
    */
  var all =
    List(Flower("yellow-tulip", "Yellow Tulip"), Flower("red-rose", "Red Rose"), Flower("black-rose", "Black Rose"))
}
