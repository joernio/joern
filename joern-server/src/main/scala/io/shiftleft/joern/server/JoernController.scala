package io.shiftleft.joern.server

import akka.actor.ActorSystem
import io.shiftleft.codepropertygraph.Cpg
import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats, JValue}

import scala.collection.mutable.ListBuffer
import scala.concurrent.ExecutionContext
import scala.util.Try

class JoernController(system: ActorSystem)(implicit val swagger: Swagger)
    extends ScalatraServlet
    with NativeJsonSupport
    with SwaggerSupport
    with FutureSupport {

  protected implicit val jsonFormats: Formats = DefaultFormats
  protected val applicationDescription = "Joern-Server REST API"
  protected implicit def executor: ExecutionContext = system.dispatcher

  private val cpgs : ListBuffer[Cpg] = ListBuffer()

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
    (apiOperation[Unit]("create")
      summary "Create a code property graph"
      tags ""
      parameter queryParam[List[String]]("filenames").description("File/Directory names"))

  post("/create", operation(create)) {
    val filenames = Try[List[String]](parsedBody.extract[List[String]]).getOrElse(List())
    if (filenames.isEmpty) {
      halt(400, "`filenames` not given or invalid")
    }
    response.setHeader("Location", s"/cpg/${cpgs.size}")
    202
  }

}
