package io.shiftleft.joern.server

import akka.actor.ActorSystem
import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.joern.{CpgLoader, JoernParse}
import org.json4s.ParserUtil.ParseException
import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats, JString, JValue}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class CreateCpgRequest(filenames: List[String])

class JoernController(system: ActorSystem)(implicit val swagger: Swagger)
    extends ScalatraServlet
    with NativeJsonSupport
    with SwaggerSupport
    with FutureSupport {

  val logger = LoggerFactory.getLogger(getClass)
  protected implicit val jsonFormats: Formats = DefaultFormats
  protected val applicationDescription = "Joern-Server REST API"
  protected implicit def executor: ExecutionContext = system.dispatcher

  private var cpg: Option[Cpg] = None

  before() {
    contentType = formats("json")
  }

  override def readJsonFromBody(bd: String): JValue = {
    Try(super.readJsonFromBody(bd)) match {
      case Success(value) => value
      case Failure(exception) =>
        exception match {
          case _: ParseException => {
            logger.warn(exception.getMessage)
            new JString("")
          }
          case e => throw (e)
        }
    }
  }

  val create =
    (apiOperation[Unit]("create")
      summary "Create a code property graph"
      tags ""
      parameter queryParam[List[String]]("filenames").description("File/Directory names"))

  post("/create", operation(create)) {
    Try(parsedBody.extract[CreateCpgRequest]) match {
      case Success(v) => {
        val filenames = v.filenames
        if (filenames.isEmpty) {
          halt(400, "`filenames` not given or invalid")
        }
        if (filenames.count(File(_).exists) != filenames.size) {
          halt(400, "Not all specified files exist")
        }
        createCpg(filenames)
        response.setHeader("Location", s"/cpg")
        202
      }
      case Failure(exception) => {
        println(exception)
        logger.warn(s"Invalid create request: $exception")
        400
      }
    }
  }

  private def createCpg(filenames: List[String]): AsyncResult =
    new AsyncResult {
      val is = Future {
        val cpgFilename = "/tmp/cpg.bin.zip"
        logger.info(s"Attempting to create CPG for: ${filenames.mkString(",")}")
        JoernParse.parse(filenames.toArray, cpgFilename)
        cpg = Some(CpgLoader.load(cpgFilename))
        logger.info("CPG is ready")
      }
    }

}
