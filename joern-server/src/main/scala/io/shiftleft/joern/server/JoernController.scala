package io.shiftleft.joern.server

import akka.actor.ActorSystem
import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.joern.{CpgLoader, JoernParse}
import javax.script.ScriptEngineManager
import org.json4s.ParserUtil.ParseException
import org.scalatra._
import org.scalatra.swagger._
import org.scalatra.json._
import org.json4s.{DefaultFormats, Formats, JString, JValue}
import org.slf4j.LoggerFactory

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

case class CreateCpgRequest(filenames: List[String])
case class QueryRequest(query: String)

case class StatusResponse(isCpgLoaded: Boolean)
case class QueryResponse(response: String)

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
  private var queryResult: Option[String] = None

  before() {
    contentType = formats("json")
  }

  private val createBuilder =
    (apiOperation[Unit]("create")
      summary "Create a code property graph"
      tags ""
      parameter queryParam[List[String]]("filenames").description("File/Directory names"))

  post("/create", operation(createBuilder)) {
    handleOrReportAsInvalid[CreateCpgRequest] { v =>
      val filenames = v.filenames
      if (filenames.isEmpty) {
        halt(400, "`filenames` not given or invalid")
      }
      if (filenames.count(File(_).exists) != filenames.size) {
        halt(400, "Not all specified files exist")
      }
      createCpg(filenames)
      response.setHeader("Location", s"/status")
      Accepted()
    }

    def createCpg(filenames: List[String]): AsyncResult =
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

  private val statusBuilder = (apiOperation[String]("status")
    summary "Status of the CPG"
    tags "")

  get("/status", operation(statusBuilder)) {
    Ok(StatusResponse(isCpgLoaded = cpg.isDefined))
  }

  private val queryBuilder = (apiOperation[String]("query")
    summary "query"
    tags "")

  post("/query", operation(queryBuilder)) {
    handleOrReportAsInvalid[QueryRequest] { v =>
      if (!cpg.isDefined) {
        BadRequest("CPG is not loaded")
      } else {
        val query = v.query
        logger.info(s"received query: $query")
        runQuery(query)
        Accepted()
      }
    }

    def runQuery(query: String): AsyncResult =
      new AsyncResult {
        val is = Future {
          Try {
            logger.info("Running query")
            import javax.script.ScriptEngineManager
            val e = new ScriptEngineManager().getEngineByName("scala")
            e.put("aCpg", cpg.get)
            e.eval(s"""
                import io.shiftleft.codepropertygraph.Cpg
              | val cpg = aCpg.asInstanceOf[io.shiftleft.codepropertygraph.Cpg]
              | $query
            """.stripMargin).toString
          } match {
            case Success(v)         => queryResult = Some(v)
            case Failure(exception) => queryResult = Some(exception.toString)
          }
        }
      }
  }

  val queryResultBuilder = (apiOperation[String]("queryresult")
    summary "query result"
    tags "")

  get("/queryresult", operation(queryResultBuilder)) {
    if (queryResult.isDefined) {
      val result = queryResult.get
      queryResult = None
      Ok(result)
    } else {
      Accepted("Query still in progress")
    }
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

  private def handleOrReportAsInvalid[RequestType](handler: RequestType => ActionResult)(
      implicit mt: Manifest[RequestType]): ActionResult = {
    Try(parsedBody.extract[RequestType]) match {
      case Success(v) => handler(v)
      case Failure(exception) => {
        BadRequest(s"Invalid create request: $exception")
      }
    }
  }

}
