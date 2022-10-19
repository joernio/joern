package io.joern.console.cpgqlserver

import cask.model.{Request, Response}
import io.joern.console.embammonite.{EmbeddedAmmonite, HasUUID, QueryResult}

import java.util.concurrent.ConcurrentHashMap
import java.util.{Base64, UUID}
import cask.model.Response.Raw
import cask.router.Result
import ujson.Obj

object CPGLSError extends Enumeration {
  val parseError: CPGLSError.Value = Value("cpgqls_query_parse_error")
}

class CPGQLServer(
  ammonite: EmbeddedAmmonite,
  serverHost: String,
  serverPort: Int,
  serverAuthUsername: String = "",
  serverAuthPassword: String = ""
) extends WebServiceWithWebSocket[QueryResult](serverHost, serverPort, serverAuthUsername, serverAuthPassword) {

  @cask.websocket("/connect")
  override def handler(): cask.WebsocketResult = super.handler()

  @basicAuth()
  @cask.get("/result/:uuidParam")
  override def getResult(uuidParam: String)(isAuthorized: Boolean): Response[Obj] =
    super.getResult(uuidParam)(isAuthorized)

  @basicAuth()
  @cask.postJson("/query")
  def postQuery(query: String)(isAuthorized: Boolean): Response[Obj] = {
    if (!isAuthorized) unauthorizedResponse
    else {
      val uuid = ammonite.queryAsync(query) { result =>
        returnResult(result)
      }
      Response(ujson.Obj("success" -> true, "uuid" -> uuid.toString), 200)
    }
  }

  override def resultToJson(result: QueryResult, success: Boolean): Obj = {
    ujson.Obj("success" -> success, "uuid" -> result.uuid.toString, "stdout" -> result.out)
  }

  initialize()
}

abstract class WebServiceWithWebSocket[T <: HasUUID](
  serverHost: String,
  serverPort: Int,
  serverAuthUsername: String = "",
  serverAuthPassword: String = ""
) extends cask.MainRoutes {

  class basicAuth extends cask.RawDecorator {

    def wrapFunction(ctx: Request, delegate: Delegate): Result[Raw] = {
      val authString                           = requestToAuthString(ctx)
      val Array(user, password): Array[String] = authStringToUserAndPwd(authString)
      val isAuthorized =
        if (serverAuthUsername == "" && serverAuthPassword == "")
          true
        else
          user == serverAuthUsername && password == serverAuthPassword
      delegate(Map("isAuthorized" -> isAuthorized))
    }

    private def requestToAuthString(ctx: Request): String = {
      try {
        val authHeader     = ctx.exchange.getRequestHeaders.get("authorization").getFirst
        val strippedHeader = authHeader.replaceFirst("Basic ", "")
        new String(Base64.getDecoder.decode(strippedHeader))
      } catch {
        case _: Exception => ""
      }
    }

    private def authStringToUserAndPwd(authString: String): Array[String] = {
      val split = authString.split(":")
      if (split.length == 2) {
        Array(split(0), split(1))
      } else {
        Array("", "")
      }
    }
  }

  override def port: Int = serverPort

  override def host: String = serverHost

  var openConnections                     = Set.empty[cask.WsChannelActor]
  val resultMap                           = new ConcurrentHashMap[UUID, (T, Boolean)]()
  val unauthorizedResponse: Response[Obj] = Response(ujson.Obj(), 401, headers = Seq("WWW-Authenticate" -> "Basic"))

  def handler(): cask.WebsocketResult = {
    cask.WsHandler { connection =>
      // TODO this can't be called from scala3 because it's using scala2 macros...
      connection.send(cask.Ws.Text("connected"))
      openConnections += connection
      cask.WsActor {
        case cask.Ws.Error(e) =>
          println("Connection error: " + e.getMessage)
          openConnections -= connection
        case cask.Ws.Close(_, _) | cask.Ws.ChannelClosed() =>
          println("Connection closed.")
          openConnections -= connection
      }
    }
  }

  def getResult(uuidParam: String)(isAuthorized: Boolean): Response[Obj] = {
    val res = if (!isAuthorized) {
      unauthorizedResponse
    } else {
      val uuid =
        try {
          UUID.fromString(uuidParam)
        } catch {
          case _: IllegalArgumentException => null
        }
      val finalRes = if (uuid == null) {
        ujson.Obj("success" -> false, "err" -> "UUID parameter is incorrectly formatted")
      } else {
        val resFromMap = resultMap.remove(uuid)
        if (resFromMap == null) {
          ujson.Obj("success" -> false, "err" -> "No result found for specified UUID")
        } else {
          resultToJson(resFromMap._1, resFromMap._2)
        }
      }
      Response(finalRes, 200)
    }
    res
  }

  def returnResult(result: T): Unit = {
    resultMap.put(result.uuid, (result, true))
    openConnections.foreach { connection =>
      connection.send(cask.Ws.Text(result.uuid.toString))
    }
    Response(ujson.Obj("success" -> true, "uuid" -> result.uuid.toString), 200)
  }

  def resultToJson(result: T, b: Boolean): Obj

  initialize()
}
