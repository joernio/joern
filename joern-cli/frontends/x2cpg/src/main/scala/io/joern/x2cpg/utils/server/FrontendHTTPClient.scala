package io.joern.x2cpg.utils.server

import java.io.IOException
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.URI
import java.net.http.HttpRequest.BodyPublishers
import java.net.http.HttpResponse.BodyHandlers
import scala.util.Success
import scala.util.Failure
import scala.util.Try

case class FrontendHTTPClient(host: String = FrontendHTTPDefaults.host, port: Int = FrontendHTTPDefaults.port) {

  private val underlyingClient: HttpClient = HttpClient.newBuilder().build()

  def buildRequest(args: Array[String]): HttpRequest = {
    HttpRequest
      .newBuilder()
      .uri(URI.create(s"http://$host:$port/${FrontendHTTPDefaults.route}"))
      .header("Content-Type", "application/x-www-form-urlencoded")
      .POST(BodyPublishers.ofString(args.mkString("&")))
      .build()
  }

  def sendRequest(req: HttpRequest): Try[String] = {
    val resp = underlyingClient.send(req, BodyHandlers.ofString())
    resp match {
      case r if r.statusCode() == 200 => Success(resp.body())
      case r => Failure(new IOException(s"Sending request failed with code ${r.statusCode()}: ${r.body()}"))
    }
  }

}
