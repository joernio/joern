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

/** Represents an HTTP client for interacting with a frontend server.
  *
  * This class provides functionality to create and send HTTP requests to a specified frontend server. The server's host
  * needs to be configured.
  *
  * @param port
  *   The port of the frontend server.
  */
case class FrontendHTTPClient(port: Int) {

  /** The underlying HTTP client used to send requests. */
  private val underlyingClient: HttpClient = HttpClient.newBuilder().build()

  /** Builds an HTTP POST request with the given arguments.
    *
    * The request is sent to the configured host and port, with a URI path defined by `FrontendHTTPDefaults.route`. The
    * request body is constructed from the `args` array, which is concatenated into a single string separated by "&" and
    * sent as `application/x-www-form-urlencoded`.
    *
    * @param args
    *   An array of arguments to be included in the POST request body.
    * @return
    *   The constructed `HttpRequest` object.
    */
  def buildRequest(args: Array[String]): HttpRequest = {
    HttpRequest
      .newBuilder()
      .uri(URI.create(s"http://localhost:$port/run"))
      .header("Content-Type", "application/x-www-form-urlencoded")
      .POST(BodyPublishers.ofString(args.mkString("&")))
      .build()
  }

  /** Sends the given HTTP request and returns the response body if successful.
    *
    * This method sends the provided `HttpRequest` built with `buildRequest` using the underlying HTTP client. If the
    * response status code is 200, the response body is returned as a `Success`. If the status code indicates a failure,
    * a `Failure` containing an `IOException` with the error details is returned.
    *
    * @param req
    *   The `HttpRequest` to be sent.
    * @return
    *   A `Try[String]` containing the response body in case of success, or an `IOException` in case of failure.
    */
  def sendRequest(req: HttpRequest): Try[String] = {
    val resp = underlyingClient.send(req, BodyHandlers.ofString())
    resp match {
      case r if r.statusCode() == 200 => Success(resp.body())
      case r => Failure(new IOException(s"Sending request failed with code ${r.statusCode()}: ${r.body()}"))
    }
  }

}
