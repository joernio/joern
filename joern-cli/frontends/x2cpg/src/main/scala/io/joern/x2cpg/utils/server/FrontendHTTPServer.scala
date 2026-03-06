package io.joern.x2cpg.utils.server

import io.joern.x2cpg.X2CpgConfig
import org.slf4j.LoggerFactory

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.{InetSocketAddress, URLDecoder}
import java.nio.charset.StandardCharsets
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.compiletime.uninitialized
import scala.io.Source
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.Using

/** Companion object for `FrontendHTTPServer` providing default executor configurations. */
object FrontendHTTPServer {

  /** ExecutorService for single-threaded execution. */
  def singleThreadExecutor(): ExecutorService = Executors.newSingleThreadExecutor()

  /** ExecutorService for cached thread pool execution. */
  def cachedThreadPoolExecutor(): ExecutorService = Executors.newCachedThreadPool()

  /** Default ExecutorService used by `FrontendHTTPServer`. */
  def defaultExecutor(): ExecutorService = cachedThreadPoolExecutor()

  private val logger = LoggerFactory.getLogger(classOf[FrontendHTTPServer])
}

/** A trait representing a frontend HTTP server for handling operations any subclass of `X2CpgMain` may offer via its
  * main function. This trait provides methods and configurations for setting up an HTTP server that processes requests
  * related to `X2CpgMain`. It includes handling request execution either in a single-threaded or multi-threaded manner,
  * depending on the executor configuration.
  *
  * @param executor
  *   ExecutorService used to execute HTTP requests.
  * @param handleRequest
  *   function handling the reequest
  */
class FrontendHTTPServer(executor: ExecutorService, handleRequest: Array[String] => Unit) {
  import FrontendHTTPServer.*

  /** This can be overridden to switch between single-threaded and multi-threaded execution. By default, it uses the
    * cached thread pool executor from `FrontendHTTPServer`.
    */

  private var httpServer: HttpServer = uninitialized
  private var runningRequests        = 0
  private var lastRequest            = System.nanoTime()
  private var isRunning              = false

  /** Handler for HTTP requests, providing functionality to handle specific routes. */
  private class RunHandler extends HttpHandler {

    /** Handles POST requests to the "/run" endpoint.
      *
      * The request is expected to include `input`, `output`, and (optionally) frontend arguments (unbounded). The
      * request is expected to be sent `application/x-www-form-urlencoded`. The provided `X2CpgFrontend` is run with
      * these input/output/arguments and the resulting CPG output path is returned in the response and status code 200.
      * In case of a failure, status code 400 is sent together with a response containing the reason.
      */
    override def handle(exchange: HttpExchange): Unit = {
      if (exchange.getRequestMethod != "POST") {
        sendResponse(exchange, 405, "Method Not Allowed")
        return
      }

      synchronized {
        runningRequests += 1
      }

      try {
        val requestBody = Using(Source.fromInputStream(exchange.getRequestBody, StandardCharsets.UTF_8.name())) {
          _.mkString
        }.getOrElse("")

        val params = parseFormUrlEncoded(requestBody)

        val outputDir = params.getOrElse("output", X2CpgConfig.defaultOutputPath)
        val arguments = params.flatMap {
          case ("input", value)                      => List(value)
          case (arg, value) if value.strip().isEmpty => List(s"--$arg")
          case (arg, value)                          => List(s"--$arg", value)
        }.toArray

        logger.debug("Got POST with arguments: " + arguments.mkString(" "))

        Try(handleRequest(arguments)) match {
          case Failure(exception) =>
            exception.printStackTrace()
            sendResponse(exchange, 400, exception.getMessage)
          case Success(_) =>
            sendResponse(exchange, 200, outputDir)
        }
      } finally {
        synchronized {
          runningRequests -= 1
          lastRequest = System.nanoTime()
          notifyAll()
        }
      }
    }

    /** Parses an application/x-www-form-urlencoded body (e.g. "key1=val1&key2=val2") into key-value pairs. */
    private def parseFormUrlEncoded(body: String): Map[String, String] = {
      if (body.isEmpty) return Map.empty

      def decode(s: String): String = URLDecoder.decode(s, StandardCharsets.UTF_8)

      val pairs = body.split("&")
      pairs.map { pair =>
        val keyAndValue = pair.split("=", 2) // limit 2 so value may contain '='
        val key         = decode(keyAndValue(0))
        val value       = if (keyAndValue.length > 1) decode(keyAndValue(1)) else ""
        key -> value
      }.toMap
    }

    private def sendResponse(exchange: HttpExchange, statusCode: Int, body: String): Unit = {
      val responseBytes = body.getBytes(StandardCharsets.UTF_8)
      exchange.getResponseHeaders.add("Content-Type", "text/plain; charset=UTF-8")
      exchange.getResponseHeaders.add("Connection", "close")
      exchange.sendResponseHeaders(statusCode, responseBytes.length)
      exchange.getResponseBody.write(responseBytes)
      exchange.getResponseBody.close()
    }
  }

  /** Stops the underlying HTTP server if it is running.
    *
    * This method checks if the server is running and, if so, stops the server. It also logs a debug message indicating
    * that the server has been stopped. If the server is not running, this method does nothing.
    */
  def stop(): Unit = {
    if (isRunning) {
      executor.shutdown()
      httpServer.stop(0)
      isRunning = false
      logger.debug("Server stopped.")
    }
  }

  /** Starts the server and returns the server's randomly chosen port
    */
  def startup(): Int = {
    require(!isRunning)

    // Create server on port 0 (OS chooses random available port)
    httpServer = HttpServer.create(new InetSocketAddress(0), 0)
    httpServer.createContext("/run", new RunHandler())
    httpServer.setExecutor(executor)
    httpServer.start()

    isRunning = true

    val port = httpServer.getAddress.getPort
    logger.debug(s"Server started on port $port")
    port
  }

  /** Stops the server, once it hasn't served any new requests for longer than timeout seconds
    */
  def stopServerAfterTimeout(timeoutSeconds: Long): Unit = {
    synchronized {
      while (isRunning) {
        wait(TimeUnit.SECONDS.toMillis(timeoutSeconds))
        if (runningRequests == 0 && System.nanoTime() > lastRequest + TimeUnit.SECONDS.toNanos(timeoutSeconds)) {
          stop()
        }
      }
    }
  }
}
