package io.joern.x2cpg.utils.server

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.X2CpgMain
import io.joern.x2cpg.utils.Environment
import net.freeutils.httpserver.HTTPServer
import net.freeutils.httpserver.HTTPServer.Context
import org.slf4j.LoggerFactory
import scopt.OParser

import java.net.ServerSocket
import java.nio.file.Paths
import java.util.concurrent.{ExecutorService, Executors, Phaser, Semaphore, TimeUnit}
import scala.annotation.tailrec
import scala.concurrent.TimeoutException
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.util.Failure
import scala.util.Random
import scala.util.Success
import scala.util.Try

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

  private object server extends HTTPServer(0 /* port 0 means the OS chooses a random open port for us */ ) {
    def chosenPort: Int = serv.getLocalPort

    server.getVirtualHost(null).addContexts(frontendHTTPHandler)
    setExecutor(FrontendHTTPServer.this.executor)
  }

  /** Handler for HTTP requests, providing functionality to handle specific routes.
    *
    * @param server
    *   The underlying HTTP server instance.
    */
  private object frontendHTTPHandler {

    /** Handles POST requests to the "/run" endpoint.
      *
      * This method is annotated to handle POST requests directed to the `/run` path. The request `req` is expected to
      * include `input`, `output`, and (optionally) frontend arguments (unbounded). The request is expected to be sent
      * `application/x-www-form-urlencoded`. The provided `X2CpgFrontend` is run with these input/output/arguments and
      * the resulting CPG output path is returned in the response `resp` and status code 200. In case of a failure,
      * status code 400 is sent together with a response containing the reason.
      *
      * @param req
      *   The HTTP request received by the server.
      * @param resp
      *   The HTTP response to be sent by the server.
      * @return
      *   The HTTP status code for the response.
      */
    @Context(value = "/run", methods = Array("POST"))
    def run(req: server.Request, resp: server.Response): Int = {
      synchronized {
        runningRequests += 1
      }

      try {
        resp.getHeaders.add("Content-Type", "text/plain")
        resp.getHeaders.add("Connection", "close")

        val params = req.getParamsList.asScala
        val outputDir = params
          .collectFirst { case Array(arg, value) if arg == "output" => value }
          .getOrElse(X2CpgConfig.defaultOutputPath)
        val arguments = params.collect {
          case Array(arg, value) if arg == "input"        => Array(value)
          case Array(arg, value) if value.strip().isEmpty => Array(s"--$arg")
          case Array(arg, value)                          => Array(s"--$arg", value)
        }.flatten
        logger.debug("Got POST with arguments: " + arguments.mkString(" "))

        Try(handleRequest(arguments.toArray)) match {
          case Failure(exception) =>
            resp.send(400, exception.getMessage)
          case Success(_) =>
            resp.send(200, outputDir)
        }
      } finally {
        synchronized {
          runningRequests -= 1
          lastRequest = System.nanoTime()
          notifyAll()
        }
      }

      0
    }
  }

  /** Stops the underlying HTTP server if it is running.
    *
    * This method checks if the `underlyingServer` is defined and, if so, stops the server. It also logs a debug message
    * indicating that the server has been stopped. If the server is not running, this method does nothing.
    */
  def stop(): Unit = {
    if (isRunning) {
      executor.shutdown()
      server.stop()
      isRunning = false;
      logger.debug("Server stopped.")
    }
  }

  private var runningRequests = 0
  private var lastRequest     = System.nanoTime()
  private var isRunning       = false

  /** Starts the server and returns the server's randomly chosen port
    */
  def startup(): Int = {
    require(!isRunning)
    server.start()

    server.chosenPort
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
