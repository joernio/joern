package io.joern.x2cpg.utils.server

import io.joern.x2cpg.X2Cpg
import io.joern.x2cpg.X2CpgConfig
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.X2CpgMain
import net.freeutils.httpserver.HTTPServer
import net.freeutils.httpserver.HTTPServer.Context
import org.slf4j.LoggerFactory

import java.util.concurrent.Executors
import java.util.concurrent.ExecutorService
import scala.util.Failure
import scala.util.Success
import scala.util.Try

import scala.jdk.CollectionConverters.ListHasAsScala

object FrontendHTTPServer {

  private lazy val SingleThreadExecutor: ExecutorService     = Executors.newSingleThreadExecutor()
  private lazy val CachedThreadPoolExecutor: ExecutorService = Executors.newCachedThreadPool()
  private val DefaultExecutor: ExecutorService               = CachedThreadPoolExecutor

}

trait FrontendHTTPServer[T <: X2CpgConfig[T], X <: X2CpgFrontend[T]] { this: X2CpgMain[T, X] =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  private var underlyingServer: Option[HTTPServer] = None

  protected def newDefaultConfig(): T

  /** Override to switch between single-threaded sequential or multi-threaded asynchronous execution of requests */
  protected val executor: ExecutorService = FrontendHTTPServer.DefaultExecutor

  protected class FrontendHTTPHandler(val server: HTTPServer) {
    @Context(value = "/run", methods = Array("POST"))
    def run(req: server.Request, resp: server.Response): Int = {
      resp.getHeaders.add("Content-Type", "text/plain")

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

      val config = X2Cpg
        .parseCommandLine(arguments.toArray, cmdLineParser, newDefaultConfig())
        .getOrElse(newDefaultConfig())
      Try(frontend.run(config)) match {
        case Failure(exception) =>
          resp.send(400, exception.getMessage)
        case Success(_) =>
          resp.send(200, outputDir)
      }
      0
    }
  }

  def stop(): Unit = {
    underlyingServer.foreach { server =>
      server.stop()
      logger.debug("Server stopped.")
    }
  }

  def startup(config: T): Unit = {
    underlyingServer = Some(new HTTPServer(config.serverPort))
    val host = underlyingServer.get.getVirtualHost(null)
    underlyingServer.get.setExecutor(executor)
    host.addContexts(new FrontendHTTPHandler(underlyingServer.get))
    try {
      underlyingServer.get.start()
      logger.debug(s"Server started on ${Option(host.getName).getOrElse("localhost")}:${config.serverPort}.")
    } finally {
      Runtime.getRuntime.addShutdownHook(new Thread(() => {
        stop()
      }))
    }
  }

}
