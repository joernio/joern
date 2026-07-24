package io.joern.x2cpg.utils.server

import io.joern.x2cpg.X2CpgConfig
import org.slf4j.LoggerFactory

import com.sun.net.httpserver.{HttpExchange, HttpHandler, HttpServer}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
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
  * Concurrency model:
  *   - `httpServerRef` is the canonical liveness flag. Reads (`isRunning`) and the at-most-once shutdown claim in
  *     `stop()` go through `AtomicReference`, so cross-thread visibility is guaranteed without holding the intrinsic
  *     monitor.
  *   - The intrinsic monitor (`synchronized`) protects `runningRequests` and `lastRequest`, and is used by
  *     `stopServerAfterTimeout` to `wait`/`notifyAll` on liveness changes. It is held only for short, non-blocking
  *     critical sections.
  *   - `stop()` deliberately does NOT hold the monitor while running `server.stop(0)`/`executor.awaitTermination`,
  *     because in-flight `RunHandler.handle` calls need that same monitor for their `runningRequests -= 1` finally
  *     block — holding it across shutdown would deadlock against them. A brief `synchronized { notifyAll() }` runs
  *     before `awaitTermination` so any thread parked in `stopServerAfterTimeout` is released without waiting for the
  *     executor to drain.
  *   - `startup()` synchronizes on a dedicated `startupLock` only to make the `require(!isRunning)` precondition and
  *     the subsequent server bind/start atomic relative to other `startup()` calls. It uses a separate monitor from the
  *     intrinsic one because it protects unrelated state and never needs to wait on handlers.
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

  /** Capture the context classloader at construction time so handler threads can use it. This is necessary because
    * com.sun.net.httpserver executor threads may not inherit the correct classloader, which causes issues with
    * ConfigFactory.load not finding application.conf resources. Note: this surfaced only with `csharpsrc2cpg/testOnly
    * io.joern.csharpsrc2cpg.io.CSharp2CpgHTTPServerTests` for some reason...
    */
  private val contextClassLoader: ClassLoader = Thread.currentThread().getContextClassLoader

  // Atomic so concurrent stop() calls race on a single CAS rather than racing on a `var` write; a non-null value
  // also serves as the canonical liveness flag, read without holding the monitor via `isRunning`.
  private val httpServerRef: AtomicReference[HttpServer] = new AtomicReference(null)
  // Guarded by the intrinsic monitor of `this`. Mutated by RunHandler (in/out of `handle`) and read by
  // `stopServerAfterTimeout` to decide when the server has been idle long enough to shut down.
  private var runningRequests = 0
  private var lastRequest     = System.nanoTime()
  // Dedicated monitor for `startup()`'s precondition-check + bind/start, kept separate from the intrinsic monitor
  // of `this` so it does not interact with the request/idle-tracking lock used by handlers and the timeout loop.
  private val startupLock = new Object

  private def isRunning: Boolean = httpServerRef.get() != null

  /** Handler for HTTP requests, providing functionality to handle specific routes. */
  private class RunHandler extends HttpHandler {

    /** Handles POST requests to the "/run" endpoint.
      *
      * The request is expected to include `input`, `output`, and (optionally) frontend arguments (unbounded). The
      * request is expected to be sent as a JSON object. String values become option values and null values become
      * flags. The provided `X2CpgFrontend` is run with these input/output/arguments and the resulting CPG output path
      * is returned in the response and status code 200. In case of a failure, status code 400 is sent together with a
      * response containing the reason.
      */
    override def handle(exchange: HttpExchange): Unit = {
      if (exchange.getRequestMethod != "POST") {
        sendResponse(exchange, 405, "Method Not Allowed")
        return
      }

      val previousClassLoader = Thread.currentThread().getContextClassLoader
      Thread.currentThread().setContextClassLoader(contextClassLoader)

      synchronized {
        runningRequests += 1
      }

      try {
        val requestBody = Using(Source.fromInputStream(exchange.getRequestBody, StandardCharsets.UTF_8.name())) {
          _.mkString
        }.getOrElse("")

        Try {
          val params = parseFromJson(requestBody)

          var outputDir = X2CpgConfig.defaultOutputPath
          val arguments = params.flatMap {
            case ("output", Some(value)) =>
              outputDir = value
              List("--output", value)
            case ("input", Some(value)) => List(value)
            case (arg, None)            => List(s"--$arg")
            case (arg, Some(value))     => List(s"--$arg", value)
          }.toArray

          logger.debug("Got POST with arguments: " + arguments.mkString(" "))
          handleRequest(arguments)
          outputDir
        } match {
          case Failure(exception) =>
            exception.printStackTrace()
            sendResponse(exchange, 400, exception.getMessage)
          case Success(outputDir) =>
            sendResponse(exchange, 200, outputDir)
        }
      } finally {
        Thread.currentThread().setContextClassLoader(previousClassLoader)
        synchronized {
          runningRequests -= 1
          lastRequest = System.nanoTime()
          notifyAll()
        }
      }
    }

    private def parseFromJson(body: String): List[(String, Option[String])] = {
      val list = ujson.read(body).obj.toList
      list.flatMap {
        case (key, ujson.Str(value)) => Some((key, Some(value)))
        case (key, ujson.Null)       => Some((key, None))
        case (key, anything) =>
          logger.warn("Ignoring HTTP request argument {} with unsupported json type {}", key, anything.getClass)
          None
      }
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
    * `httpServerRef.getAndSet(null)` atomically claims the server, so concurrent `stop()` calls run the shutdown
    * sequence at most once — only the caller that observes a non-null value proceeds. The intrinsic monitor is NOT held
    * during `server.stop(0)`/`executor.awaitTermination`, because in-flight handlers need that same monitor for their
    * `runningRequests -= 1` finally block; holding it across shutdown would deadlock. `synchronized { notifyAll() }`
    * runs before `awaitTermination` so any thread parked in `stopServerAfterTimeout` is released without waiting for
    * the executor to drain — the loop's `isRunning` predicate already sees `httpServerRef` as `null`.
    */
  def stop(): Unit = {
    val server = httpServerRef.getAndSet(null)
    if (server != null) {
      server.stop(0)
      executor.shutdown()
      synchronized { notifyAll() }
      executor.awaitTermination(10, TimeUnit.SECONDS)
      logger.debug("Server stopped.")
    }
  }

  /** Starts the server and returns the server's randomly chosen port.
    *
    * Synchronizes on `startupLock` (not the intrinsic monitor of `this`) to make the `require(!isRunning)` check and
    * the subsequent bind/start atomic against other `startup()` calls. Using a dedicated lock keeps this independent of
    * the request/idle-tracking monitor used by handlers and `stopServerAfterTimeout`, since the state being protected
    * here (the bind sequence) is unrelated.
    */
  def startup(): Int = startupLock.synchronized {
    require(!isRunning)

    // Create server on port 0 (OS chooses random available port)
    val server = HttpServer.create(new InetSocketAddress(0), 0)
    server.createContext("/run", new RunHandler())
    server.setExecutor(executor)
    // Start before publishing so `stop()` never sees an unstarted server.
    server.start()
    httpServerRef.set(server)

    val port = server.getAddress.getPort
    logger.debug(s"Server started on port $port")
    port
  }

  /** Stops the server, once it hasn't served any new requests for longer than timeout seconds.
    *
    * Loops on the intrinsic monitor: `wait` is woken either by a finishing handler (which calls `notifyAll` in its
    * finally block) or by `stop()`'s trailing `synchronized { notifyAll() }`. The loop predicate uses `isRunning`
    * (atomic read) so that once `stop()` flips `httpServerRef` to `null` the next iteration exits cleanly.
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
