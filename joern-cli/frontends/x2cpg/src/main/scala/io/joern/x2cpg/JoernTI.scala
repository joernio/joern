package io.joern.x2cpg

import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory
import io.joern.slicing._

import java.io.{InputStreamReader, OutputStreamWriter}
import java.net.Socket
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.collection.mutable
import scala.sys.process.Process
import scala.util.{Failure, Success, Try}

/** Interface class with the Joern type inference project.
  *
  * @see
  *   <a href="https://github.com/joernio/type-inference-models">JoernTI GitHub Project</a>
  * @throws RuntimeException
  *   if JoernTI is not installed (with spawnProcess = true) or if one cannot connect to the JoernTI socket.
  */
final class JoernTI(val hostname: String = "localhost", val port: Int = 1337, spawnProcess: Boolean = false)
    extends AutoCloseable {

  private val log = LoggerFactory.getLogger(classOf[JoernTI])

  private val server: Option[Process] = if (spawnProcess) {
    if (isJoernTIAvailable) {
      Option(ExternalCommand.startProcess("joernti server"))
    } else {
      throw new RuntimeException("Unable to spawn the JoernTI process as the `joernti` executable cannot be found!")
    }
  } else {
    None
  }

  server match {
    case Some(proc) if !proc.isAlive() => throw new RuntimeException("Could not spawn the JoernTI server!")
    case _                             => log.info("Server started successfully")
  }

  private val socket: Socket = acquireSocket match {
    case Failure(exception) => throw new RuntimeException("Unable to connect to JoernTI server!", exception)
    case Success(conn)      => conn
  }

  private val out: OutputStreamWriter = new OutputStreamWriter(socket.getOutputStream, StandardCharsets.UTF_8)
  private val in: InputStreamReader   = new InputStreamReader(socket.getInputStream, StandardCharsets.UTF_8)

  /** Allows us to try an operation n times before propagating the exception.
    */
  @tailrec
  private def retry[T](n: Int)(fn: => T): T = {
    try {
      fn
    } catch {
      case e: Throwable =>
        Try(Thread.sleep(100))
        if (n > 1) retry(n - 1)(fn)
        else throw e
    }
  }

  private def acquireSocket: Try[Socket] =
    Try(retry(3) { new Socket(hostname, port) })

  private def isJoernTIAvailable: Boolean =
    ExternalCommand.run("joernti version").isSuccess

  def infer(slice: ProgramUsageSlice): Try[List[InferenceResult]] = Try {
    retry(3) { Try(out.write(slice.toJson)) }
    val buf = new Array[Char](1024)
    val sb  = new mutable.StringBuilder()
    retry(3) { while (in.read(buf) != -1) sb.append(buf) }
    // TODO: Convert properly
//    sb.toString().asInstanceOf[List[InferenceResult]]
    List.empty
  }

  override def close(): Unit = {
    Try(in.close())
    Try(out.close())
    Try(socket.close())
    server.foreach(_.destroy())
  }

}
