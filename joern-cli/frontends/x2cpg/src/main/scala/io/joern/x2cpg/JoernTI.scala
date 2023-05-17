package io.joern.x2cpg

import io.circe.parser._
import io.joern.slicing._
import io.joern.x2cpg.utils.ExternalCommand
import org.slf4j.LoggerFactory

import java.io.{InputStream, OutputStreamWriter}
import java.net.Socket
import java.nio.charset.StandardCharsets
import scala.annotation.tailrec
import scala.sys.process.Process
import scala.util.{Failure, Success, Try, Using}

/** Interface class with the Joern type inference project.
  *
  * @see
  *   <a href="https://github.com/joernio/type-inference-models">JoernTI GitHub Project</a>
  * @throws RuntimeException
  *   if JoernTI is not installed (with spawnProcess = true) or if one cannot connect to the JoernTI socket.
  */
final class JoernTI(
  val hostname: String = "localhost",
  val port: Int = 1337,
  spawnProcess: Boolean = false,
  pathToCheckpoints: String = "./data/model_checkpoints/default"
) extends AutoCloseable {

  private val log = LoggerFactory.getLogger(classOf[JoernTI])

  private val server: Option[Process] = if (spawnProcess) {
    if (isJoernTIAvailable) {
      Option(ExternalCommand.startProcess(s"joernti ml $pathToCheckpoints --run-as-server --port 1337"))
    } else {
      throw new RuntimeException("Unable to spawn the JoernTI process as the `joernti` executable cannot be found!")
    }
  } else {
    None
  }

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
    if (slice.objectSlices.sizeIs == 0) return Try(List.empty[InferenceResult])
    val payload = slice.toJson
    acquireSocket match {
      case Failure(exception) =>
        throw new RuntimeException("Unable to connect to JoernTI server!", exception)
      case Success(value) =>
        Using.resource(value) { socket =>
          val out = new OutputStreamWriter(socket.getOutputStream, StandardCharsets.UTF_8)
          val response = retry(3) {
            out.write(s"$payload\r\r")
            out.flush()
            bytes(socket.getInputStream).map(_.toChar).mkString
          }
          if (response.isBlank)
            List.empty[InferenceResult]
          else
            decode[List[InferenceResult]](response) match {
              case Left(exception) =>
                log.error("Unable to read type inference response!", exception)
                List.empty[InferenceResult]
              case Right(res) =>
                res
            }
        }
    }

  }

  def bytes(in: InputStream, initSize: Int = 8192): Array[Byte] = {
    var buf    = new Array[Byte](initSize)
    val step   = initSize
    var pos, n = 0
    while ({
      if (pos + step > buf.length) buf = java.util.Arrays.copyOf(buf, buf.length << 1)
      n = in.read(buf, pos, step)
      n != -1
    }) pos += n
    if (pos != buf.length) buf = java.util.Arrays.copyOf(buf, pos)
    buf
  }

  override def close(): Unit =
    server.foreach(_.destroy())

}
