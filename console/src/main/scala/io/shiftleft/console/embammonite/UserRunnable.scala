package io.shiftleft.console.embammonite

import org.slf4j.{Logger, LoggerFactory}

import java.io.{BufferedReader, PrintWriter}
import java.util.UUID
import java.util.concurrent.BlockingQueue
import scala.util.Try

class UserRunnable(queue: BlockingQueue[Job], writer: PrintWriter, reader: BufferedReader, errReader: BufferedReader)
    extends Runnable {

  private val logger: Logger = LoggerFactory.getLogger(classOf[UserRunnable])

  private val magicEchoSeq: Seq[Char] = List(27, 91, 57, 57, 57, 57, 68, 27, 91, 48, 74, 64, 32).map(_.toChar)
  private val endMarker = """.*END: ([0-9a-f\-]+)""".r

  override def run(): Unit = {
    try {
      var terminate = false;
      while (!(terminate && queue.isEmpty)) {
        val job = queue.take()
        if (isTerminationMarker(job)) {
          terminate = true
        } else {
          sendQueryToAmmonite(job)
          val stdoutPair = stdOutUpToMarker()
          val stdOutput = stdoutPair.get
          val errOutput = exhaustStderr()
          val result = new QueryResult(stdOutput, errOutput, job.uuid)
          job.observer(result)
        }
      }
    } catch {
      case _: InterruptedException =>
        logger.info("Interrupted WriterThread")
    }
    logger.debug("WriterThread terminated gracefully")
  }

  private def isTerminationMarker(job: Job): Boolean = {
    job.uuid == null && job.query == null
  }

  private def sendQueryToAmmonite(job: Job): Unit = {
    writer.println(job.query.trim)
    writer.println(s""""END: ${job.uuid}"""")
    writer.println(s"""throw new RuntimeException("END: ${job.uuid}")""")
    writer.flush()
  }

  private def stdOutUpToMarker(): Option[String] = {
    var currentOutput: String = ""
    var line = reader.readLine()
    while (line != null) {
      if (!line.startsWith(magicEchoSeq) && !line.isEmpty) {
        val uuid = uuidFromLine(line)
        if (uuid.isEmpty) {
          currentOutput += line + "\n"
        } else {
          return Some(currentOutput)
        }
      }
      line = reader.readLine()
    }
    None
  }

  private def uuidFromLine(line: String): Iterator[UUID] = {
    endMarker.findAllIn(line).matchData.flatMap { m =>
      Try { UUID.fromString(m.group(1)) }.toOption
    }
  }

  private def exhaustStderr(): String = {
    var currentOutput = ""
    var line = errReader.readLine()
    while (line != null) {
      val uuid = uuidFromLine(line)
      if (uuid.isEmpty) {
        currentOutput += line
      } else {
        return currentOutput
      }
      line = errReader.readLine()
    }
    currentOutput
  }

}
