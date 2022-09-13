package io.joern.x2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object ExternalCommand {

  private val IS_WIN: Boolean =
    scala.util.Properties.isWin

  private val shellPrefix: Seq[String] =
    if (IS_WIN) "cmd" :: "/c" :: Nil else "sh" :: "-c" :: Nil

  def run(command: String, cwd: String, separateStdErr: Boolean = false): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = if (separateStdErr) new ConcurrentLinkedQueue[String] else stdOutOutput
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)

    Process(shellPrefix :+ command, new java.io.File(cwd)).!(processLogger) match {
      case 0 =>
        Success(stdOutOutput.asScala.toSeq)
      case _ =>
        Failure(new RuntimeException(stdErrOutput.asScala.mkString(System.lineSeparator())))
    }
  }
}
