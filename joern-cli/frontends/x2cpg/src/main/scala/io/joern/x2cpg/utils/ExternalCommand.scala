package io.joern.x2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object ExternalCommand {
  def run(
    command: String,
    cwd: String,
    separateStdErr: Boolean = false,
    extraEnv: Map[String, String] = Map.empty
  ): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = if (separateStdErr) new ConcurrentLinkedQueue[String] else stdOutOutput
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)
    Process(command, new java.io.File(cwd), extraEnv.toList: _*).!(processLogger) match {
      case 0 =>
        Success(stdOutOutput.asScala.toSeq)
      case _ =>
        Failure(new RuntimeException(stdErrOutput.asScala.mkString(System.lineSeparator())))
    }
  }
}
