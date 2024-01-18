package io.joern.x2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

trait ExternalCommand {

  protected val IsWin: Boolean = scala.util.Properties.isWin

  // do not prepend any shell layer by default
  // individual frontends may override this
  protected val shellPrefix: Seq[String] = Nil

  protected def handleRunResult(result: Try[Int], stdOut: Seq[String], stdErr: Seq[String]): Try[Seq[String]] = {
    result match {
      case Success(0) =>
        Success(stdOut)
      case _ =>
        val allOutput = stdOut ++ stdErr
        Failure(new RuntimeException(allOutput.mkString(System.lineSeparator())))
    }
  }

  def run(
    command: String,
    cwd: String,
    separateStdErr: Boolean = false,
    extraEnv: Map[String, String] = Map.empty
  ): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = if (separateStdErr) new ConcurrentLinkedQueue[String] else stdOutOutput
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)
    handleRunResult(
      Try(Process(shellPrefix :+ command, new java.io.File(cwd), extraEnv.toList: _*).!(processLogger)),
      stdOutOutput.asScala.toSeq,
      stdErrOutput.asScala.toSeq
    )
  }

}

object ExternalCommand extends ExternalCommand
