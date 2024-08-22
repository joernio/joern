package io.joern.x2cpg.utils

import java.io.File
import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

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

  def run(command: String, cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = new ConcurrentLinkedQueue[String]
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)
    val process = shellPrefix match {
      case Nil => Process(command, new java.io.File(cwd), extraEnv.toList*)
      case _   => Process(shellPrefix :+ command, new java.io.File(cwd), extraEnv.toList*)
    }
    handleRunResult(Try(process.!(processLogger)), stdOutOutput.asScala.toSeq, stdErrOutput.asScala.toSeq)
  }

  // We use the java ProcessBuilder API instead of the Scala version because it
  // offers the possibility to merge stdout and stderr into one stream of output.
  // Maybe the Scala version also offers this but since there is no documentation
  // I was not able to figure it out.
  def runWithMergeStdoutAndStderr(command: String, cwd: String): (Int, String) = {
    val builder = new ProcessBuilder()
    builder.command(command.split(' ')*)
    builder.directory(new File(cwd))
    builder.redirectErrorStream(true)

    val process     = builder.start()
    val outputBytes = process.getInputStream.readAllBytes()
    val returnValue = process.waitFor()

    (returnValue, new String(outputBytes))
  }
}

object ExternalCommand extends ExternalCommand
