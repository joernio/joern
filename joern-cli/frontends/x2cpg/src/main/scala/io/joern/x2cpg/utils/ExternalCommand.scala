package io.joern.x2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import org.apache.commons.lang.StringUtils
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters._

object ExternalCommand {

  private val IS_WIN: Boolean = scala.util.Properties.isWin

  private val shellPrefix: Seq[String] = if (IS_WIN) "cmd" :: "/c" :: Nil else "sh" :: "-c" :: Nil

  def run(
    command: String,
    cwd: String,
    separateStdErr: Boolean = false,
    extraEnv: Map[String, String] = Map.empty
  ): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = if (separateStdErr) new ConcurrentLinkedQueue[String] else stdOutOutput
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)

    Process(shellPrefix :+ command, new java.io.File(cwd), extraEnv.toList: _*).!(processLogger) match {
      case 0 =>
        Success(stdOutOutput.asScala.toSeq)
      case _ =>
        Failure(new RuntimeException(stdErrOutput.asScala.mkString(System.lineSeparator())))
    }
  }

  private val COMMAND_AND: String = " && "

  def toOSCommand(command: String): String = if (IS_WIN) command + ".cmd" else command

  def runMultiple(command: String, inDir: String = ".", extraEnv: Map[String, String] = Map.empty): Try[String] = {
    val dir           = new java.io.File(inDir)
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = new ConcurrentLinkedQueue[String]
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)
    val commands      = command.split(COMMAND_AND).toSeq
    commands.map { cmd =>
      val cmdWithQuotesAroundDir = StringUtils.replace(cmd, inDir, s"'$inDir'")
      Try(Process(cmdWithQuotesAroundDir, dir, extraEnv.toList: _*).!(processLogger)).getOrElse(1)
    }.sum match {
      case 0 =>
        Success(stdOutOutput.asScala.mkString(System.lineSeparator()))
      case _ =>
        val allOutput = stdOutOutput.asScala ++ stdErrOutput.asScala
        Failure(new RuntimeException(allOutput.mkString(System.lineSeparator())))
    }
  }
}
