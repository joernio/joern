package io.joern.c2cpg.utils

import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

object ExternalCommand extends io.joern.x2cpg.utils.ExternalCommand {

  override def handleRunResult(result: Try[Int], stdOut: Seq[String], stdErr: Seq[String]): Try[Seq[String]] = {
    result match {
      case Success(0) =>
        Success(stdOut)
      case Success(1) if IsWin && IncludeAutoDiscovery.gccAvailable() =>
        // the command to query the system header file locations within a Windows
        // environment always returns Success(1) for whatever reason...
        Success(stdOut)
      case _ =>
        Failure(new RuntimeException(stdOut.mkString(System.lineSeparator())))
    }
  }

  override def run(command: String, cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val processLogger = ProcessLogger(stdOutOutput.add, stdOutOutput.add)
    val process = shellPrefix match {
      case Nil => Process(command, new java.io.File(cwd), extraEnv.toList: _*)
      case _   => Process(shellPrefix :+ command, new java.io.File(cwd), extraEnv.toList: _*)
    }
    handleRunResult(Try(process.!(processLogger)), stdOutOutput.asScala.toSeq, Nil)
  }

}
