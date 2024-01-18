package io.joern.c2cpg.utils

import scala.util.{Failure, Success, Try}

object ExternalCommand extends io.joern.x2cpg.utils.ExternalCommand {

  private val IS_WIN: Boolean = scala.util.Properties.isWin

  override def handleRunResult(result: Try[Int], stdOut: Seq[String], stdErr: Seq[String]): Try[Seq[String]] = {
    result match {
      case Success(0) =>
        Success(stdOut)
      case Success(1) if IS_WIN && IncludeAutoDiscovery.gccAvailable() =>
        // the command to query the system header file locations within a Windows
        // environment always returns Success(1) for whatever reason...
        Success(stdErr)
      case _ =>
        Failure(new RuntimeException(stdOut.mkString(System.lineSeparator())))
    }
  }

}
