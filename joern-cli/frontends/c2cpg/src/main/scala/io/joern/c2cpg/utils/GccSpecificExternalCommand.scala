package io.joern.c2cpg.utils

import io.shiftleft.semanticcpg.utils.ExternalCommand

import scala.util.{Failure, Success, Try}

object GccSpecificExternalCommand {

  import ExternalCommand.ExternalCommandResult

  private val IsWin = scala.util.Properties.isWin

  def run(command: Seq[String], cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    ExternalCommand.run(command, Option(cwd), mergeStdErrInStdOut = true, extraEnv) match {
      case ExternalCommandResult(0, stdOut, _) =>
        Success(stdOut)
      case ExternalCommandResult(1, stdOut, _) if IsWin && IncludeAutoDiscovery.gccAvailable() =>
        // the command to query the system header file locations within a Windows
        // environment always returns Success(1) for whatever reason...
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, _) =>
        Failure(new RuntimeException(stdOut.mkString(System.lineSeparator())))
    }
  }

}
