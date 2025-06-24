package io.joern.c2cpg.utils

import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.semanticcpg.utils.ExternalCommandResult

import java.nio.file.Paths
import scala.util.{Failure, Success, Try}

object GccSpecificExternalCommand {

  private val IsWin = scala.util.Properties.isWin

  def run(command: Seq[String], cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    ExternalCommand.run(command, Option(Paths.get(cwd)), mergeStdErrInStdOut = true, extraEnv) match {
      case ExternalCommandResult(0, stdOut, _, _, _) =>
        Success(stdOut)
      case ExternalCommandResult(1, stdOut, _, _, _) if IsWin && IncludeAutoDiscovery.gccAvailable() =>
        // the command to query the system header file locations within a Windows
        // environment always returns Success(1) for whatever reason...
        Success(stdOut)
      case other =>
        Failure(new RuntimeException(other.getOutputText))
    }
  }

}
