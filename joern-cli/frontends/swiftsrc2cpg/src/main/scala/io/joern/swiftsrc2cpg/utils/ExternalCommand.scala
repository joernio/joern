package io.joern.swiftsrc2cpg.utils

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ExternalCommand {

  import io.shiftleft.semanticcpg.utils.ExternalCommand.ExternalCommandResult

  def run(command: Seq[String], cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    io.shiftleft.semanticcpg.utils.ExternalCommand
      .run(command, Option(cwd), mergeStdErrInStdOut = true, extraEnv) match {
      case ExternalCommandResult(0, stdOut, _, _) =>
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, stdErr, _) if stdErr.isEmpty && stdOut.nonEmpty =>
        // SwiftAstGen exits with exit code != 0 on Windows.
        // To catch with we specifically handle the empty stdErr here.
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, _, _) =>
        Failure(new RuntimeException(stdOut.mkString(System.lineSeparator())))
    }
  }

}
