package io.joern.swiftsrc2cpg.utils

import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ExternalCommand {

  import io.joern.x2cpg.utils.ExternalCommand.ExternalCommandResult

  def run(command: Seq[String], cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    io.joern.x2cpg.utils.ExternalCommand.run(command, cwd, mergeStdErrInStdOut = true, extraEnv) match {
      case ExternalCommandResult(0, stdOut, _) =>
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, stdErr) if stdErr.isEmpty && stdOut.nonEmpty =>
        // SwiftAstGen exits with exit code != 0 on Windows.
        // To catch with we specifically handle the empty stdErr here.
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, _) =>
        Failure(new RuntimeException(stdOut.mkString(System.lineSeparator())))
    }
  }

}
