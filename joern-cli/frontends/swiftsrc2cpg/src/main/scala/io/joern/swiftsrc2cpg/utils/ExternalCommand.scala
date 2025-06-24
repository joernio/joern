package io.joern.swiftsrc2cpg.utils

import io.shiftleft.semanticcpg.utils.ExternalCommandResult

import java.nio.file.Paths
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ExternalCommand {

  def run(command: Seq[String], cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    io.shiftleft.semanticcpg.utils.ExternalCommand
      .run(command, Option(Paths.get(cwd)), mergeStdErrInStdOut = true, extraEnv) match {
      case ExternalCommandResult(0, stdOut, _, _, _) =>
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, stdErr, _, _) if stdErr.isEmpty && stdOut.nonEmpty =>
        // SwiftAstGen exits with exit code != 0 on Windows.
        // To catch with we specifically handle the empty stdErr here.
        Success(stdOut)
      case ExternalCommandResult(_, stdOut, stdErr, _, _)
          if stdErr.isEmpty && stdOut.isEmpty && scala.util.Properties.isWin =>
        // SwiftAstGen exits with exit code != 0 on Windows
        // and empty stdOut and stdErr if the Swift runtime is not installed at all
        Failure(new RuntimeException("""
            | Unable to execute SwiftAstGen!
            | On Windows systems Swift needs to be installed.
            | Please see: https://www.swift.org/install/windows/
            |""".stripMargin))
      case other =>
        Failure(new RuntimeException(other.getOutputText))
    }
  }

}
