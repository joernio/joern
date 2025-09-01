package io.joern.swiftsrc2cpg.utils

import io.shiftleft.semanticcpg.utils.ExternalCommandResult

import java.io.{BufferedReader, InputStream, InputStreamReader}
import java.nio.file.Paths
import scala.util.{Failure, Success, Try, Using}

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
        Failure(new RuntimeException(other.stdOutAndError.mkString(System.lineSeparator())))

    }
  }

  /** Executes a command and returns the first stdout line that matches a predicate.
    *
    * The command is launched via `ProcessBuilder`, with stderr discarded to avoid mixing with stdout. Stdout is read
    * line by line; the first line for which `find` returns `true` is captured. Resources are closed via
    * `scala.util.Using`.
    *
    * We do not use [[io.shiftleft.semanticcpg.utils.ExternalCommand]] deliberately here as this writes std out and std
    * err to a temporary file. Reading the process’s stdout directly through pipes with a BufferedReader is faster,
    * lower‑latency, and uses fewer resources than redirecting to a temporary file and then reading the file like
    * ExternalCommand does (for small, short-living external process invocations).
    *
    * Note:
    *   - A result is returned only if the process exits successfully \(`exit code == 0`\). If the process fails, `None`
    *     is returned even if a matching line was observed.
    *   - Stderr is ignored \(`Redirect.DISCARD`\).
    *
    * @param command
    *   The command to execute, tokenized as a sequence of arguments.
    * @param find
    *   Predicate applied to each stdout line; the first matching line is selected. If left out the first line is
    *   selected.
    * @return
    *   `Some(line)` if a matching stdout line is found and the process exits with code 0; otherwise `None`.
    */
  def findInStdOut(command: Seq[String], find: String => Boolean = _ => true): Option[String] = {
    val pb = new ProcessBuilder(command*)
    pb.redirectError(ProcessBuilder.Redirect.DISCARD)
    val process = pb.start()

    var result = Option.empty[String]
    Using.Manager { use =>
      val input  = use(new InputStreamReader(process.getInputStream))
      val reader = use(new BufferedReader(input))
      result = Iterator
        .continually(reader.readLine())
        .takeWhile(_ != null)
        .find(find)
    }

    if (process.waitFor() == 0) result else Option.empty
  }

  /** Executes a command and returns its output as an InputStream.
    *
    * This method builds and starts a process with the given command, configuring it to merge stderr into stdout and
    * setting the working directory.
    *
    * @param invocationCommand
    *   The command to execute
    * @param workingDir
    *   The directory to execute the command in
    * @return
    *   An InputStream containing the output of the Swift compiler process
    */
  def inputStreamFromCommand(command: Seq[String], workingDir: String): InputStream = {
    val builder = new ProcessBuilder(command*)
    builder.directory(Paths.get(workingDir).toFile)
    builder.redirectErrorStream(true)
    builder.start().getInputStream
  }

}
