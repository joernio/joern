package io.joern.x2cpg.utils

import io.shiftleft.utils.IOUtils

import java.io.File
import java.nio.file.{Path, Paths}
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try}
import scala.util.control.NonFatal

object ExternalCommand {

  case class ExternalCommandResult(exitCode: Int, stdOut: Seq[String], stdErr: Seq[String]) {
    def successOption: Option[Seq[String]] = exitCode match {
      case 0 => Some(stdOut)
      case _ => None
    }
    def toTry: Try[Seq[String]] = exitCode match {
      case 0 => Success(stdOut)
      case nonZeroExitCode =>
        val allOutput = stdOut ++ stdErr
        val message = s"""Process exited with code $nonZeroExitCode. Output:
           |${allOutput.mkString(System.lineSeparator())}
           |""".stripMargin
        Failure(new RuntimeException(message))
    }
  }

  def run(
    command: Seq[String],
    cwd: String,
    mergeStdErrInStdOut: Boolean = false,
    extraEnv: Map[String, String] = Map.empty
  ): ExternalCommandResult = {
    val builder = new ProcessBuilder()
      .command(command.toArray*)
      .directory(new File(cwd))
      .redirectErrorStream(mergeStdErrInStdOut)
    builder.environment().putAll(extraEnv.asJava)

    val stdOutFile = File.createTempFile("x2cpg", "stdout")
    val stdErrFile = Option.when(!mergeStdErrInStdOut)(File.createTempFile("x2cpg", "stderr"))

    try {
      builder.redirectOutput(stdOutFile)
      stdErrFile.foreach(f => builder.redirectError(f))

      val process     = builder.start()
      val returnValue = process.waitFor()

      val stdOut = IOUtils.readLinesInFile(stdOutFile.toPath)
      val stdErr = stdErrFile.map(f => IOUtils.readLinesInFile(f.toPath)).getOrElse(Seq.empty)
      ExternalCommandResult(returnValue, stdOut, stdErr)
    } catch {
      case NonFatal(exception) =>
        ExternalCommandResult(1, Seq.empty, stdErr = Seq(exception.getMessage))
    } finally {
      stdOutFile.delete()
      stdErrFile.foreach(_.delete())
    }
  }

  /** Finds the absolute path to the executable directory (e.g. `/path/to/javasrc2cpg/bin`). Based on the package path
    * of a loaded classfile based on some (potentially flakey?) filename heuristics. Context: we want to be able to
    * invoke the x2cpg frontends from any directory, not just their install directory, and then invoke other
    * executables, like astgen, php-parser et al.
    */
  def executableDir(packagePath: Path): Path = {
    val packagePathAbsolute = packagePath.toAbsolutePath
    val fixedDir =
      if (packagePathAbsolute.toString.contains("lib")) {
        var dir = packagePathAbsolute
        while (dir.toString.contains("lib"))
          dir = dir.getParent
        dir
      } else if (packagePathAbsolute.toString.contains("target")) {
        var dir = packagePathAbsolute
        while (dir.toString.contains("target"))
          dir = dir.getParent
        dir
      } else {
        Paths.get(".")
      }

    fixedDir.resolve("bin/").toAbsolutePath
  }
}
