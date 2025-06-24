package io.shiftleft.semanticcpg.utils

import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.{Path, Paths}
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*
import scala.language.implicitConversions
import scala.util.control.NonFatal
import scala.util.Properties.isWin

object ExternalCommand {
  protected[utils] val logger = LoggerFactory.getLogger(getClass)

  def run(
    command: Seq[String],
    workingDir: Option[Path] = None,
    mergeStdErrInStdOut: Boolean = false,
    extraEnv: Map[String, String] = Map.empty,
    isShellCommand: Boolean = false,
    timeout: Duration = Duration.Inf,
    additionalContext: String = ""
  ): ExternalCommandResult = {
    val cmd =
      if (isShellCommand) {
        val invokeShell =
          if (isWin) Seq("cmd.exe", "/C")
          else Seq("sh", "-c")
        invokeShell ++ command
      } else {
        command
      }

    val builder = new ProcessBuilder().command(cmd.toArray*)
    builder.environment().putAll(extraEnv.asJava)
    builder.redirectErrorStream(mergeStdErrInStdOut)
    workingDir.foreach(dir => builder.directory(dir.toFile))

    val stdOutFile = File.createTempFile("x2cpg", "stdout")
    val stdErrFile = Option.when(!mergeStdErrInStdOut)(File.createTempFile("x2cpg", "stderr"))
    builder.redirectOutput(stdOutFile)
    stdErrFile.foreach(builder.redirectError)

    val actualWorkingDir = workingDir.getOrElse(Paths.get(".").toAbsolutePath.toString)
    val input = s"""cmd: `${cmd.mkString(" ")}`, workingDir: $actualWorkingDir, extraEnv: $extraEnv"""
    val additionalContextMaybe = Option(additionalContext).filter(_.nonEmpty)
    try {
      logger.debug(s"executing command: $input")
      val process = builder.start()
      if (timeout.isFinite) {
        logger.debug(s"waiting until command completes (but max $timeout)")
        val finished = process.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS)
        if (!finished) {
          logger.warn(s"timeout reached - will now kill the external command")
          process.destroy()
          throw new TimeoutException(s"command '${command.mkString(" ")}' with timeout='$timeout' has timed out")
        }
      } else {
        logger.debug("waiting until command completes (without a timeout)")
        process.waitFor()
      }

      logger.debug(s"command finished successfully")
      val stdOut = IOUtils.readLinesInFile(stdOutFile.toPath)
      val stdErr = stdErrFile.map(_.toPath).map(IOUtils.readLinesInFile).getOrElse(Seq.empty)
      ExternalCommandResult(process.exitValue(), stdOut, stdErr, input, additionalContextMaybe)
    } catch {
      case NonFatal(exception) =>
        val stdOut = IOUtils.readLinesInFile(stdOutFile.toPath)
        val stdErr = stdErrFile.map(f => IOUtils.readLinesInFile(f.toPath)).getOrElse(Seq.empty) :+ exception.getMessage
        logger.debug(s"command did not finish successfully. Input was: $input")
        ExternalCommandResult(1, stdOut, stdErr, input, additionalContextMaybe)
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
