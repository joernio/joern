package io.shiftleft.semanticcpg.utils

import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.{Files, Path, Paths}
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
    val cmd     = createCommand(command, isShellCommand)
    val builder = new ProcessBuilder().command(cmd.toArray*)
    builder.environment().putAll(extraEnv.asJava)
    builder.redirectErrorStream(mergeStdErrInStdOut)
    workingDir.foreach(dir => builder.directory(dir.toFile))

    val stdOutFile = File.createTempFile("x2cpg", "stdout")
    val stdErrFile = Option.when(!mergeStdErrInStdOut)(File.createTempFile("x2cpg", "stderr"))
    builder.redirectOutput(stdOutFile)
    stdErrFile.foreach(builder.redirectError)

    val actualWorkingDir       = workingDir.getOrElse(Paths.get(".").toAbsolutePath.toString)
    val input                  = s"""cmd: `${cmd.mkString(" ")}`, workingDir: $actualWorkingDir, extraEnv: $extraEnv"""
    val additionalContextMaybe = Option(additionalContext).filter(_.nonEmpty)

    try {
      logger.debug(s"executing command: $input")
      val exitValue = execute(builder, timeout)
      logger.debug(s"command finished")
      val stdOut = IOUtils.readLinesInFile(stdOutFile.toPath)
      val stdErr = stdErrFile.map(_.toPath).map(IOUtils.readLinesInFile).getOrElse(Seq.empty)
      ExternalCommandResult(exitValue, stdOut, stdErr, input, additionalContextMaybe)
    } catch {
      case NonFatal(exception) =>
        val stdOut = IOUtils.readLinesInFile(stdOutFile.toPath)
        val stdErr = stdErrFile.map(f => IOUtils.readLinesInFile(f.toPath)).getOrElse(Seq.empty) :+ exception.getMessage
        val contextSuffix = additionalContextMaybe.map(ctx => s" ($ctx)").getOrElse("")
        logger.debug(s"command did not finish successfully. Input was: $input$contextSuffix")
        ExternalCommandResult(1, stdOut, stdErr, input, additionalContextMaybe)
    } finally {
      stdOutFile.delete()
      stdErrFile.foreach(_.delete())
    }
  }

  private def createCommand(command: Seq[String], isShellCommand: Boolean) = {
    if (isShellCommand) {
      val invokeShell =
        if (isWin) Seq("cmd.exe", "/C")
        else Seq("sh", "-c")
      invokeShell ++ command
    } else {
      command
    }
  }

  /** @return the exit value of the process */
  private def execute(processBuilder: ProcessBuilder, timeout: Duration): Int = {
    val process = processBuilder.start()
    if (timeout.isFinite) {
      logger.debug(s"waiting until command completes (with timeout=$timeout)")
      val finished = process.waitFor(timeout.toMillis, TimeUnit.MILLISECONDS)
      if (!finished) {
        logger.warn(s"timeout reached - will now kill the external command")
        process.destroy()
        val command = processBuilder.command().asScala.mkString(" ")
        throw new TimeoutException(s"command '$command' has timed out after $timeout")
      }
    } else {
      logger.debug("waiting until command completes (without a timeout)")
      process.waitFor()
    }
    process.exitValue()
  }

  /** Finds the absolute path to the executable directory (e.g. `/path/to/javasrc2cpg/bin`). Based on the package path
    * of a loaded classfile based on some (potentially flakey?) filename heuristics. Context: we want to be able to
    * invoke the x2cpg frontends from any directory, not just their install directory, and then invoke other
    * executables, like astgen, php-parser et al.
    */
  def executableDir(packagePath: Path): Path = {
    val packagePathAbsolute = packagePath.toAbsolutePath
    val fixedDir            =
      if (packagePathAbsolute.toString.contains("lib")) {
        var dir = packagePathAbsolute
        while (dir.toString.contains("lib"))
          dir = dir.getParent
        dir
      } else if (packagePathAbsolute.toString.contains("target")) {
        // SBT 2.x stores all subproject output under <root>/target/out/jvm/<scala>/<project>/.
        resolveSbt2ProjectDir(packagePathAbsolute).getOrElse(Paths.get("."))
      } else {
        resolveBloopProjectDir(packagePathAbsolute).getOrElse(Paths.get("."))
      }

    fixedDir.resolve("bin/").toAbsolutePath
  }

  private val BloopDirPattern = """"directory"\s*:\s*"([^"]+)"""".r

  /** SBT 2.x centralises all subproject output under `<root>/target/out/jvm/<scala>/<project>/`, where `<project>` is
    * the module's base directory name. We resolve the real subproject directory by checking the known module layouts
    * (top-level and `joern-cli/frontends/`), falling back to the Bloop config (auto-generated by sbt-bloop via
    * metals.sbt) for non-standard layouts.
    */
  private def resolveSbt2ProjectDir(packagePath: Path): Option[Path] = {
    try {
      val nameCount = packagePath.getNameCount
      val targetIdx = (0 until nameCount - 4).reverse.find { i =>
        packagePath.getName(i).toString == "target"
        && packagePath.getName(i + 1).toString == "out"
        && packagePath.getName(i + 2).toString == "jvm"
      }
      targetIdx.flatMap { i =>
        val workspaceRoot = packagePath.getRoot.resolve(packagePath.subpath(0, i))
        val projectName   = packagePath.getName(i + 4).toString
        val candidates    = Seq(
          workspaceRoot.resolve(projectName),
          workspaceRoot.resolve("joern-cli").resolve("frontends").resolve(projectName)
        )
        candidates.find(dir => Files.isDirectory(dir)).orElse {
          val configFile = workspaceRoot.resolve(".bloop").resolve(s"$projectName.json")
          if (Files.exists(configFile)) {
            val content = IOUtils.readLinesInFile(configFile).mkString("\n")
            BloopDirPattern.findFirstMatchIn(content).map(regexMatch => Paths.get(regexMatch.group(1)))
          } else None
        }
      }
    } catch {
      case NonFatal(e) =>
        logger.warn(s"Failed to resolve SBT 2.x project directory for $packagePath", e)
        None
    }
  }

  /** Resolves the actual project directory when running under Bloop. Bloop stores compiled classes under
    * `.bloop/<projectName>/...` at the workspace root, so we read the project's Bloop config JSON
    * (`.bloop/<projectName>.json`) to find the real project directory.
    */
  private def resolveBloopProjectDir(packagePath: Path): Option[Path] = {
    try {
      val bloopDir  = Path.of(".bloop")
      val nameCount = packagePath.getNameCount
      (0 until nameCount).find(i => packagePath.getName(i) == bloopDir) match {
        case Some(i) if i + 1 < nameCount =>
          val workspaceRoot = packagePath.getRoot.resolve(packagePath.subpath(0, i))
          val projectName   = packagePath.getName(i + 1).toString
          val configFile    = workspaceRoot.resolve(".bloop").resolve(s"$projectName.json")
          val content       = IOUtils.readLinesInFile(configFile).mkString("\n")
          BloopDirPattern.findFirstMatchIn(content).map(regexMatch => Paths.get(regexMatch.group(1)))
        case _ => None
      }
    } catch {
      case NonFatal(e) =>
        logger.warn(s"Failed to resolve bloop project directory for $packagePath", e)
        None
    }
  }

}
