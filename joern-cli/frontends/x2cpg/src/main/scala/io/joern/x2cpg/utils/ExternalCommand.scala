package io.joern.x2cpg.utils

import java.io.File
import java.nio.file.{Path, Paths}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*

object ExternalCommand {

  case class ExternalCommandResult(exitCode: Int, stdOut: Seq[String], stdErr: Seq[String]) {
    def successOption: Option[Seq[String]] = exitCode match {
      case 0 => Some(stdOut)
      case _ => None
    }
    def toTry: Try[Seq[String]] = exitCode match {
      case 0 => Success(stdOut)
      case _ => Failure(new RuntimeException((stdOut ++ stdErr).mkString(System.lineSeparator())))
    }
  }

  def run(
    command: Seq[String],
    cwd: String,
    mergeStdErrInStdOut: Boolean = false,
    extraEnv: Map[String, String] = Map.empty
  ): ExternalCommandResult = {
    val builder = new ProcessBuilder()
    builder.command(command.toList.asJava)
    builder.environment().putAll(extraEnv.asJava)
    builder.directory(new File(cwd))
    builder.redirectErrorStream(mergeStdErrInStdOut)
    try {
      val process     = builder.start()
      val returnValue = process.waitFor()
      val outputBytes = process.getInputStream.readAllBytes()
      val errBytes    = process.getErrorStream.readAllBytes()
      val result =
        ExternalCommandResult(
          returnValue,
          new String(outputBytes).lines().iterator().asScala.toSeq,
          new String(errBytes).lines().iterator().asScala.toSeq
        )
      process.getInputStream.close()
      process.getOutputStream.close()
      process.getErrorStream.close()
      result
    } catch {
      case ex: Throwable => ExternalCommandResult(1, Seq.empty, stdErr = Seq(ex.getMessage))
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
