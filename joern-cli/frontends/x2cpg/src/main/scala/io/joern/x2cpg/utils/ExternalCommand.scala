package io.joern.x2cpg.utils

import org.slf4j.LoggerFactory

import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.nio.file.Path
import java.nio.file.Paths
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal
import scala.util.Failure
import scala.util.Success
import scala.util.Try

object ExternalCommand {

  private val logger = LoggerFactory.getLogger(ExternalCommand.getClass)

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

    val stdOut = scala.collection.mutable.ArrayBuffer.empty[String]
    val stdErr = scala.collection.mutable.ArrayBuffer.empty[String]

    try {
      val process = builder.start()

      val outputReaderThread = new Thread(() => {
        val outputReader = new BufferedReader(new InputStreamReader(process.getInputStream))
        outputReader.lines.iterator.forEachRemaining(stdOut.addOne)
      })

      val errorReaderThread = new Thread(() => {
        val errorReader = new BufferedReader(new InputStreamReader(process.getErrorStream))
        errorReader.lines.iterator.forEachRemaining(stdErr.addOne)
      })

      outputReaderThread.start()
      errorReaderThread.start()

      val returnValue = process.waitFor()
      outputReaderThread.join()
      errorReaderThread.join()

      process.getInputStream.close()
      process.getOutputStream.close()
      process.getErrorStream.close()
      process.destroy()

      if (stdErr.nonEmpty) logger.warn(s"subprocess stderr: ${stdErr.mkString(System.lineSeparator())}")
      ExternalCommandResult(returnValue, stdOut.toSeq, stdErr.toSeq)
    } catch {
      case NonFatal(exception) =>
        ExternalCommandResult(1, Seq.empty, stdErr = Seq(exception.getMessage))
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
