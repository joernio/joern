package io.joern.x2cpg.utils

import org.slf4j.LoggerFactory

import java.io.File
import java.nio.file.{Path, Paths}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.sys.process.{Process, ProcessLogger}
import scala.util.{Failure, Success, Try}
import scala.jdk.CollectionConverters.*
import System.lineSeparator

trait ExternalCommand {

  private val logger = LoggerFactory.getLogger(this.getClass)

  protected val IsWin: Boolean = scala.util.Properties.isWin

  // do not prepend any shell layer by default
  // individual frontends may override this
  protected val shellPrefix: Seq[String] = Nil

  protected def handleRunResult(result: Try[Int], stdOut: Seq[String], stdErr: Seq[String]): Try[Seq[String]] = {
    if (stdErr.nonEmpty) logger.warn(s"subprocess stderr: ${stdErr.mkString(lineSeparator)}")

    result match {
      case Success(0)     => Success(stdOut)
      case Failure(error) => Failure(error)
      case Success(nonZeroExitCode) =>
        val allOutput = stdOut ++ stdErr
        val message =
          s"""Process exited with code $nonZeroExitCode. Output:
             |${allOutput.mkString(lineSeparator)}
             |""".stripMargin
        Failure(new RuntimeException(message))
    }
  }

  def run(command: String, cwd: String, extraEnv: Map[String, String] = Map.empty): Try[Seq[String]] = {
    val stdOutOutput  = new ConcurrentLinkedQueue[String]
    val stdErrOutput  = new ConcurrentLinkedQueue[String]
    val processLogger = ProcessLogger(stdOutOutput.add, stdErrOutput.add)
    val process = shellPrefix match {
      case Nil => Process(command, new java.io.File(cwd), extraEnv.toList*)
      case _   => Process(shellPrefix :+ command, new java.io.File(cwd), extraEnv.toList*)
    }
    handleRunResult(Try(process.!(processLogger)), stdOutOutput.asScala.toSeq, stdErrOutput.asScala.toSeq)
  }

  // We use the java ProcessBuilder API instead of the Scala version because it
  // offers the possibility to merge stdout and stderr into one stream of output.
  // Maybe the Scala version also offers this but since there is no documentation
  // I was not able to figure it out.
  def runWithMergeStdoutAndStderr(command: String, cwd: String): (Int, String) = {
    val builder = new ProcessBuilder()
    builder.command(command.split(' ')*)
    builder.directory(new File(cwd))
    builder.redirectErrorStream(true)

    val process     = builder.start()
    val outputBytes = process.getInputStream.readAllBytes()
    val returnValue = process.waitFor()

    (returnValue, new String(outputBytes))
  }
}

object ExternalCommand extends ExternalCommand {

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
