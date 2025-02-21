package io.joern.console.cpgcreation

import better.files.File
import io.shiftleft.semanticcpg.utils.ExternalCommand
import io.shiftleft.codepropertygraph.generated.Cpg
import scala.util.Try

/** A CpgGenerator generates Code Property Graphs from code. Each supported language implements a Generator, e.g.,
  * [[JavaCpgGenerator]] implements Java Archive to CPG conversion, while [[CSharpCpgGenerator]] translates C# projects
  * into code property graphs.
  */
abstract class CpgGenerator() {

  def isWin: Boolean = scala.util.Properties.isWin

  def isAvailable: Boolean

  /** is this a JVM based frontend? if so, we'll invoke it with -Xmx for max heap settings */
  def isJvmBased: Boolean

  /** Generate a CPG for the given input path. Returns the output path, or a Failure, if no CPG was generated.
    *
    * This method appends command line options in config.frontend.cmdLineParams to the shell command.
    */
  def generate(inputPath: String, outputPath: String = "cpg.bin.zip"): Try[String]

  protected def runShellCommand(program: String, arguments: Seq[String]): Try[Unit] =
    Try {
      assert(File(program).exists, s"CPG generator does not exist at: $program")

      val cmd       = Seq(program) ++ maxMemoryParameter ++ arguments
      val cmdString = cmd.mkString(" ")

      println(
        s"""=======================================================================================================
           |Invoking CPG generator in a separate process. Note that the new process will consume additional memory.
           |If you are importing a large codebase (and/or running into memory issues), please try the following:
           |1) exit joern
           |2) invoke the frontend: $cmdString
           |3) start joern, import the cpg: `importCpg("path/to/cpg")`
           |=======================================================================================================
           |""".stripMargin
      )

      val exitValue = ExternalCommand.run(cmd).exitCode
      assert(exitValue == 0, s"Error running shell command: exitValue=$exitValue; $cmd")
    }

  protected lazy val maxMemoryParameter = {
    if (isJvmBased) {
      val maxValueInMegabytes = Runtime.getRuntime.maxMemory / 1024 / 1024
      Seq(s"-J-Xmx${maxValueInMegabytes}m")
    } else Nil
  }

  /** override in specific cpg generators to make them apply post processing passes */
  def applyPostProcessingPasses(cpg: Cpg): Cpg =
    cpg

}
