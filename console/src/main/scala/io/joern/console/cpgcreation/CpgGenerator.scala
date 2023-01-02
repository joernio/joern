package io.joern.console.cpgcreation

import better.files.File
import io.shiftleft.codepropertygraph.Cpg

import scala.sys.process._

/** A CpgGenerator generates Code Property Graphs from code. Each supported language implements a Generator, e.g.,
  * [[JavaCpgGenerator]] implements Java Archive to CPG conversion, while [[CSharpCpgGenerator]] translates C# projects
  * into code property graphs.
  */
abstract class CpgGenerator() {

  def isWin: Boolean = scala.util.Properties.isWin

  def isAvailable: Boolean

  /** is this a JVM based frontend? if so, we'll invoke it with -Xmx for max heap settings */
  def isJvmBased: Boolean

  /** Generate a CPG for the given input path. Returns the output path, or None, if no CPG was generated.
    *
    * This method appends command line options in config.frontend.cmdLineParams to the shell command.
    */
  def generate(inputPath: String, outputPath: String = "cpg.bin.zip", namespaces: List[String] = List()): Option[String]

  protected def runShellCommand(program: String, arguments: Seq[String]): Option[String] = {
    if (!File(program).exists) {
      System.err.println(s"CPG generator does not exist at: $program")
      return None
    }
    val cmd       = Seq[String](program) ++ arguments
    val exitValue = cmd.run().exitValue()
    if (exitValue == 0) {
      Some(cmd.toString)
    } else {
      System.err.println(s"Error running shell command: $cmd")
      None
    }
  }

  def applyPostProcessingPasses(cpg: Cpg): Cpg = {
    cpg
  }

}
