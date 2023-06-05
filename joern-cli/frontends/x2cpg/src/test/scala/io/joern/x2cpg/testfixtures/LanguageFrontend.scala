package io.joern.x2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg

import java.io.File
import io.joern.x2cpg.X2CpgConfig
import scala.annotation.nowarn

/** LanguageFrontend encapsulates the logic that translates the source code directory into CPGs
  */
trait LanguageFrontend {

  /** A standard file extension for the source code files of the given language. E.g. `.c` for C language
    */
  val fileSuffix: String

  /** Generate CPG for the given source code directory
    * @param sourceCodeFile
    *   directory where source code is located
    * @return
    *   CPG representation stored in a file
    */
  def execute(sourceCodeFile: File): Cpg

  /** Generate CPG for the given source directory with the option to override the method to pass config down.
    * @param sourceCodeFile
    *   direcotry where source code is located
    * @param config
    *   frontend config: ignored unless overridden
    * @return
    *   CPG representation stored in a file
    */
  def execute[T <: X2CpgConfig[_]](sourceCodeFile: File, @nowarn config: T): Cpg = {
    execute(sourceCodeFile)
  }
}
