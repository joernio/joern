package io.joern.x2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg

import java.io.File

/** LanguageFrontend encapsulates the logic that translates the source code directory into CPGs
  */
abstract class LanguageFrontend {

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
}
