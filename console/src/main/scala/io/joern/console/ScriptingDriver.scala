package io.joern.console

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.io.AbstractFile

import java.io.File

/**
 * Extends dotty.tools.scripting.ScriptingDriver to add support for predef, i.e. run some code before the main script.
 */
class ScriptingDriver(predefCode: String, compilerArgs: Array[String], scriptFile: File, scriptArgs: Array[String])
  extends dotty.tools.scripting.ScriptingDriver(compilerArgs, scriptFile, scriptArgs) {

  override protected def doCompile(compiler: Compiler, files: List[AbstractFile])(using Context): Reporter = {
    val run = compiler.newRun
    val predefCode0 =
      """
        |object WS1 {
        |  def bar(i: Int): String = "ws1 bar " + i
        |}
        |import WS1.bar
        |""".stripMargin
    run.compileFromStrings(List(predefCode0))
//    run.compileFromStrings(List(predefCode))
    finish(compiler, run)

    super.doCompile(compiler, files)
  }

}
