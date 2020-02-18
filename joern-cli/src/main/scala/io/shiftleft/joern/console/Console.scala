package io.shiftleft.joern.console

import better.files.File

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.joern.CpgLoader
import io.shiftleft.joern.scripting.JoernScriptManager

object Console extends JoernScriptManager {

  def banner(): Unit = {
    println("""
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
      """.stripMargin)
  }

  private var _cpg: Cpg = null

  /**
    * Return the CPG that was last loaded
    * */
  def cpg: Cpg = _cpg match {
    case null       => throw new RuntimeException("No CPG loaded. Use `loadCpg(filename)` first")
    case value: Cpg => value
  }

  /**
    * Load CPG stored at `inputPath`.
    * */
  def loadCpg(inputPath: String): Option[Cpg] = {
    if (!File(inputPath).exists) {
      report(s"File does not exist: $inputPath")
      return None
    }
    _cpg = CpgLoader.loadFromOdb(inputPath)
    Some(cpg)
  }

  /**
    * (Re)-apply semantics stored in `semanticsFilenameOpt`.
    * If `semanticsFilenameOpt` is None default semantics
    * are applied.
    * */
  def applySemantics(semanticsFilenameOpt: Option[String]): Unit =
    CpgLoader.applySemantics(cpg, semanticsFilenameOpt)

  private def report(msg: String): Unit = {
    System.err.println(msg)
  }

}
