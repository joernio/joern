package io.shiftleft.joern.console

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.joern.CpgLoader

object Console {

  def banner() : Unit = {
    println(
      """
        |     ██╗ ██████╗ ███████╗██████╗ ███╗   ██╗
        |     ██║██╔═══██╗██╔════╝██╔══██╗████╗  ██║
        |     ██║██║   ██║█████╗  ██████╔╝██╔██╗ ██║
        |██   ██║██║   ██║██╔══╝  ██╔══██╗██║╚██╗██║
        |╚█████╔╝╚██████╔╝███████╗██║  ██║██║ ╚████║
        | ╚════╝  ╚═════╝ ╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝
      """.stripMargin)
  }

  private var _cpg : Cpg = null

  /**
    * Return the CPG that was last loaded
    * */
  def cpg : Cpg = _cpg match {
    case null => throw new RuntimeException("No CPG loaded. Use `loadCpg(filename)` first")
    case value : Cpg => value
  }

  /**
    * Load CPG stored at `inputPath` and apply data flow semantics from `semanticsFilenameOpt`.
    * If `semanticsFilename` is not given or `None`, default semantics will be applied.
    * */
  def loadCpg(inputPath : String) : Option[Cpg] = {
    if(!File(inputPath).exists) {
      report(s"File does not exist: $inputPath")
      return None
    }
    _cpg = CpgLoader.loadWithoutSemantics(inputPath)
    Some(cpg)
  }

  def applySemantics(semanticsFilenameOpt : Option[String]) : Unit =
    CpgLoader.applySemantics(cpg, semanticsFilenameOpt)

  private def report(msg : String): Unit = {
    System.err.println(msg)
  }


}
