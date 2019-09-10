package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.dataflowengine.layers.dataflows.DataFlowRunner
import io.shiftleft.dataflowengine.semanticsloader.SemanticsLoader

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  * */
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * */
  def load(filename: String, semanticsFilenameOpt: Option[String] = None): Cpg = {
    val cpg = loadWithoutSemantics(filename)
    applySemantics(cpg, semanticsFilenameOpt)
    cpg
  }

  /**
    * Apply data flow semantics from `semanticsFilenameOpt` to `cpg`. If `semanticsFilenameOpt`
    * is omitted or None, default semantics will be applied.
    * */
  def applySemantics(cpg : Cpg, semanticsFilenameOpt : Option[String] = None): Unit = {
    val semanticsFilename = semanticsFilenameOpt.getOrElse("joern-cli/src/main/resources/default.semantics")
    val semantics = new SemanticsLoader(semanticsFilename).load
    new DataFlowRunner(semantics).run(cpg, new SerializedCpg())
  }

  /**
    * Load code property graph but do not apply semantics
    * @param filename name of the file that stores the cpg
    * */
  def loadWithoutSemantics(filename: String): Cpg = {
    val config = CpgLoaderConfig()
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

}
