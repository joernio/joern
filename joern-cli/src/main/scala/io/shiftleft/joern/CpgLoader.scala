package io.shiftleft.joern

import io.shiftleft.SerializedCpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig
import io.shiftleft.layers.DataFlowRunner
import io.shiftleft.semanticsloader.SemanticsLoader

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  * */
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * */
  def load(filename: String, semanticsFilenameOpt: Option[String] = None): Cpg = {
    val config = CpgLoaderConfig.default
    val cpg = io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
    val semanticsFilename = semanticsFilenameOpt.getOrElse("joern-cli/src/main/resources/default.semantics")
    val semantics = new SemanticsLoader(semanticsFilename).load
    new DataFlowRunner(semantics).run(cpg, new SerializedCpg())
    cpg
  }

}
