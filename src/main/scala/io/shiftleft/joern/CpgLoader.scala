package io.shiftleft.joern

import io.shiftleft.cpgloading.CpgLoaderConfig
import io.shiftleft.queryprimitives.steps.starters.Cpg

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  * */
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * @param config loader configuration. If not specified, use default + default semantics
    * */
  def load(filename : String, config : CpgLoaderConfig = CpgLoaderConfig.default) : Cpg = {
    val config = CpgLoaderConfig.default
    if (config.semanticsFilename.isEmpty) {
      config.semanticsFilename = Some("src/main/resources/default.semantics")
    }
    io.shiftleft.cpgloading.CpgLoader.load(filename, config)
  }

}
