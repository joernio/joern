package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  * */
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * @param semanticsFilename file from which semantics are loaded. If not provided defaults are loaded.
    * */
  def load(filename: String, semanticsFilename: Option[String] = None): Cpg = {
    val config = CpgLoaderConfig.default
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

}
