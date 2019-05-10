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
    * */
  def load(filename: String): Cpg = {
    val config = CpgLoaderConfig.default
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

}
