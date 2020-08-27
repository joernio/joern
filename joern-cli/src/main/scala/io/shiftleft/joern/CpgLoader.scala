package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.cpgloading.CpgLoaderConfig

import overflowdb.OdbConfig

/**
  * Thin wrapper around `codepropertygraph`'s CpgLoader
  **/
object CpgLoader {

  /**
    * Load code property graph
    * @param filename name of the file that stores the cpg
    * @param storeFilename if unequal non-empty - location of ODB store
    * */
  def load(filename: String, storeFilename: String = ""): Cpg = {
    val config = if (storeFilename != "") {
      val odbConfig = OdbConfig.withDefaults().withStorageLocation(storeFilename)
      CpgLoaderConfig().withOverflowConfig(odbConfig)
    } else {
      CpgLoaderConfig()
    }
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.load(filename, config)
  }

  /**
    * Load code property graph from overflowDB
    * @param filename name of the file that stores the cpg
    * */
  def loadFromOdb(filename: String): Cpg = {
    val odbConfig = OdbConfig.withDefaults().withStorageLocation(filename)
    val config = CpgLoaderConfig().withOverflowConfig(odbConfig).doNotCreateIndexesOnLoad
    io.shiftleft.codepropertygraph.cpgloading.CpgLoader.loadFromOverflowDb(config)
  }

}
