package io.shiftleft.console

import io.shiftleft.codepropertygraph.cpgloading.{CpgLoader, CpgLoaderConfig}
import overflowdb.Config

object CpgConverter {

  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit = {
    val odbConfig = Config.withDefaults.withStorageLocation(dstFilename)
    val config = CpgLoaderConfig.withDefaults.doNotCreateIndexesOnLoad.withOverflowConfig(odbConfig)
    CpgLoader.load(srcFilename, config).close
  }

}
