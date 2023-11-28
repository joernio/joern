package io.joern.console

import io.shiftleft.codepropertygraph.cpgloading.{CpgLoader, CpgLoaderConfig}

object CpgConverter {

  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit = {
    val odbConfig = Config.withDefaults.withStorageLocation(dstFilename)
    val config    = CpgLoaderConfig.withDefaults.doNotCreateIndexesOnLoad.withOverflowConfig(odbConfig)
    CpgLoader.load(srcFilename).close
  }

}
