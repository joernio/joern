package io.joern.console

import io.shiftleft.codepropertygraph.cpgloading.CpgLoader

object CpgConverter {

  def convertProtoCpgToFlatgraph(srcFilename: String, dstFilename: String): Unit = {
    // TODO reimplement - in cpg build first
    //    val odbConfig = Config.withDefaults.withStorageLocation(dstFilename)
    //    val config    = CpgLoaderConfig.withDefaults.doNotCreateIndexesOnLoad.withOverflowConfig(odbConfig)
    //    CpgLoader.load(srcFilename).close
    ???
  }

  @deprecated("method got renamed to `convertProtoCpgToFlatgraph, please use that instead", "joern v3")
  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit =
    convertProtoCpgToFlatgraph(srcFilename, dstFilename)

}
