package io.joern.console

import io.shiftleft.codepropertygraph.cpgloading.{CpgLoader, ProtoCpgLoader}

import java.nio.file.Paths

object CpgConverter {

  def convertProtoCpgToFlatgraph(srcFilename: String, dstFilename: String): Unit = {
    val cpg = ProtoCpgLoader.loadFromProtoZip(srcFilename, Option(Paths.get(dstFilename)))
    cpg.close()
  }

  @deprecated("method got renamed to `convertProtoCpgToFlatgraph, please use that instead", "joern v3")
  def convertProtoCpgToOverflowDb(srcFilename: String, dstFilename: String): Unit =
    convertProtoCpgToFlatgraph(srcFilename, dstFilename)

}
