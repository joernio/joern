package io.joern.jssrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewMetaData
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

class JsMetaDataPass(cpg: Cpg, hash: String, inputPath: String) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.debug("Generating meta-data.")
    val absolutePathToRoot = File(inputPath).path.toAbsolutePath.toString
    val metaNode           = NewMetaData().language(Languages.JSSRC).root(absolutePathToRoot).hash(hash).version("0.1")
    diffGraph.addNode(metaNode)
  }

}
