package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewMetaData
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.SimpleCpgPass
import org.slf4j.LoggerFactory

class JsMetaDataPass(cpg: Cpg) extends SimpleCpgPass(cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    logger.debug("Generating meta-data.")
    // TODO: re-add hash here:
    val metaNode = NewMetaData().language(Languages.JSSRC).version("0.1")
    diffGraph.addNode(metaNode)
  }

}
