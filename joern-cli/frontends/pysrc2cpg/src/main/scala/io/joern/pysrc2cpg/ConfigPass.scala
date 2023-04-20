package io.joern.pysrc2cpg

import io.joern.pysrc2cpg.Py2Cpg.{InputPair, InputProvider}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class ConfigPass(cpg: Cpg, inputProviders: Iterable[InputProvider])
    extends ConcurrentWriterCpgPass[InputProvider](cpg) {

  private val logger                                 = LoggerFactory.getLogger(getClass)
  override def generateParts(): Array[InputProvider] = inputProviders.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, inputProvider: InputProvider): Unit = {
    val inputPair  = inputProvider()
    val configNode = NewConfigFile().name(inputPair.relFileName).content(inputPair.content)
    diffGraph.addNode(configNode)
    logger.debug(s"Added file '${inputPair.relFileName}' as config.")
  }

}
