package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.Kt2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory

class ConfigPass(
    inputProviders: Iterable[Kt2Cpg.InputProvider],
    cpg: Cpg,
    keyPool: IntervalKeyPool
) extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(inputProviders.size))) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def partIterator: Iterator[String] = {
    inputProviders.map { ip => ip().fileName }.iterator
  }

  override def runOnPart(fileName: String): Iterator[DiffGraph] = {
    val diffGraph = DiffGraph.newBuilder

    val fileContents = {
      val ip = inputProviders.filter { ip => ip().fileName == fileName }.toList
      if (ip.size == 1) {
        ip.head().content
      } else {
        // TODO: maybe use a None here and check later for issues
        ""
      }
    }
    // TODO: check for UTF-8 so that things don't break like for js
    logger.debug(s"Adding file '$fileName' as config.")
    val configNode = NewConfigFile().name(fileName).content(fileContents)
    diffGraph.addNode(configNode)
    Iterator(diffGraph.build())
  }

}
