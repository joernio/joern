package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.{FileContentAtPath, Kt2Cpg, KtFileWithMeta}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.{DiffGraph, IntervalKeyPool, ParallelCpgPass}
import org.slf4j.LoggerFactory

class ConfigPass(
    fileContentsAtPath: Iterable[FileContentAtPath],
    cpg: Cpg,
    keyPool: IntervalKeyPool
) extends ParallelCpgPass[String](cpg, keyPools = Some(keyPool.split(fileContentsAtPath.size))) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def partIterator: Iterator[String] = {
    fileContentsAtPath.map { entry => entry.filename }.iterator
  }

  override def runOnPart(fileName: String): Iterator[DiffGraph] = {
    val contentsAtPath = fileContentsAtPath
      .filter { entry =>
        entry.filename == fileName
      }
      .toList
      .headOption
    contentsAtPath match {
      case Some(fm) =>
        val diffGraph = DiffGraph.newBuilder
        val configNode = NewConfigFile().name(fm.relativizedPath).content(fm.content)
        diffGraph.addNode(configNode)
        logger.debug(s"Adding file `$fileName` as config.")
        Iterator(diffGraph.build())
      case None =>
        logger.info(s"Could not find file at `$fileName`.")
        Iterator[DiffGraph]()
    }
  }

}
