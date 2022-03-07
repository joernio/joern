package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.FileContentAtPath
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import org.slf4j.LoggerFactory

class ConfigPass(fileContentsAtPath: Iterable[FileContentAtPath], cpg: Cpg)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[String] =
    fileContentsAtPath.map { entry => entry.filename }.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    val contentsAtPath = fileContentsAtPath
      .filter { entry =>
        entry.filename == fileName
      }
      .toList
      .headOption
    contentsAtPath match {
      case Some(fm) =>
        val configNode = NewConfigFile().name(fm.relativizedPath).content(fm.content)
        diffGraph.addNode(configNode)
        logger.debug(s"Adding file `$fileName` as config.")
      case None =>
        logger.info(s"Could not find file at `$fileName`.")
    }
  }

}
