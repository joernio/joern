package io.joern.kotlin2cpg.passes

import io.joern.kotlin2cpg.FileContentAtPath
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ForkJoinParallelCpgPass
import org.slf4j.LoggerFactory

class ConfigPass(fileContentsAtPath: Iterable[FileContentAtPath], cpg: Cpg)
    extends ForkJoinParallelCpgPass[FileContentAtPath](cpg) {

  private val logger = LoggerFactory.getLogger(getClass)

  override def generateParts(): Array[FileContentAtPath] = fileContentsAtPath.toArray

  override def runOnPart(diffGraph: DiffGraphBuilder, fileContent: FileContentAtPath): Unit = {
    logger.debug(s"Adding file `${fileContent.filename}` as config.")
    val configNode = NewConfigFile().name(fileContent.relativizedPath).content(fileContent.content)
    diffGraph.addNode(configNode)
  }

}
