package io.joern.rubysrc2cpg.passes

import better.files.File
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory
import java.nio.file.Path

/** Creates the CONFIGURATION layer from any existing `Gemfile` or `Gemfile.lock` files found in `inputPath` at root
  * level.
  */
class ConfigPass(cpg: Cpg, inputPath: String) extends ConcurrentWriterCpgPass[Path](cpg) {
  private val logger = LoggerFactory.getLogger(this.getClass)

  override def generateParts(): Array[Path] = lookForGemfiles(inputPath)

  override def runOnPart(diffGraph: DiffGraphBuilder, gemfilePath: Path): Unit = {
    val configFileName     = gemfilePath.getFileName.toString
    val configFileContents = IOUtils.readEntireFile(gemfilePath)
    val configFileNode     = NewConfigFile().name(configFileName).content(configFileContents)
    diffGraph.addNode(configFileNode)
    logger.debug(s"Added file '$configFileName' as config.")
  }

  /** Returns the paths of any root-leveled `Gemfile` or `Gemfile.lock` file.
    */
  private def lookForGemfiles(inputPath: String): Array[Path] = {
    val rootPath = File(inputPath)
    val gemFiles = Set("Gemfile", "Gemfile.lock").map(rootPath / _)
    gemFiles.filter(_.exists).filter(_.isRegularFile).map(_.path).toArray
  }
}
