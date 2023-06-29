package io.joern.rubysrc2cpg.passes

import better.files.File
import io.joern.x2cpg.passes.frontend.XConfigFileCreationPass
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewConfigFile
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.utils.IOUtils
import org.slf4j.LoggerFactory

import java.nio.file.Path

/** Creates the CONFIGURATION layer from any existing `Gemfile` or `Gemfile.lock` files found in `inputPath` at root
  * level.
  */
class ConfigFileCreationPass(cpg: Cpg, inputPath: String) extends XConfigFileCreationPass(cpg) {
  
  private val logger = LoggerFactory.getLogger(this.getClass)

  override protected val configFileFilters: List[File => Boolean] = List(
    isRootLevelGemfile
  )
  
  private def isRootLevelGemfile(file: File): Boolean = {
    val rootPath = File(inputPath)
    val acceptableGemfilePaths = Set("Gemfile", "Gemfile.lock").map(rootPath / _)
    acceptableGemfilePaths.contains(file)
  }
  
}
