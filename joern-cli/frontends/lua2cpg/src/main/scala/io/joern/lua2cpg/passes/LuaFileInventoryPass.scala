package io.joern.lua2cpg.passes

import io.joern.lua2cpg.Config
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.passes.CpgPass
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class LuaFileInventoryPass(cpg: Cpg, config: Config) extends CpgPass(cpg) {

  override def run(diffGraph: DiffGraphBuilder): Unit = {
    val inputRoot = Paths.get(config.inputPath).absolutePathAsString
    val luaFiles = SourceFiles.determine(
      inputPath = inputRoot,
      sourceFileExtensions = Set(".lua"),
      ignoredDefaultRegex = Some(config.defaultIgnoredFilesRegex),
      ignoredFilesRegex = Some(config.ignoredFilesRegex),
      ignoredFilesPath = Some(config.ignoredFiles)
    )()

    luaFiles.zipWithIndex.foreach { case (file, index) =>
      val relativeName = SourceFiles.toRelativePath(file, inputRoot)
      diffGraph.addNode(NewFile().name(relativeName).order(index + 1))
    }
  }
}
