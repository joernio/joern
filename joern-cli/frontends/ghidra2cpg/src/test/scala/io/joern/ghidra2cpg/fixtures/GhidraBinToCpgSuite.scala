package io.joern.ghidra2cpg.fixtures

import io.joern.ghidra2cpg.{Config, Ghidra2Cpg}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.utils.ProjectRoot
import org.apache.commons.io.FileUtils
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

import java.nio.file.Files

class GhidraFrontend extends LanguageFrontend {
  override type ConfigType = Config
  override val fileSuffix: String = ""

  override def execute(inputFile: java.io.File): Cpg = {
    val dir = Files.createTempDirectory("ghidra2cpg-tests").toFile
    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      override def run(): Unit = FileUtils.deleteQuietly(dir)
    }))

    val tempDir = Files.createTempDirectory("ghidra2cpg").toFile
    Runtime.getRuntime.addShutdownHook(new Thread(new Runnable {
      override def run(): Unit = FileUtils.deleteQuietly(tempDir)
    }))

    val cpgBin = dir.getAbsolutePath
    val config = Config().withInputPath(inputFile.getAbsolutePath).withOutputPath(cpgBin)
    new Ghidra2Cpg().createCpg(config).get
  }

}

class GhidraBinToCpgSuite extends BinToCpgFixture(new GhidraFrontend) {
  override val binDirectory = ProjectRoot.relativise("joern-cli/frontends/ghidra2cpg/src/test/testbinaries/")

  protected def flowToResultPairs(path: Path): List[String] = path.resultPairs().map(_._1)
}
