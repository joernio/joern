package io.joern.ghidra2cpg.fixtures

import io.joern.ghidra2cpg.{Config, Ghidra2Cpg}
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.utils.ProjectRoot
import org.apache.commons.io.FileUtils
import io.shiftleft.codepropertygraph.generated.nodes
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

import java.nio.file.Files

class GhidraFrontend extends LanguageFrontend {
  override val fileSuffix: String = ""

  override def execute(inputFile: java.io.File): Cpg = {
    val dir = Files.createTempDirectory("ghidra2cpg-tests").toFile
    Runtime.getRuntime.addShutdownHook(new Thread(() => FileUtils.deleteQuietly(dir)))

    val tempDir = Files.createTempDirectory("ghidra2cpg").toFile
    Runtime.getRuntime.addShutdownHook(new Thread(() => FileUtils.deleteQuietly(tempDir)))

    val cpgBin                         = dir.getAbsolutePath
    implicit val defaultConfig: Config = Config()
    new Ghidra2Cpg().createCpg(inputFile.getAbsolutePath, Some(cpgBin)).get
  }

}

class GhidraBinToCpgSuite extends BinToCpgFixture(new GhidraFrontend) {
  override val binDirectory = ProjectRoot.relativise("joern-cli/frontends/ghidra2cpg/src/test/testbinaries/")

  def flowToResultPairs(path: Path): List[String] = {
    val pairs = path.elements.map {
      case point: nodes.MethodParameterIn => {
        val method      = point.method.head
        val method_name = method.name
        val code        = s"$method_name(${method.parameter.l.sortBy(_.order).map(_.code).mkString(", ")})"
        code
      }
      case point => point.statement.repr
    }
    pairs.headOption
      .map(x => x :: pairs.sliding(2).collect { case Seq(a, b) if a != b => b }.toList)
      .getOrElse(List())
  }
}
