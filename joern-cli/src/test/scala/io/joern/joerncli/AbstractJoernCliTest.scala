package io.joern.joerncli

import io.joern.console.FrontendConfig
import io.joern.console.cpgcreation.{CCpgGenerator, JsSrcCpgGenerator}
import io.joern.x2cpg.frontendspecific.jssrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.utils.FileUtil
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Path
import scala.util.Failure

trait AbstractJoernCliTest {

  protected def withTestCpg[T](file: Path, language: String = Languages.C)(f: ((Cpg, String)) => T): T = {
    f(loadTestCpg(file, language))
  }

  private def loadTestCpg(file: Path, language: String = Languages.C): (Cpg, String) = {
    val tmpFile        = FileUtil.newTemporaryFile("cpg", "bin")
    val cpgOutFileName = tmpFile.toString
    FileUtil.delete(tmpFile)

    val cpgGenerator = language match {
      case Languages.C | Languages.CSHARP         => CCpgGenerator(new FrontendConfig(), relativePath("c2cpg"))
      case Languages.JSSRC | Languages.JAVASCRIPT => JsSrcCpgGenerator(new FrontendConfig(), relativePath("jssrc2cpg"))
    }
    cpgGenerator.generate(inputPath = file.toString, outputPath = cpgOutFileName) match {
      case Failure(exception) => throw new AssertionError("error while invoking cpgGenerator", exception)
      case _                  =>
    }

    // Link CPG fragments and enhance to create semantic CPG
    val cpg = DefaultOverlays.create(cpgOutFileName)
    language match {
      case Languages.JSSRC | Languages.JAVASCRIPT =>
        jssrc2cpg.postProcessingPasses(cpg, XTypeRecoveryConfig(enabledDummyTypes = false)).foreach(_.createAndApply())
      case _ =>
    }
    (cpg, cpgOutFileName)
  }

  private def relativePath(frontendName: String): Path =
    Path.of(ProjectRoot.relativise(s"joern-cli/frontends/$frontendName/target/universal/stage/bin"))

}
