package io.joern.joerncli

import better.files.File
import io.joern.console.FrontendConfig
import io.joern.console.cpgcreation.{CCpgGenerator, JsSrcCpgGenerator}
import io.joern.jssrc2cpg.{JsSrc2Cpg, Config as JsConfig}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.utils.ProjectRoot

import java.nio.file.Path
import scala.util.Failure

trait AbstractJoernCliTest {

  protected def withTestCpg[T](file: File, language: String = Languages.C)(f: ((Cpg, String)) => T): T = {
    f(loadTestCpg(file, language))
  }

  private def loadTestCpg(file: File, language: String = Languages.C): (Cpg, String) = {
    val tmpFile        = File.newTemporaryFile("cpg", "bin")
    val cpgOutFileName = tmpFile.pathAsString
    tmpFile.delete()

    val cpgGenerator = language match {
      case Languages.C | Languages.CSHARP         => CCpgGenerator(new FrontendConfig(), relativePath("c2cpg"))
      case Languages.JSSRC | Languages.JAVASCRIPT => JsSrcCpgGenerator(new FrontendConfig(), relativePath("jssrc2cpg"))
    }
    cpgGenerator.generate(inputPath = file.pathAsString, outputPath = cpgOutFileName) match {
      case Failure(exception) => throw new AssertionError("error while invoking cpgGenerator", exception)
      case _                  =>
    }

    // Link CPG fragments and enhance to create semantic CPG
    val cpg = DefaultOverlays.create(cpgOutFileName)
    language match {
      case Languages.JSSRC | Languages.JAVASCRIPT =>
        JsSrc2Cpg.postProcessingPasses(cpg, Option(JsConfig().withDisableDummyTypes(true))).foreach(_.createAndApply())
      case _ =>
    }
    (cpg, cpgOutFileName)
  }

  private def relativePath(frontendName: String): Path =
    Path.of(ProjectRoot.relativise(s"joern-cli/frontends/$frontendName/target/universal/stage/bin"))

}
