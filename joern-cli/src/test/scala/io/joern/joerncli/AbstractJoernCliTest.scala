package io.joern.joerncli

import io.joern.console.testing.InProcessCpgGenerator
import io.joern.x2cpg.frontendspecific.jssrc2cpg
import io.joern.x2cpg.passes.frontend.XTypeRecoveryConfig
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.layers.LayerCreatorContext
import io.shiftleft.semanticcpg.utils.FileUtil

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
      case Languages.C                            => InProcessCpgGenerator.c()
      case Languages.JSSRC | Languages.JAVASCRIPT => InProcessCpgGenerator.jssrc()
    }
    cpgGenerator.generate(inputPath = file.toString, outputPath = cpgOutFileName) match {
      case Failure(exception) => throw new AssertionError("error while invoking cpgGenerator", exception)
      case _                  =>
    }

    // Link CPG fragments and enhance to create semantic CPG
    val cpg = DefaultOverlays.create(cpgOutFileName)
    language match {
      case Languages.JSSRC | Languages.JAVASCRIPT =>
        new jssrc2cpg.JavaScriptPostProcessing(XTypeRecoveryConfig(enabledDummyTypes = false))
          .run(new LayerCreatorContext(cpg))
      case _ =>
    }
    (cpg, cpgOutFileName)
  }
}
