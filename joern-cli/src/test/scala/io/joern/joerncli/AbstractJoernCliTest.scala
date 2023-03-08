package io.joern.joerncli

import better.files.File
import io.joern.console.cpgcreation.{CCpgGenerator, JsSrcCpgGenerator}
import io.joern.console.FrontendConfig
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.utils.ProjectRoot

import scala.util.{Failure, Success}

trait AbstractJoernCliTest {

  protected def withTestCpg[T](file: File, language: String = Languages.C)(f: ((Cpg, String)) => T): T = {
    f(loadTestCpg(file, language))
  }

  private def loadTestCpg(file: File, language: String = Languages.C): (Cpg, String) = {
    val tmpFile        = File.newTemporaryFile("cpg", "bin")
    val cpgOutFileName = tmpFile.pathAsString
    tmpFile.delete()

    val cpgGenerator = language match {
      case Languages.C | Languages.CSHARP         => CCpgGenerator(new FrontendConfig(), ProjectRoot.find.path)
      case Languages.JSSRC | Languages.JAVASCRIPT => JsSrcCpgGenerator(new FrontendConfig(), ProjectRoot.find.path)
    }
    cpgGenerator.generate(inputPath = file.pathAsString, outputPath = cpgOutFileName) match {
      case Failure(exception) => throw new AssertionError("error while invoking cpgGenerator", exception)
      case _                  =>
    }

    // Link CPG fragments and enhance to create semantic CPG
    val cpg = DefaultOverlays.create(cpgOutFileName)
    (cpg, cpgOutFileName)
  }

}
