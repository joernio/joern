package io.joern.c2cpg.testfixtures

import better.files.File
import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.generated.Cpg

trait AstC2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val cpg          = newEmptyCpg(Option(cpgOutFile.pathAsString))
    val pathAsString = sourceCodePath.getAbsolutePath
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(pathAsString)
      .withOutputPath(pathAsString)
    val astCreationPass = new AstCreationPass(cpg, config)
    astCreationPass.createAndApply()
    new FunctionDeclNodePass(cpg, astCreationPass.unhandledMethodDeclarations())(ValidationMode.Enabled)
      .createAndApply()
    cpg
  }
}
