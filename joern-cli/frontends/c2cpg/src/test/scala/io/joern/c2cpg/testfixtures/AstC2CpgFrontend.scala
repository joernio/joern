package io.joern.c2cpg.testfixtures

import io.joern.c2cpg.Config
import io.joern.c2cpg.passes.AstCreationPass
import io.joern.c2cpg.passes.FunctionDeclNodePass
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil

trait AstC2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)
    val cpg          = newEmptyCpg(Option(cpgOutFile.toString))
    val pathAsString = sourceCodePath.getAbsolutePath
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(pathAsString)
      .withOutputPath(pathAsString)
      .withSchemaValidation(ValidationMode.Enabled)
    val astCreationPass = new AstCreationPass(cpg, config)
    astCreationPass.createAndApply()
    new FunctionDeclNodePass(cpg, astCreationPass.unhandledMethodDeclarations(), config).createAndApply()
    cpg
  }
}
