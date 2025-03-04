package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.passes.AstCreationPass
import io.joern.jssrc2cpg.passes.JavaScriptTypeNodePass
import io.joern.jssrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.FileUtil
import io.shiftleft.codepropertygraph.generated.Cpg

trait AstJsSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)
    val cpg          = newEmptyCpg(Option(cpgOutFile.toString))
    val pathAsString = sourceCodePath.getAbsolutePath
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(pathAsString)
      .withOutputPath(pathAsString)
    val astGenResult    = new AstGenRunner(config).execute(File(pathAsString))
    val astCreationPass = new AstCreationPass(cpg, astGenResult, config)(ValidationMode.Enabled)
    astCreationPass.createAndApply()
    JavaScriptTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
    cpg
  }
}
