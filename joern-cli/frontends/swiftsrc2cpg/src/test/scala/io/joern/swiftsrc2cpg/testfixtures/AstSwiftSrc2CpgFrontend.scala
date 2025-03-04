package io.joern.swiftsrc2cpg.testfixtures

import better.files.File
import io.joern.swiftsrc2cpg.passes.AstCreationPass
import io.joern.swiftsrc2cpg.passes.SwiftTypeNodePass
import io.joern.swiftsrc2cpg.Config
import io.joern.swiftsrc2cpg.utils.AstGenRunner
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import io.joern.x2cpg.utils.FileUtil
import io.shiftleft.codepropertygraph.generated.Cpg

trait AstSwiftSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)
    val cpg          = newEmptyCpg(Option(cpgOutFile.toString))
    val pathAsString = sourceCodePath.getAbsolutePath
    var config = Config()
      .withInputPath(pathAsString)
      .withOutputPath(pathAsString)
    val definedConfig = getConfig().collect { case c: Config => config.withDefines(c.defines) }
    if (definedConfig.isDefined) {
      config = definedConfig.get
    }
    val astGenResult    = new AstGenRunner(config).execute(File(pathAsString))
    val astCreationPass = new AstCreationPass(cpg, astGenResult, config)(ValidationMode.Enabled)
    astCreationPass.createAndApply()
    SwiftTypeNodePass.withRegisteredTypes(astCreationPass.typesSeen(), cpg).createAndApply()
    cpg
  }
}
