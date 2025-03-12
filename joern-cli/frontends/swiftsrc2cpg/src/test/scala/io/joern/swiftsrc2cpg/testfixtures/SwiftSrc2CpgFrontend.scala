package io.joern.swiftsrc2cpg.testfixtures

import io.joern.swiftsrc2cpg.{Config, SwiftSrc2Cpg}
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.utils.FileUtil

trait SwiftSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)
    val swiftsrc2cpg = new SwiftSrc2Cpg()
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.toString)
    swiftsrc2cpg.createCpg(config).get
  }
}
