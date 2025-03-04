package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.Config
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.joern.x2cpg.utils.FileUtil
import io.shiftleft.codepropertygraph.generated.Cpg

trait JsSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = FileUtil.newTemporaryFile(suffix = "cpg.bin")
    FileUtil.deleteOnExit(cpgOutFile)
    val jssrc2cpg = new JsSrc2Cpg()
    val config = getConfig()
      .fold(Config(tsTypes = false))(_.asInstanceOf[Config])
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.toString)
    jssrc2cpg.createCpg(config).get
  }
}
