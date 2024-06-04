package io.joern.c2cpg.testfixtures

import better.files.File
import io.joern.c2cpg.C2Cpg
import io.joern.c2cpg.Config
import io.joern.x2cpg.testfixtures.LanguageFrontend
import io.shiftleft.codepropertygraph.generated.Cpg

trait C2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val c2cpg = new C2Cpg()
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    c2cpg.createCpg(config).get
  }
}
