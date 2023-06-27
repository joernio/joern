package io.joern.go2cpg.testfixtures

import better.files.File
import io.joern.gosrc2cpg.{Config, GoSrc2Cpg}
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg
import org.scalatest.Inside
trait Go2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile("go2cpg.bin")
    cpgOutFile.deleteOnExit()
    val go2cpg = new GoSrc2Cpg()
    val config = Config()
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    go2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithGo(val fileSuffix: String) extends DefaultTestCpg with Go2CpgFrontend

class GoCodeToCpgSuite(fileSuffix: String = ".go")
    extends Code2CpgFixture(() => new DefaultTestCpgWithGo(fileSuffix))
    with Inside
