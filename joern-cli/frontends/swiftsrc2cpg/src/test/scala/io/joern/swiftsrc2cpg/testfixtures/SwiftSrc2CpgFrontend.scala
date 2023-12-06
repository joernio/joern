package io.joern.swiftsrc2cpg.testfixtures

import better.files.File
import io.joern.swiftsrc2cpg.SwiftSrc2Cpg
import io.joern.swiftsrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, DefaultTestCpg, LanguageFrontend}
import org.scalatest.Inside

trait SwiftSrc2CpgFrontend extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val swiftsrc2cpg = new SwiftSrc2Cpg()
    val config = getConfig()
      .fold(Config())(_.asInstanceOf[Config])
      .withInputPath(sourceCodePath.getAbsolutePath)
      .withOutputPath(cpgOutFile.pathAsString)
    swiftsrc2cpg.createCpg(config).get
  }
}

class DefaultTestCpgWithSwiftSrc(val fileSuffix: String) extends DefaultTestCpg with SwiftSrc2CpgFrontend

class SwiftSrc2CpgSuite(fileSuffix: String = ".swift")
    extends Code2CpgFixture(() => new DefaultTestCpgWithSwiftSrc(fileSuffix))
    with Inside
