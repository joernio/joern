package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.joern.x2cpg.testfixtures.{Code2CpgFixture, LanguageFrontend}
import org.scalatest.Inside

class JsSrc2CpgFrontend(override val fileSuffix: String = ".js") extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgOutFile = File.newTemporaryFile(suffix = "cpg.bin")
    cpgOutFile.deleteOnExit()
    val jssrc2cpg = new JsSrc2Cpg()
    val config    = Config(inputPath = sourceCodePath.getAbsolutePath, outputPath = cpgOutFile.pathAsString)
    jssrc2cpg.createCpg(config).get
  }
}

class JsSrc2CpgSuite(fileSuffix: String = ".js") extends Code2CpgFixture(new JsSrc2CpgFrontend(fileSuffix)) with Inside
