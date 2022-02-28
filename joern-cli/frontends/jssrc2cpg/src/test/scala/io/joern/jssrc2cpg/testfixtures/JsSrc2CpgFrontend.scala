package io.joern.jssrc2cpg.testfixtures

import better.files.File
import io.joern.jssrc2cpg.JsSrc2Cpg
import io.joern.jssrc2cpg.JsSrc2Cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}
import org.scalatest.Inside

class JsSrc2CpgFrontend(override val fileSuffix: String = ".js") extends LanguageFrontend {
  def execute(sourceCodePath: java.io.File): Cpg = {
    val cpgFile = java.io.File.createTempFile("jssrc2cpg", ".bin")
    cpgFile.deleteOnExit()
    val packageJson = File(sourceCodePath.getPath) / "package.json"
    packageJson.createIfNotExists()
    packageJson.deleteOnExit()
    val jssrc2cpg = new JsSrc2Cpg()
    val config    = Config(inputPaths = Set(sourceCodePath.getAbsolutePath), outputPath = cpgFile.getAbsolutePath)
    jssrc2cpg.runAndOutput(config)
  }
}

class JsSrc2CpgSuite(fileSuffix: String = ".js") extends CodeToCpgFixture(new JsSrc2CpgFrontend(fileSuffix)) with Inside
