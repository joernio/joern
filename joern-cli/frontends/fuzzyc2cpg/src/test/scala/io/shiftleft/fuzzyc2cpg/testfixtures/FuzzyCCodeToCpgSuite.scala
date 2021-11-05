package io.shiftleft.fuzzyc2cpg.testfixtures

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.fuzzyc2cpg.FuzzyC2Cpg
import io.shiftleft.semanticcpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.File

class FuzzycFrontend extends LanguageFrontend {
  def execute(sourceCodePath: File): Cpg = {
    val cpgFile = File.createTempFile("fuzzyc", ".zip")
    cpgFile.deleteOnExit()
    val fuzzyc2Cpg = new FuzzyC2Cpg()
    fuzzyc2Cpg.runAndOutput(Set(sourceCodePath.getAbsolutePath), Set(fileSuffix), Some(cpgFile.getAbsolutePath))
  }
  override val fileSuffix: String = ".c"
}

class FuzzyCCodeToCpgSuite extends CodeToCpgFixture(new FuzzycFrontend) {}
