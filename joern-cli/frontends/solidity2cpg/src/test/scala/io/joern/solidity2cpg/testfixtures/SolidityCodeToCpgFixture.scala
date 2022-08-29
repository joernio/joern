package io.joern.solidity2cpg.testfixtures

import io.joern.solidity2cpg.Solidity2Cpg
import io.joern.x2cpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}
import io.shiftleft.codepropertygraph.Cpg

import java.io.File

class SolidityFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".sol"

  override def execute(sourceCodeFile: File): Cpg = {
    new Solidity2Cpg().createCpg(sourceCodeFile.getAbsolutePath)
  }
}

class SolidityCodeToCpgFixture extends CodeToCpgFixture(new SolidityFrontend) {}
