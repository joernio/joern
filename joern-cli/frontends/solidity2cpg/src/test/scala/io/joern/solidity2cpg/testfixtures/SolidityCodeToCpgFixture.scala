package io.joern.solidity2cpg.testfixtures

import io.joern.solidity2cpg.Solidity2Cpg
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.testfixtures.{CodeToCpgFixture, LanguageFrontend}

import java.io.File

class SolidityFrontend extends LanguageFrontend {

  override val fileSuffix: String = ".sol"

  override def execute(sourceCodeFile: File): Cpg = {
    new Solidity2Cpg().createCpg(sourceCodeFile.getAbsolutePath)
  }
}

class SolidityCodeToCpgFixture extends CodeToCpgFixture(new SolidityFrontend) {}
