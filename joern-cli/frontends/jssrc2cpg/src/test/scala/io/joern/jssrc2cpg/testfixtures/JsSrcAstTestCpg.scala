package io.joern.jssrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class JsSrcAstTestCpg(val fileSuffix: String) extends DefaultTestCpg with AstJsSrc2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    // not needed
  }
}
