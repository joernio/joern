package io.joern.c2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class CAstTestCpg(val fileSuffix: String) extends DefaultTestCpg with AstC2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    new AstLinkerPass(this).createAndApply()
  }
}
