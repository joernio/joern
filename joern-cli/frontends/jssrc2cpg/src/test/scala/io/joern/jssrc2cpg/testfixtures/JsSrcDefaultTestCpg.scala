package io.joern.jssrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class JsSrcDefaultTestCpg(val fileSuffix: String) extends DefaultTestCpg with JsSrc2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }
}
