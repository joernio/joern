package io.joern.c2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class CDefaultTestCpg(val fileSuffix: String) extends DefaultTestCpg with C2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }
}
