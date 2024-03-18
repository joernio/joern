package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class SwiftDefaultTestCpg(val fileSuffix: String)
    extends DefaultTestCpg
    with SwiftSrc2CpgFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    super.applyPasses()
    applyOssDataFlow()
  }
}
