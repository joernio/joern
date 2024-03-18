package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class SwiftAstTestCpg(val fileSuffix: String) extends DefaultTestCpg with AstSwiftSrc2CpgFrontend with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    // not needed
  }
}
