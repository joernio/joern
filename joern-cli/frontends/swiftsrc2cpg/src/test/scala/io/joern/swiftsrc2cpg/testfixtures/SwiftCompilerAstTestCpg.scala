package io.joern.swiftsrc2cpg.testfixtures

import io.joern.dataflowengineoss.testfixtures.SemanticTestCpg
import io.joern.x2cpg.testfixtures.DefaultTestCpg

class SwiftCompilerAstTestCpg(val fileSuffix: String)
    extends DefaultTestCpg
    with AstSwiftCompilerSrc2CpgFrontend
    with SemanticTestCpg {

  override protected def applyPasses(): Unit = {
    // not needed
  }
}
