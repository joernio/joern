package io.joern.x2cpg.testfixtures

import io.joern.x2cpg.X2Cpg

abstract class DefaultTestCpg extends TestCpg {
  override protected def applyPasses(): Unit = {
    X2Cpg.applyDefaultOverlays(this)
  }
}
