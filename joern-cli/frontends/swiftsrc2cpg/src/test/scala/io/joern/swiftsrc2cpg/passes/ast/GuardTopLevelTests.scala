// This test file has been translated from swift/test/Parse/guard-top-level.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite

class GuardTopLevelTests extends SwiftSrc2CpgSuite {

  "GuardTopLevelTests" should {
    "testGuardTopLevel1" ignore {
      val cpg = code("""
        |let a: Int? = 1
        |guard let b = a else {}
        |""".stripMargin)
      ???
    }
  }

}
