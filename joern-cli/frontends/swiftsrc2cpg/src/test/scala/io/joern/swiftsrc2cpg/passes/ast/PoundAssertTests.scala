// This test file has been translated from swift/test/Parse/pound_assert.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class PoundAssertTests extends AstSwiftSrc2CpgSuite {

  "PoundAssertTests" should {

    "testPoundAssert1" ignore {
      val cpg = code("#assert(true, 123)")
      ???
    }

    "testPoundAssert2" ignore {
      val cpg = code(""" #assert(true, "error \(1) message")""")
      ???
    }

    "testPoundAssert5" ignore {
      val cpg = code("""
        |func unbalanced1() {
        |  #assert(true)
        |}
        |""".stripMargin)
      ???
    }

    "testPoundAssert6" ignore {
      val cpg = code("""
        |func unbalanced2() {
        |  #assert(true, "hello world")
        |}
        |""".stripMargin)
      ???
    }

  }

}
