// This test file has been translated from swift/test/Parse/pound_assert.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class PoundAssertTests extends SwiftSrc2CpgSuite {

  "PoundAssertTests" should {

    "testPoundAssert1" in {
      val cpg              = code("#assert(true, 123)")
      val List(assertCall) = cpg.call.nameExact("assert").l
      assertCall.code shouldBe "#assert(true, 123)"
      assertCall.ast.isLiteral.code.l shouldBe List("true", "123")
    }

    "testPoundAssert2" in {
      val cpg              = code(""" #assert(true, "error \(1) message")""")
      val List(assertCall) = cpg.call.nameExact("assert").l
      assertCall.code shouldBe "#assert(true, \"error \\(1) message\")"
      assertCall.ast.isLiteral.code.l should contain("true")
    }

    "testPoundAssert5" in {
      val cpg = code("""
        |func unbalanced1() {
        |  #assert(true)
        |}
        |""".stripMargin)
      val List(unbalanced1) = cpg.method.nameExact("unbalanced1").l
      val List(assertCall)  = unbalanced1.call.nameExact("assert").l
      assertCall.code shouldBe "#assert(true)"
      assertCall.ast.isLiteral.code.l shouldBe List("true")
    }

    "testPoundAssert6" in {
      val cpg = code("""
        |func unbalanced2() {
        |  #assert(true, "hello world")
        |}
        |""".stripMargin)
      val List(unbalanced2) = cpg.method.nameExact("unbalanced2").l
      val List(assertCall)  = unbalanced2.call.nameExact("assert").l
      assertCall.code shouldBe "#assert(true, \"hello world\")"
      assertCall.ast.isLiteral.code.l shouldBe List("true", "\"hello world\"")
    }

  }

}
