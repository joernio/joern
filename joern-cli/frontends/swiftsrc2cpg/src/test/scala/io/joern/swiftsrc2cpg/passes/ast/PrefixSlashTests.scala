package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class PrefixSlashTests extends SwiftSrc2CpgSuite {

  "PrefixSlashTests" should {

    "testPrefixSlash2" in {
      val cpg = code("""
        |prefix operator /
        |prefix func / <T> (_ x: T) -> T { x }
        |""".stripMargin)
      val List(slashFunc) = cpg.method.nameExact("/").l
      slashFunc.fullName shouldBe "Test0.swift:<global>./:(_:T)->T"
    }

    "testPrefixSlash4" in {
      val cpg = code("""
        |prefix operator /
        |_ = /E.e
        |(/E.e).foo(/0)
        |""".stripMargin)
      cpg.call.code.l should contain allOf ("_ = /E.e", "/E.e", "(/E.e).foo(/0)", "/0")
    }

    "testPrefixSlash6" in {
      val cpg = code("""
        |prefix operator /
        |foo(/E.e, /E.e)
        |foo((/E.e), /E.e)
        |foo((/)(E.e), /E.e)
        |""".stripMargin)
      val fooCalls = cpg.call.nameExact("foo").code.l
      fooCalls shouldBe List("foo(/E.e, /E.e)", "foo((/E.e), /E.e)", "foo((/)(E.e), /E.e)")
    }

    "testPrefixSlash8" in {
      val cpg = code("""
        |prefix operator /
        |_ = bar(/E.e) / 2
        |""".stripMargin)
      val List(barCall) = cpg.call.nameExact("bar").l
      barCall.code shouldBe "bar(/E.e)"
      cpg.call.code.l should contain("bar(/E.e) / 2")
    }

  }

}
