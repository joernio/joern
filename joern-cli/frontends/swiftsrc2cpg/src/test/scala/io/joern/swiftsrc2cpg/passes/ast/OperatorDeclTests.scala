// This test file has been translated from swift/test/Parse/operator_decl.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class OperatorDeclTests extends SwiftSrc2CpgSuite {

  // Top-level operator/precedencegroup decls are parser-only — they leave the user file's
  // <global> method body empty and produce no user-defined methods or type decls.
  private def assertNoOpCpg(cpg: io.shiftleft.codepropertygraph.generated.Cpg): org.scalatest.Assertion = {
    val List(globalMethod) = cpg.method.nameExact("<global>").filename("Test0.swift").l
    globalMethod.block.astChildren.l shouldBe empty
    cpg.method.filename("Test0.swift").nameNot("<global>").l shouldBe empty
    cpg.typeDecl.filename("Test0.swift").nameNot("<global>").l shouldBe empty
  }

  "OperatorDeclTests" should {

    "testOperatorDecl3" in {
      val cpg = code("prefix operator ++*++ : A")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl5" in {
      val cpg = code("postfix operator ++**+ : A")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl11a" in {
      val cpg = code("prefix operator ??")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl11b" in {
      val cpg = code("postfix operator ??")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl11c" in {
      val cpg = code("prefix operator !!")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl11d" in {
      val cpg = code("postfix operator !!")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl16" in {
      val cpg = code("""
        |precedencegroup F {
        |  higherThan: A, B, C
        |}
        |""".stripMargin)
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl17" in {
      val cpg = code("""
        |precedencegroup BangBangBang {
        |  associativity: none
        |  associativity: left
        |}
        |""".stripMargin)
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl19" in {
      val cpg = code("""
        |class Foo {
        |  infix operator |||
        |}
        |""".stripMargin)
      // Class body's `infix operator |||` declaration is parser-only — the class still
      // produces a typeDecl and synthesized init, but no method named `|||`.
      val List(foo) = cpg.typeDecl.nameExact("Foo").l
      foo.fullName shouldBe "Test0.swift:<global>.Foo"
      cpg.method.nameExact("init").fullName.l should contain(
        "Test0.swift:<global>.Foo.init:()->Test0.swift:<global>.Foo"
      )
      cpg.method.nameExact("|||").l shouldBe empty
      cpg.call.l shouldBe empty
    }

    "testOperatorDecl20" in {
      val cpg = code("infix operator **<< : UndeclaredPrecedenceGroup")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl21" in {
      val cpg = code("""
        |protocol Proto {}
        |infix operator *<*< : F, Proto
        |""".stripMargin)
      // Protocol contributes a typeDecl; the `infix operator` declaration is parser-only.
      val List(proto) = cpg.typeDecl.nameExact("Proto").l
      proto.fullName shouldBe "Test0.swift:<global>.Proto"
      cpg.method.nameExact("init").fullName.l should contain(
        "Test0.swift:<global>.Proto.init:()->Test0.swift:<global>.Proto"
      )
      cpg.method.nameExact("*<*<").l shouldBe empty
      cpg.call.l shouldBe empty
    }

    "testRegexLikeOperator" in {
      val cpg = code("prefix operator /^/")
      assertNoOpCpg(cpg)
    }

  }

}
