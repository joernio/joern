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

    "testOperatorDecl19" ignore {
      val cpg = code("""
        |class Foo {
        |  infix operator |||
        |}
        |""".stripMargin)
      ???
    }

    "testOperatorDecl20" in {
      val cpg = code("infix operator **<< : UndeclaredPrecedenceGroup")
      assertNoOpCpg(cpg)
    }

    "testOperatorDecl21" ignore {
      val cpg = code("""
        |protocol Proto {}
        |infix operator *<*< : F, Proto
        |""".stripMargin)
      ???
    }

    "testRegexLikeOperator" in {
      val cpg = code("prefix operator /^/")
      assertNoOpCpg(cpg)
    }

  }

}
