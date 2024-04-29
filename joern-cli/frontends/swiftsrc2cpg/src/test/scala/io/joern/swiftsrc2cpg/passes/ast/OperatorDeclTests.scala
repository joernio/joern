// This test file has been translated from swift/test/Parse/operator_decl.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class OperatorDeclTests extends AstSwiftSrc2CpgSuite {

  "OperatorDeclTests" should {

    "testOperatorDecl3" ignore {
      val cpg = code("prefix operator ++*++ : A")
      ???
    }

    "testOperatorDecl5" ignore {
      val cpg = code("postfix operator ++**+ : A")
      ???
    }

    "testOperatorDecl11a" ignore {
      val cpg = code("prefix operator ??")
      ???
    }

    "testOperatorDecl11b" ignore {
      val cpg = code("postfix operator ??")
      ???
    }

    "testOperatorDecl11c" ignore {
      val cpg = code("prefix operator !!")
      ???
    }

    "testOperatorDecl11d" ignore {
      val cpg = code("postfix operator !!")
      ???
    }

    "testOperatorDecl16" ignore {
      val cpg = code("""
        |precedencegroup F {
        |  higherThan: A, B, C
        |}
        |""".stripMargin)
      ???
    }

    "testOperatorDecl17" ignore {
      val cpg = code("""
        |precedencegroup BangBangBang {
        |  associativity: none
        |  associativity: left
        |}
        |""".stripMargin)
      ???
    }

    "testOperatorDecl19" ignore {
      val cpg = code("""
        |class Foo {
        |  infix operator |||
        |}
        |""".stripMargin)
      ???
    }

    "testOperatorDecl20" ignore {
      val cpg = code("infix operator **<< : UndeclaredPrecedenceGroup")
      ???
    }

    "testOperatorDecl21" ignore {
      val cpg = code("""
        |protocol Proto {}
        |infix operator *<*< : F, Proto
        |""".stripMargin)
      ???
    }

    "testRegexLikeOperator" ignore {
      val cpg = code("prefix operator /^/")
      ???
    }

  }

}
