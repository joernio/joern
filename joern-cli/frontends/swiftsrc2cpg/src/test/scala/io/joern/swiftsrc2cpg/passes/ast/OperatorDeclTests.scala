// This test file has been translated from swift/test/Parse/operator_decl.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class OperatorDeclTests extends AbstractPassTest {

  "OperatorDeclTests" should {

    "testOperatorDecl3" ignore AstFixture("prefix operator ++*++ : A") { cpg =>
      ???
    }

    "testOperatorDecl5" ignore AstFixture("postfix operator ++**+ : A") { cpg =>
      ???
    }

    "testOperatorDecl11a" ignore AstFixture("prefix operator ??") { cpg =>
      ???
    }

    "testOperatorDecl11b" ignore AstFixture("postfix operator ??") { cpg => ??? }

    "testOperatorDecl11c" ignore AstFixture("prefix operator !!") { cpg =>
      ???
    }

    "testOperatorDecl11d" ignore AstFixture("postfix operator !!") { cpg =>
      ???
    }

    "testOperatorDecl16" ignore AstFixture("""
        |precedencegroup F {
        |  higherThan: A, B, C
        |}
        |""".stripMargin) { cpg => ??? }

    "testOperatorDecl17" ignore AstFixture("""
        |precedencegroup BangBangBang {
        |  associativity: none
        |  associativity: left
        |}
        |""".stripMargin) { cpg => ??? }

    "testOperatorDecl19" ignore AstFixture("""
        |class Foo {
        |  infix operator |||
        |}
        |""".stripMargin) { cpg => ??? }

    "testOperatorDecl20" ignore AstFixture("infix operator **<< : UndeclaredPrecedenceGroup") { cpg => ??? }

    "testOperatorDecl21" ignore AstFixture(
      // Note: We should not allow specification of multiple precedence groups
      """
        |protocol Proto {}
        |infix operator *<*< : F, Proto
        |""".stripMargin
    ) { cpg => ??? }

    "testRegexLikeOperator" ignore AstFixture("prefix operator /^/") { cpg => ??? }

  }

}
