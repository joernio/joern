package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PrefixSlashTests extends AbstractPassTest {

  "PrefixSlashTests" should {

    "testPrefixSlash2" ignore AstFixture("""
        |prefix operator /
        |prefix func / <T> (_ x: T) -> T { x }
        |""".stripMargin) { cpg => ??? }

    "testPrefixSlash4" ignore AstFixture("""
        |prefix operator /
        |_ = /E.e
        |(/E.e).foo(/0)
        |""".stripMargin) { cpg => ??? }

    "testPrefixSlash6" ignore AstFixture("""
        |prefix operator /
        |foo(/E.e, /E.e)
        |foo((/E.e), /E.e)
        |foo((/)(E.e), /E.e)
        |""".stripMargin) { cpg => ??? }

    "testPrefixSlash8" ignore AstFixture("""
        |prefix operator /
        |_ = bar(/E.e) / 2
        |""".stripMargin) { cpg => ??? }

  }

}
