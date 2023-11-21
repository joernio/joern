// This test file has been translated from swift/test/Parse/copy_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class CopyExprTests extends AbstractPassTest {

  "CopyExprTests" should {

    "testGlobal" ignore AstFixture("""
        |var global: Int = 5
        |func testGlobal() {
        |  let _ = copy global
        |}
        |""".stripMargin) { cpg => ??? }

    "testLet" ignore AstFixture("""
        |func testLet() {
        |  let t = String()
        |  let _ = copy t
        |}
        |""".stripMargin) { cpg => ??? }

    "testVar" ignore AstFixture("""
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  let _ = copy t
        |}
        |""".stripMargin) { cpg => ??? }

    "testParseCanCopyClosureDollarIdentifier" ignore AstFixture("""
        |class Klass {}
        |let f: (Klass) -> () = {
        |  let _ = copy $0
        |}
        |""".stripMargin) { cpg => ??? }
  }

}
