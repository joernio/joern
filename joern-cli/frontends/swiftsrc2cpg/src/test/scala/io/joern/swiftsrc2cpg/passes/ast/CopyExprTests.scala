// This test file has been translated from swift/test/Parse/copy_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class CopyExprTests extends AstSwiftSrc2CpgSuite {

  "CopyExprTests" should {

    "testGlobal" in {
      val cpg = code("""
        |var global: Int = 5
        |func testGlobal() {
        |  let _ = copy global
        |}
        |""".stripMargin)
      cpg.call.code(".*copy global").argument(2).isIdentifier.name.l shouldBe List("global")
    }

    "testLet" in {
      val cpg = code("""
        |func testLet() {
        |  let t = String()
        |  let _ = copy t
        |}
        |""".stripMargin)
      cpg.call.code(".*copy t").argument(2).isIdentifier.name.l shouldBe List("t")
    }

    "testVar" in {
      val cpg = code("""
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  let _ = copy t
        |}
        |""".stripMargin)
      cpg.call.code(".*copy t").argument(2).isIdentifier.name.l shouldBe List("t")
    }

    "testParseCanCopyClosureDollarIdentifier" in {
      val cpg = code("""
        |class Klass {}
        |let f: (Klass) -> () = {
        |  let _ = copy $0
        |}
        |""".stripMargin)
      cpg.call.code(".*copy \\$0").argument(2).isIdentifier.name.l shouldBe List("$0")
    }
  }

}
