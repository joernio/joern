// This test file has been translated from swift/test/Parse/copy_expr.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class CopyExprTests extends AstSwiftSrc2CpgSuite {

  "CopyExprTests" should {

    "testGlobal" ignore {
      val cpg = code("""
        |var global: Int = 5
        |func testGlobal() {
        |  let _ = copy global
        |}
        |""".stripMargin)
      ???
    }

    "testLet" ignore {
      val cpg = code("""
        |func testLet() {
        |  let t = String()
        |  let _ = copy t
        |}
        |""".stripMargin)
      ???
    }

    "testVar" ignore {
      val cpg = code("""
        |func testVar() {
        |  var t = String()
        |  t = String()
        |  let _ = copy t
        |}
        |""".stripMargin)
      ???
    }

    "testParseCanCopyClosureDollarIdentifier" ignore {
      val cpg = code("""
        |class Klass {}
        |let f: (Klass) -> () = {
        |  let _ = copy $0
        |}
        |""".stripMargin)
      ???
    }
  }

}
