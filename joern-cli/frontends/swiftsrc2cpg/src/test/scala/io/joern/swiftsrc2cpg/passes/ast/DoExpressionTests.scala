package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class DoExpressionTests extends AstSwiftSrc2CpgSuite {

  "DoExpressionTests" should {

    "testDoExpr1" ignore {
      val cpg = code("let x = do { 5 }")
      ???
    }

    "testDoExpr2" ignore {
      val cpg = code("let x = do { 5 } catch { 0 }")
      ???
    }

    "testDoExpr3" ignore {
      val cpg = code("func foo() { do { 5 } }")
      ???
    }

    "testDoExpr4" ignore {
      val cpg = code("func foo() { do { 5 } as Int }")
      ???
    }

    "testDoExpr5" ignore {
      val cpg = code("do { 5 } as Int")
      ???
    }

    "testDoExpr6" ignore {
      val cpg = code("""
        |func foo() {
        |  do { 5 } catch { 0 } as Int
        |}
        |""".stripMargin)
      ???
    }

    "testDoExpr7" ignore {
      val cpg = code("y = do { 5 } catch { 0 } as Int")
      ???
    }

    "testDoExpr8" ignore {
      val cpg = code("{ do { 5 } catch { 0 } }")
      ???
    }

    "testDoExpr11" ignore {
      val cpg = code("return do { 5 }")
      ???
    }

    "testDoExpr12" ignore {
      val cpg = code("""
        |return
        |do { 5 }
        |""".stripMargin)
      ???
    }

  }

}
