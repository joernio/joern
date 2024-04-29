package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class DoExpressionTests extends AstSwiftSrc2CpgSuite {

  "DoExpressionTests" should {

    "testDoExpr1" in {
      val cpg = code("let x = do { 5 }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

    "testDoExpr2" in {
      val cpg = code("let x = do { 5 } catch { 0 }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        val List(catchX) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isLiteral.code.l shouldBe List("0")
      }
    }

    "testDoExpr3" in {
      val cpg = code("func foo() { do { 5 } }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

    "testDoExpr4" in {
      val cpg = code("func foo() { do { 5 } as Int }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

    "testDoExpr5" in {
      val cpg = code("do { 5 } as Int")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

    "testDoExpr6" in {
      val cpg = code("""
        |func foo() {
        |  do { 5 } catch { 0 } as Int
        |}
        |""".stripMargin)
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        val List(catchX) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isLiteral.code.l shouldBe List("0")
      }
    }

    "testDoExpr7" in {
      val cpg = code("y = do { 5 } catch { 0 } as Int")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        val List(catchX) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isLiteral.code.l shouldBe List("0")
      }
    }

    "testDoExpr8" in {
      val cpg = code("{ do { 5 } catch { 0 } }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        val List(catchX) = t.astChildren.isControlStructure.isCatch.l
        catchX.order shouldBe 2
        catchX.ast.isLiteral.code.l shouldBe List("0")
      }
    }

    "testDoExpr11" in {
      val cpg = code("return do { 5 }")
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

    "testDoExpr12" in {
      val cpg = code("""
        |return
        |do { 5 }
        |""".stripMargin)
      inside(cpg.controlStructure.isTry.l) { case List(t) =>
        t.astChildren.isLiteral.order(1).code.l shouldBe List("5")
        t.astChildren.isControlStructure.isCatch shouldBe empty
      }
    }

  }

}
