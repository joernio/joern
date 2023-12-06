package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class DoExpressionTests extends AbstractPassTest {

  "DoExpressionTests" should {

    "testDoExpr1" ignore AstFixture("let x = do { 5 }") { cpg => ??? }

    "testDoExpr2" ignore AstFixture("let x = do { 5 } catch { 0 }") { cpg => ??? }

    "testDoExpr3" ignore AstFixture("func foo() { do { 5 } }") { cpg => ??? }

    "testDoExpr4" ignore AstFixture("func foo() { do { 5 } as Int }") { cpg => ??? }

    "testDoExpr5" ignore AstFixture("do { 5 } as Int") { cpg => ??? }

    "testDoExpr6" ignore AstFixture("""
        |func foo() {
        |  do { 5 } catch { 0 } as Int
        |}
        |""".stripMargin) { cpg => ??? }

    "testDoExpr7" ignore AstFixture("y = do { 5 } catch { 0 } as Int") { cpg => ??? }

    "testDoExpr8" ignore AstFixture("{ do { 5 } catch { 0 } }") { cpg => ??? }

    "testDoExpr11" ignore AstFixture("return do { 5 }") { cpg => ??? }

    "testDoExpr12" ignore AstFixture("""
        |return
        |do { 5 }
        |""".stripMargin) { cpg => ??? }

  }

}
