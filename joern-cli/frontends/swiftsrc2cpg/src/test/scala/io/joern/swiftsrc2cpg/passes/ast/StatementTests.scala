package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class StatementTests extends AbstractPassTest {

  "StatementTests" should {

    "testIf" ignore AstFixture("""
      |if let baz {}
      |if let self = self {}
      |""".stripMargin) { cpg => ??? }

    "testDoCatch" in AstFixture("""
      |do {
      |  try foo()
      |} catch {
      |  bar()
      |}
      |do { try foo() }
      |catch where (error as NSError) == NSError() {}
      |""".stripMargin) { cpg =>
      val List(doStructure1, doStructure2) =
        cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY).code("do \\{.*").l

      val List(tryBlock1) = doStructure1.astChildren.order(1).l
      tryBlock1.astChildren.isCall.codeExact("foo()").size shouldBe 1
      val List(catchBlock1) = doStructure1.astChildren.order(2).l
      catchBlock1.astChildren.isCall.codeExact("bar()").size shouldBe 1

      val List(tryBlock2) = doStructure2.astChildren.order(1).l
      tryBlock2.astChildren.isCall.codeExact("foo()").size shouldBe 1
      val List(catchBlock2) = doStructure2.astChildren.order(2).l
      catchBlock2.astChildren.isCall.code.l shouldBe List("(error as NSError) == NSError()")
    }

    "testReturn" ignore AstFixture("""
      |return actor
      |{ return 0 }
      |return
      |""".stripMargin) { cpg => ??? }

    "testMissingIfClauseIntroducer" ignore AstFixture("if _ = 42 {}") { cpg => ??? }

    "testIfHasSymbol" ignore AstFixture("""
      |if #_hasSymbol(foo) {}
      |if #_hasSymbol(foo as () -> ()) {}
      |""".stripMargin) { cpg => ??? }

    "testYield" ignore AstFixture("""
      |var x: Int {
      |  _read {
      |    yield &x
      |  }
      |}
      |func f() -> Int {
      |  yield 5
      |}
      |""".stripMargin) { cpg => ??? }

    "testTrailingClosureInIfCondition" ignore AstFixture("if test { $0 } {}") { cpg => ??? }

    "testClosureInsideIfCondition" ignore AstFixture("if true, {x}() {}") { cpg => ??? }

  }

}
