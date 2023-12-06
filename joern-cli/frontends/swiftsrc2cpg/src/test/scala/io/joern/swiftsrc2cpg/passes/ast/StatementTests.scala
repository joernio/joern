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

    "testDo" ignore AstFixture("do {}") { cpg => ??? }

    "testDoCatch" ignore AstFixture("""
      |do {} catch {}
      |do {}
      |catch where (error as NSError) == NSError() {}
      |""".stripMargin) { cpg => ??? }

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
