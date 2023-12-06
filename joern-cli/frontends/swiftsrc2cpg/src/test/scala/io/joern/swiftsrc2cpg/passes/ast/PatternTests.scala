package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PatternTests extends AbstractPassTest {

  "PatternTests" should {

    "testNonBinding1" ignore AstFixture("if case let E<Int>.e(y) = x {}") { cpg => ??? }

    "testNonBinding2" ignore AstFixture("""
        |switch e {
        |  case let E<Int>.e(y):
        |    y
        |}
        |""".stripMargin) { cpg => ??? }

    "testNonBinding3" ignore AstFixture("if case let (y[0], z) = x {}") { cpg => ??? }

    "testNonBinding4" ignore AstFixture("""
        |switch x {
        |  case let (y[0], z):
        |    z
        |}
        |""".stripMargin) { cpg => ??? }

    "testNonBinding5" ignore AstFixture("if case let y[z] = x {}") { cpg => ??? }

    "testNonBinding6" ignore AstFixture("""
        |switch 0 {
        |  case let y[z]:
        |    z
        |   case y[z]:
        |     0
        |   default:
        |     0
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
