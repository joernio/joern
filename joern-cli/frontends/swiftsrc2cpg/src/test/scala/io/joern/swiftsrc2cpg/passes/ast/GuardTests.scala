// This test file has been translated from swift/test/Parse/guard.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GuardTests extends AbstractPassTest {

  "GuardTests" should {

    "testGuard1" ignore AstFixture("""
        |func noConditionNoElse() {
        |  guard {} else {}
        |}
        |""".stripMargin) { cpg => }

  }

}
