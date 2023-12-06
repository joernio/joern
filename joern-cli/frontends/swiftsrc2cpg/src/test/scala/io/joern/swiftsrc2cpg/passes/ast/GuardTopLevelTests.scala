// This test file has been translated from swift/test/Parse/guard-top-level.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class GuardTopLevelTests extends AbstractPassTest {

  "GuardTopLevelTests" should {
    "testGuardTopLevel1" ignore AstFixture("""
        |let a: Int? = 1
        |guard let b = a else {}
        |""".stripMargin) { cpg => ??? }
  }

}
