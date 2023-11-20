

// This test file has been translated from swift/test/Parse/playground_lvalues.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class PlaygroundLvaluesTests extends AbstractPassTest {
  "testPlaygroundLvalues1" ignore AstFixture("") { cpg =>
    assertParse(
      """
      var a = 1, b = 2
      let z = 3
      """
    )
  }

  "testPlaygroundLvalues2" ignore AstFixture("") { cpg =>
    assertParse(
      """
      a
      (a, b)
      (a, z)
      """
    )
  }

}
