// This test file has been translated from swift/test/Parse/foreach.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ForeachTests extends AbstractPassTest {

  "ForeachTests" should {

    "testForeach2" ignore AstFixture("""
        |func for_each1(r: Range<Int>, iir: IntRange<Int>) {
        |  var sum = 0
        |  for i in r {
        |    sum = sum + i
        |  }
        |}
        |
        |func for_each2(r: Range<Int>, iir: IntRange<Int>) {
        |  var sum = 0
        |  for (i, j) in iir {
        |    sum = sum + i + j
        |  }
        |}
        |
        |func for_each3(r: Range<Int>, iir: IntRange<Int>) {
        |  var sum = 0
        |  for (i, j) : (Int, Int) in iir {
        |    sum = sum + i + j
        |  }
        |}
        |""".stripMargin) { cpg => ??? }

  }

}
