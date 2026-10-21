// This test file has been translated from swift/test/Parse/foreach_async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.SwiftSrc2CpgSuite
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class ForeachAsyncTests extends SwiftSrc2CpgSuite {

  "ForeachAsyncTests" should {

    "testForeachAsync4" in {
      val cpg = code("""
        |func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        |  var sum = 0
        |  // Simple foreach loop, using the variable in the body
        |  for await i in r {
        |    sum = sum + i
        |  }
        |}""".stripMargin)
      val List(forEach) = cpg.method.nameExact("for_each").l
      forEach.fullName shouldBe "Test0.swift:<global>.for_each:(r:AsyncRange,iir:AsyncIntRange)->ANY"
      val List(whileLoop) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l
      whileLoop.method.name shouldBe "for_each"
      cpg.call.nameExact("<operator>.iterator").code.l shouldBe List("<operator>.iterator(r)")
      cpg.call.code.l should contain("sum = sum + i")
    }

    "testForeachAsync5" in {
      val cpg = code("""
        |func for_each1(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        |  var sum = 0
        |  for await (i, j) in iir {
        |    sum = sum + i + j
        |  }
        |}
        |func for_each2(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        |  var sum = 0
        |  for await (i, j) : (Int, Int) in iir {
        |    sum = sum + i + j
        |  }
        |}
        |""".stripMargin)
      val List(forEach1, forEach2) = cpg.method.nameExact("for_each1", "for_each2").l
      forEach1.name shouldBe "for_each1"
      forEach2.name shouldBe "for_each2"
      val List(while1, while2) = cpg.controlStructure.controlStructureType(ControlStructureTypes.WHILE).l
      while1.method.name shouldBe "for_each1"
      while2.method.name shouldBe "for_each2"
      cpg.call.nameExact("<operator>.iterator").code.l shouldBe List(
        "<operator>.iterator(iir)",
        "<operator>.iterator(iir)"
      )
    }

  }

}
