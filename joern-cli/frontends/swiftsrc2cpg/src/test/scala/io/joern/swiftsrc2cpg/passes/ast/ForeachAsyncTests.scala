// This test file has been translated from swift/test/Parse/foreach_async.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

class ForeachAsyncTests extends AstSwiftSrc2CpgSuite {

  "ForeachAsyncTests" should {

    "testForeachAsync4" ignore {
      val cpg = code("""
        |func for_each(r: AsyncRange<Int>, iir: AsyncIntRange<Int>) async {
        |  var sum = 0
        |  // Simple foreach loop, using the variable in the body
        |  for await i in r {
        |    sum = sum + i
        |  }
        |}""".stripMargin)
      ???
    }

    "testForeachAsync5" ignore {
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
      ???
    }

  }

}
