package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CallableReferenceTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple callback usage" should {

    val cpg = code("""
        |fun isOdd(x: Int) = x % 2 != 0
        |
        |fun firstOdd(x: Int): Int {
        |    val numbers = listOf(1, 2, x)
        |    val y = numbers.filter(::isOdd)[0]
        |    return y
        |}
        |
        |fun main(args : Array<String>) {
        |  println(firstOdd(3))
        |}
        |""".stripMargin)

    "should have a non-0 number of CALL nodes" in {
      cpg.call.size should not be 0
    }

    // TODO: add the rest of the test cases
  }
}
