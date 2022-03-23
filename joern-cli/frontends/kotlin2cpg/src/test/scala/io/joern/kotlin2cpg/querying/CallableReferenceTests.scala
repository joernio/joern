package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallableReferenceTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple callback usage" - {

    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
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
