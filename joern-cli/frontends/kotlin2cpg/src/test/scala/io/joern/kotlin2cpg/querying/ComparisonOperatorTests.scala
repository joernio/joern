package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.Operators

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ComparisonOperatorTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple comparison operator usage" - {

    lazy val cpg = TestContext.buildCpg("""
        |fun main(args : Array<String>): Int {
        | val x: Int = 1
        | val y: Int = 2
        |
        | if (x == 1) {
        |   return 2
        | }
        |
        | if (x != 1) {
        |   return 1
        | }
        |
        | if (x < 1) {
        |   return 3
        | }
        |
        | if (x <= 1) {
        |   return 4
        | }
        |
        | if (x > 1) {
        |   return 5
        | }
        |
        | if (x >= 1) {
        |   return 6
        | }
        |}
        |""".stripMargin)

    "should have a non-0 number of CALL nodes" in {
      cpg.call.size should not be 0
    }

    "should contain a call node for the `equals` operator" in {
      cpg.call(Operators.equals).size should not be 0
    }

    "should contain a call node for the `notEquals` operator" in {
      cpg.call(Operators.notEquals).size should not be 0
    }

    "should contain a call node for the `greaterThan` operator" in {
      cpg.call(Operators.greaterThan).size should not be 0
    }

    "should contain a call node for the `greaterEqualsThan` operator" in {
      cpg.call(Operators.greaterEqualsThan).size should not be 0
    }

    "should contain a call node for the `lessThan` operator" in {
      cpg.call(Operators.lessThan).size should not be 0
    }

    "should contain a call node for the `lessEqualsThan` operator" in {
      cpg.call(Operators.lessEqualsThan).size should not be 0
    }
  }
}
