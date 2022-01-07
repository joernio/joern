package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArithmeticOperationsTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |fun main(args : Array<String>) {
      |  println(1 + 2)
      |  println(1 - 2)
      |  println(1 * 2)
      |  println(1 / 2)
      |}
      |""".stripMargin)

  "should contain correct nuber of calls" in {
    cpg.call.size should not be 0
  }

  "should contain a call node for the addition operator" in {
    cpg.call(Operators.addition).size should not be 0
  }

  "should contain a call node for addition op with correct fields" in {
    cpg.call(Operators.addition).size shouldBe 1

    val List(p) = cpg.call(Operators.addition).l
    p.argument.size shouldBe 2
    p.lineNumber shouldBe Some(2)
    p.code shouldBe "1 + 2"
  }

  "should contain a call node for the subtraction operator" in {
    cpg.call(Operators.subtraction).size should not be 0
  }

  "should contain a call node for subtraction op with correct fields" in {
    cpg.call(Operators.subtraction).size shouldBe 1

    val List(p) = cpg.call(Operators.subtraction).l
    p.argument.size shouldBe 2
    p.lineNumber shouldBe Some(3)
    p.code shouldBe "1 - 2"
  }

  "should contain a call node for the multiplication operator" in {
    cpg.call(Operators.multiplication).size should not be 0
  }

  "should contain a call node for multiplication op with correct fields" in {
    cpg.call(Operators.multiplication).size shouldBe 1

    val List(p) = cpg.call(Operators.multiplication).l
    p.argument.size shouldBe 2
    p.lineNumber shouldBe Some(4)
    p.code shouldBe "1 * 2"
  }

  "should contain a call node for the division operator" in {
    cpg.call(Operators.division).size should not be 0
  }

  "should contain a call node for division op with correct fields" in {
    cpg.call(Operators.division).size shouldBe 1

    val List(p) = cpg.call(Operators.division).l
    p.argument.size shouldBe 2
    p.lineNumber shouldBe Some(5)
    p.code shouldBe "1 / 2"
  }
}
