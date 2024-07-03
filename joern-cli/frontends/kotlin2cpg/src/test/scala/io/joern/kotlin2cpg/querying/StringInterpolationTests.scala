package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class StringInterpolationTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with basic string interpolation" should {
    val cpg = code("""
        |fun main(args : Array<String>) {
        |  val name = "Peter"
        |  val age = 34
        |  println("$name is $age years old. The string length is ${name.length}")
        |}
        |""".stripMargin)

    "should contain a call node for the `formatString` operator" in {
      cpg.call(Operators.formatString).size should not be 0
    }

    "should contain call nodes for the `formattedValue` operator" in {
      cpg.call(Operators.formattedValue).size should not be 0

      val List(firstCall, secondCall, thirdCall) = cpg.call(Operators.formattedValue).l
      firstCall.code shouldBe "name"
      firstCall.typeFullName shouldBe "java.lang.String"
      secondCall.code shouldBe "age"
      secondCall.typeFullName shouldBe "int"
      thirdCall.code shouldBe "name.length"
      thirdCall.typeFullName shouldBe "int"
    }

    "should contain a call node for `formatString` op with correct fields" in {
      cpg.call(Operators.formatString).size shouldBe 1

      val List(c) = cpg.call(Operators.formatString).l
      c.argument.size shouldBe 3
      c.lineNumber shouldBe Some(5)
      c.code shouldBe "\"$name is $age years old. The string length is ${name.length}\""
    }

    "should contain a call node for the first `formattedValue` with correct fields" in {
      val List(a) = cpg.call(Operators.formatString).argument.argumentIndex(1).isCall.l
      a.name shouldBe Operators.formattedValue
      a.methodFullName shouldBe Operators.formattedValue
      a.lineNumber shouldBe Some(5)
      a.columnNumber shouldBe Some(12)
      a.code shouldBe "name"
      a.argument.size shouldBe 1
    }

    "should contain a call node for the second `formattedValue` with correct fields" in {
      val List(a) = cpg.call(Operators.formatString).argument.argumentIndex(2).isCall.l
      a.name shouldBe Operators.formattedValue
      a.methodFullName shouldBe Operators.formattedValue
      a.lineNumber shouldBe Some(5)
      a.columnNumber shouldBe Some(21)
      a.code shouldBe "age"
      a.argument.size shouldBe 1
    }

    "should contain a call node for the third `formattedValue` with correct fields" in {
      val List(a) = cpg.call(Operators.formatString).argument.argumentIndex(3).isCall.l
      a.name shouldBe Operators.formattedValue
      a.methodFullName shouldBe Operators.formattedValue
      a.lineNumber shouldBe Some(5)
      a.columnNumber shouldBe Some(59)
      a.code shouldBe "name.length"
      a.argument.size shouldBe 1
    }
  }
}
