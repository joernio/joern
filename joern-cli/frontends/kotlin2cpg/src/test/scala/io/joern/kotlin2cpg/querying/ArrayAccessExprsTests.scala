package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ArrayAccessExprsTests extends KotlinCode2CpgFixture(withOssDataflow = true) {

  "CPG for code with simple map construction and access" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = mapOf("one" to 1, "two" to 2, "three" to 3)
        |    val bar = foo["one"]
        |    println(bar)
        |}
        |""".stripMargin)

    "should contain a CALL node for `foo[\"one\"]` with the correct properties set" in {
      def callNodeQ = cpg.call.code("val bar.*").argument.isCall

      val List(c) = callNodeQ.l
      c.code shouldBe "foo[\"one\"]"
      c.methodFullName shouldBe Operators.indexAccess
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(14)

      val List(firstArg: Identifier, secondArg: Literal) = callNodeQ.argument.l: @unchecked
      firstArg.argumentIndex shouldBe 1
      firstArg.code shouldBe "foo"
      firstArg.lineNumber shouldBe c.lineNumber
      secondArg.argumentIndex shouldBe 2
      secondArg.code shouldBe "\"one\""
      secondArg.lineNumber shouldBe c.lineNumber
      firstArg.refsTo.size shouldBe 1
    }
  }

  "CPG for code with simple array construction and access" should {
    val cpg = code("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = listOf(1, 2, 3)
        |    val bar = foo[1]
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for `map[\"one\"]` with the correct properties set" in {
      def callNodeQ = cpg.call.code("val bar.*").argument.isCall

      val List(c) = callNodeQ.l
      c.code shouldBe "foo[1]"
      c.methodFullName shouldBe Operators.indexAccess
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(14)

      val List(firstArg: Identifier, secondArg: Literal) = callNodeQ.argument.l: @unchecked
      firstArg.argumentIndex shouldBe 1
      firstArg.code shouldBe "foo"
      firstArg.lineNumber shouldBe c.lineNumber
      secondArg.argumentIndex shouldBe 2
      secondArg.code shouldBe "1"
      secondArg.lineNumber shouldBe c.lineNumber
      firstArg.refsTo.size shouldBe 1
    }
  }
}
