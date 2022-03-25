package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ArrayAccessExprsTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple map construction and access" - {
    lazy val cpg = TestContext.buildCpg("""
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
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(14)

      val List(firstArg: Identifier, secondArg: Literal) = callNodeQ.argument.l
      firstArg.argumentIndex shouldBe 1
      firstArg.code shouldBe "foo"
      firstArg.lineNumber shouldBe c.lineNumber
      secondArg.argumentIndex shouldBe 2
      secondArg.code shouldBe "\"one\""
      secondArg.lineNumber shouldBe c.lineNumber
    }
  }

  "CPG for code with simple array construction and access" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args: Array<String>) {
        |    val foo = listOf(1, 2, 3)
        |    val bar = foo[1]
        |    println(foo)
        |}
        |""".stripMargin)

    "should contain a CALL node for `map[\"one\"]` with the correct properties set" in {
      val List(c) = cpg.call.code("val bar.*").argument.isCall.l
      c.code shouldBe "foo[1]"
      c.methodFullName shouldBe Operators.indexAccess
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(14)
    }
  }
}
