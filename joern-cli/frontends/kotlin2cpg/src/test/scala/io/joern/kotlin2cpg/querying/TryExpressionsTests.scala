package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TryExpressionsTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple `try`-expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun doSomething(x: Int): Int {
        |    val r = "41414141"
        |    val out = try {
        |        x
        |    } catch (e: Exception) {
        |        r.toInt()
        |    }
        |    return out
        |}
        |
        |fun main() {
        |    val out = doSomething(42424242)
        |    println(out)
        |// prints: `42424242`
        |}
        | """.stripMargin)

    "should contain a CALL for the `try`-expression with the correct props set" in {
      val List(c) = cpg.call.methodFullNameExact(Operators.tryCatch).l
      c.argument.size shouldBe 2

      val List(firstArg: Block, secondArg: Block) = c.argument.l
      firstArg.argumentIndex shouldBe 1
      secondArg.argumentIndex shouldBe 2
      firstArg.order shouldBe 1
      secondArg.order shouldBe 2

      val List(firstAstChildOfFirstArg: Identifier) = firstArg.astChildren.head.l
      firstAstChildOfFirstArg.order shouldBe 1
      firstAstChildOfFirstArg.name shouldBe "x"
      firstAstChildOfFirstArg.code shouldBe "x"
      firstAstChildOfFirstArg.typeFullName shouldBe "java.lang.Integer"
      firstAstChildOfFirstArg.lineNumber shouldBe Some(6)
      firstAstChildOfFirstArg.columnNumber shouldBe Some(8)

      val List(firstAstChildOfSecondArg: Call) = secondArg.astChildren.head.l
      firstAstChildOfSecondArg.order shouldBe 1
      firstAstChildOfSecondArg.name shouldBe "toInt"
      firstAstChildOfSecondArg.code shouldBe "r.toInt()"
      firstAstChildOfSecondArg.methodFullName shouldBe "java.lang.String.toInt:java.lang.Integer()"
    }
  }
}
