package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, NodeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class WhenExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple `when`-expression" should {
    val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  val x =  Random.nextInt(0, 3)
        |  val foo = when (x) {
        |      1 -> 123
        |      2 -> 234
        |      else -> {
        |          456
        |      }
        |  }
        |  println(foo)
        |}
        | """.stripMargin)

    "contain a call node with the correct props and children set" in {
      val List(c) = cpg.call("<operator>.when").l
      c.argumentIndex shouldBe 2
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val List(arg1: Block, arg2: Block, arg3: Block, arg4: Block) = c.argument.l: @unchecked
      arg1.argumentIndex shouldBe 1
      arg2.argumentIndex shouldBe 2
      arg3.argumentIndex shouldBe 3
      arg4.argumentIndex shouldBe 4
      val List(arg1First: Identifier) = arg1.astChildren.l: @unchecked
      arg1First.code shouldBe "x"
      val List(arg2First: Literal, arg2Second: Literal) = arg2.astChildren.l: @unchecked
      arg2First.code shouldBe "1"
      arg2Second.code shouldBe "123"
      val List(arg3First: Literal, arg3Second: Literal) = arg3.astChildren.l: @unchecked
      arg3First.code shouldBe "2"
      arg3Second.code shouldBe "234"
      val List(arg4First: Block)    = arg4.astChildren.l: @unchecked
      val List(arg4First1: Literal) = arg4First.astChildren.l: @unchecked
      arg4First1.code shouldBe "456"
    }
  }

  "CPG for code with if-else chain like `when`-expression" should {
    val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  val x =  true
        |  val y = false
        |  when {
        |      x -> 1
        |      y -> 2
        |      else -> 3
        |  }
        |}
        | """.stripMargin)

    "contain a call node with the correct props and children set" in {
      val List(outerConditional)                      = cpg.call(Operators.conditional).argumentIndex(-1).l
      val List(cond1, code1, nestedConditional: Call) = outerConditional.argument.l: @unchecked

      cond1.label shouldBe NodeTypes.IDENTIFIER
      cond1.code shouldBe "x"
      cond1.argumentIndex shouldBe 1

      code1.label shouldBe NodeTypes.LITERAL
      code1.code shouldBe "1"
      code1.argumentIndex shouldBe 2

      nestedConditional.argumentIndex shouldBe 3
      val List(cond2, code2, elseExpr) = nestedConditional.argument.l

      cond2.label shouldBe NodeTypes.IDENTIFIER
      cond2.code shouldBe "y"
      cond2.argumentIndex shouldBe 1

      code2.label shouldBe NodeTypes.LITERAL
      code2.code shouldBe "2"
      code2.argumentIndex shouldBe 2

      elseExpr.label shouldBe NodeTypes.LITERAL
      elseExpr.code shouldBe "3"
      elseExpr.argumentIndex shouldBe 3
    }
  }

  "CPG for code with `when` without `argument` and without `else` block" should {
    "be able to generate cpg" in {
      val cpg = code("""
          |package mypkg
          |
          |fun main() {
          |    val number = 5
          |
          |    when {
          |        number == 1 -> println("Number is 1")
          |        number == 2 -> println("Number is 2")
          |        number == 3 -> println("Number is 3")
          |    }
          |}
          |
          |""".stripMargin)

      cpg.identifier("number").nonEmpty shouldBe true
      cpg.literal("\"Number is 1\"").nonEmpty shouldBe true
    }
  }

  "smoke test for 'is'-pattern in 'when' statement" in {
    val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  when {
        |      "abc" is String -> 1
        |      else -> 2
        |  }
        |}
        | """.stripMargin)
    // Trigger actual cpg creation.
    cpg.graph
  }

  "smoke test for 'in'-pattern in 'when' statement" in {
    val cpg = code("""
        |package mypkg
        |
        |fun myfun() {
        |  when {
        |      "abc" in listOf<String>() -> 1
        |      else -> 2
        |  }
        |}
        | """.stripMargin)
    // Trigger actual cpg creation.
    cpg.graph
  }

}
