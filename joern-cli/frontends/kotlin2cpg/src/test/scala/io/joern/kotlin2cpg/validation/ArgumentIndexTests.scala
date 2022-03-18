package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  ClosureBinding,
  ControlStructure,
  FieldIdentifier,
  Identifier,
  Literal
}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class ArgumentIndexTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple if-expression inside DQE" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |import kotlin.random.Random
        |
        |fun main() {
        |    val randInt = Random.nextInt(2)
        |    val out =
        |        if (randInt == 0) {
        |            "heads"
        |        } else {
        |            "tails"
        |        }.plus(" came up")
        |    println(out)
        |}
        |""".stripMargin)

    "should contain a CALL for the DQE with ARGUMENTs with the correct index set" in {
      val List(c) = cpg.call.methodFullName(".*plus.*").l
      c.typeFullName shouldBe "java.lang.String"
      c.argument.size shouldBe 2

      val List(firstArg: Call, secondArg: Literal) = c.argument.l
      firstArg.argumentIndex shouldBe 0
      secondArg.argumentIndex shouldBe 1
    }
  }

  "CPG for code with simple when-expression inside DQE" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun main() {
        |    val randX = Random.nextInt(2)
        |    val out =
        |        when (randX) {
        |            0 -> "heads"
        |            1 -> "tails"
        |            else -> "invalid"
        |        }.plus(" came up")
        |    println(out)
        |}
        |""".stripMargin)

    "should contain a CALL for the DQE with ARGUMENTs with the correct index set" in {
      val List(c) = cpg.call.methodFullName(".*plus.*").l
      c.typeFullName shouldBe "java.lang.String"
      c.argument.size shouldBe 2

      val List(firstArg: ControlStructure, secondArg: Literal) = c.argument.l
      firstArg.argumentIndex shouldBe 0
      secondArg.argumentIndex shouldBe 1
    }
  }

  "CPG for code with simple try-catch-expression inside DQE" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package main
        |
        |fun main() {
        |    val aNumber = "41414141"
        |    val out =
        |        try {
        |            aNumber.toInt().toString()
        |        } catch (e: java.lang.NumberFormatException) {
        |            "invalid number"
        |        }.plus(" came up")
        |    println(out)
        |}
        |""".stripMargin)

    "should contain a CALL for the DQE with ARGUMENTs with the correct index set" in {
      val List(c) = cpg.call.methodFullName(".*plus.*").l
      c.typeFullName shouldBe "java.lang.String"
      c.argument.size shouldBe 2

      val List(firstArg: ControlStructure, secondArg: Literal) = c.argument.l
      firstArg.argumentIndex shouldBe 0
      secondArg.argumentIndex shouldBe 1
    }
  }
}
