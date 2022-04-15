package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Local}
import io.shiftleft.semanticcpg.language._

class IdentifierReferencesTests extends AnyFreeSpec with Matchers {
  "CPG for code with shadowed local inside lambda" - {
    lazy val cpg = TestContext.buildCpg("""
        |package main
        |
        |fun main() {
        |    val x: Int? = 41414141
        |    val out =
        |      x.takeIf { in1 ->
        |        val x: Int? = 42424242
        |        val cmp = in1.takeIf { in2 ->
        |            x!! >= in2!!
        |        }
        |        cmp != null
        |    }
        |    println(out) // prints 41414141
        |}
        |""".stripMargin)

    "should contain LOCAL nodes with correctly-set referencing IDENTIFIERS" in {
      val List(outerScopeX: Local) = cpg.local.nameExact("x").lineNumber(4).l
      outerScopeX.referencingIdentifiers.size shouldBe 2
      outerScopeX.referencingIdentifiers.lineNumber.l shouldBe List(4, 6)

      val List(innerScopeX: Local) = cpg.local.nameExact("x").lineNumber(7).l
      innerScopeX.referencingIdentifiers.size shouldBe 2
      innerScopeX.referencingIdentifiers.lineNumber.l shouldBe List(7, 9)
    }
  }

  "CPG for code with locals with the same name, but in different scopes" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val x = "FIRST"
        |    if (true) {
        |        val x ="SECOND"
        |        if (true) {
        |            val x = "THIRD"
        |            println("third: " + x)
        |        } else {
        |            println("second: " + x)
        |        }
        |    }
        |    println("first: " + x)
        |}
        |""".stripMargin)

    "should contain LOCAL nodes with correctly-set referencing IDENTIFIERS" in {
      val List(firstX: Local, secondX: Local, thirdX: Local) = cpg.local.nameExact("x").l
      val List(firstXUsage: Identifier) = cpg.call.methodFullName(Operators.addition).code(".*first.*").argument(2).l
      firstX.referencingIdentifiers.id.l.contains(firstXUsage.id) shouldBe true
      firstXUsage.refsTo.size shouldBe 1

      val List(secondXUsage: Identifier) = cpg.call.methodFullName(Operators.addition).code(".*second.*").argument(2).l
      secondX.referencingIdentifiers.id.l.contains(secondXUsage.id) shouldBe true
      secondXUsage.refsTo.size shouldBe 1

      val List(thirdXUsage: Identifier) = cpg.call.methodFullName(Operators.addition).code(".*third.*").argument(2).l
      thirdX.referencingIdentifiers.id.l.contains(thirdXUsage.id) shouldBe true
      thirdXUsage.refsTo.size shouldBe 1
    }
  }
}
