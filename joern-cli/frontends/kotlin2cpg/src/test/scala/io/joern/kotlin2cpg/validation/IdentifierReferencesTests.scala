package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

class IdentifierReferencesTests extends AnyFreeSpec with Matchers {
  "CPG for code with shadowed local inside lambda" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
}
