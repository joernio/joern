package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class ASTCreationForExpressionsTests extends GoCodeToCpgSuite {
  "AST Creation for expressions" should {
    "be correct for nested expression" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var x int
          |  var y int
          |  var z int
          |
          |  x = y + z
          |}
  """.stripMargin)

      val localX = cpg.local.order(2)
      localX.name.l shouldBe List("x")
      val localY = cpg.local.order(4)
      localY.name.l shouldBe List("y")
      val localZ = cpg.local.order(6)
      localZ.name.l shouldBe List("z")

      inside(cpg.method.name("method").ast.isCall.name(Operators.assignment).map(new OpNodes.Assignment(_)).l) {
        case List(assignment) =>
          assignment.target.code shouldBe "x"
          assignment.source.start.isCall.name.l shouldBe List(Operators.addition)
          inside(assignment.source.astChildren.l) { case List(id1: Identifier, id2: Identifier) =>
            id1.order shouldBe 1
            id1.code shouldBe "y"
            id2.order shouldBe 2
            id2.code shouldBe "z"
          }
      }
    }
  }

}
