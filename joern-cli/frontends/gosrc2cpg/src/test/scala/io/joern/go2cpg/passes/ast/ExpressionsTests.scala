package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class ExpressionsTests extends GoCodeToCpgSuite {
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

    cpg.local.name.l shouldBe List("x", "y", "z")
    val List(assignment) =
      cpg.method.name("method").ast.isCall.name(Operators.assignment).cast[OpNodes.Assignment].l
    assignment.target.code shouldBe "x"
    assignment.source.start.isCall.name.l shouldBe List(Operators.addition)
    val List(id1: Identifier, id2: Identifier) = assignment.source.astChildren.l: @unchecked
    id1.order shouldBe 1
    id1.code shouldBe "y"
    id2.order shouldBe 2
    id2.code shouldBe "z"
  }

  "be correct for expression with literal" in {
    val cpg = code("""
        |package main
        |func main() {
        |   var a = "Pandurang" + "Patil"
        |}
        |""".stripMargin)
    val List(x) = cpg.local.l
  }
}
