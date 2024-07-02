package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import scala.collection.immutable.List

class ConditionalsTests extends GoCodeToCpgSuite {
  "AST Creation for conditionals" should {
    "be correct for if" in {

      val cpg = code("""
          |package main
          |func method() {
          |  var y int = 5
          |  var x int = 2
          |  if (x > 0) {
          |    y = 0
          |  }
          |}
  """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
        controlStruct.code shouldBe "if (x > 0)"
        controlStruct.controlStructureType shouldBe ControlStructureTypes.IF
        inside(controlStruct.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "x > 0"

        }
        controlStruct.whenTrue.assignment.code.l shouldBe List("y = 0")
      }
    }

    "be correct for if-else" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var y int = 6
          |  var x int = 9
          |  if (x > 0) {
          |    y = 0
          |  } else {
          |    y = 1
          |  }
          |}
      """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(ifStmt, elseStmt) =>
        ifStmt.controlStructureType shouldBe ControlStructureTypes.IF
        ifStmt.code shouldBe "if (x > 0)"
        elseStmt.controlStructureType shouldBe ControlStructureTypes.ELSE
        elseStmt.code shouldBe "else"

        inside(ifStmt.condition.l) { case List(cndNode) =>
          cndNode.code shouldBe "x > 0"
        }

        ifStmt.whenTrue.assignment
          .map(x => (x.target.code, x.source.code))
          .headOption shouldBe Some(("y", "0"))
        ifStmt.whenFalse.assignment
          .map(x => (x.target.code, x.source.code))
          .headOption shouldBe Some(("y", "1"))
      }
    }

  }

}
