package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class VariableReferencingTests extends GoCodeToCpgSuite {
  "AST Creation for local nodes" should {
    val cpg = code("""package main
        |func main() {
        | x := 1
        | y := x
        |}
        |""".stripMargin)

    "test local variable exists" in {
      val localNode = cpg.method("main").local.name("x").head
      localNode.closureBindingId shouldBe None
    }

    "test identifier association to local nodes" in {
      val localNode = cpg.method("main").local.name("x").head
      localNode.referencingIdentifiers.l.size shouldBe 2
      localNode.referencingIdentifiers.lineNumber(3).code.head shouldBe "x"
      localNode.referencingIdentifiers.lineNumber(4).code.head shouldBe "x"
    }

    "test local variable line and column numbers" in {
      val localNodeX = cpg.method("main").local.name("x").head
      localNodeX.lineNumber shouldBe Some(3)
      localNodeX.columnNumber shouldBe Some(2)

      val localNodeY = cpg.method("main").local.name("y").head
      localNodeX.lineNumber shouldBe Some(3)
      localNodeX.columnNumber shouldBe Some(2)
    }
  }

  "Empty variable declaration" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var()
        |}
        |""".stripMargin)

    "test empty variable" in {
      cpg.method.size shouldBe 2
      cpg.method("main.<global>").local.size shouldBe 0
    }
  }
}
