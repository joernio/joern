package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite

class AstCreationForArraysTests extends GoCodeToCpgSuite {
  "AST Creation for Array Initialization" should {
    "be correct when a int array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [5]int{1,2}
          |}
          |""".stripMargin)

      val List(assignmentCallNode, arrayInitializerCallNode) = cpg.call.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [5]int{1,2}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[5]int{1,2}"

      cpg.literal.size shouldBe 2
      val List(literal1, literal2) = cpg.literal.l
      literal1.code shouldBe "1"
      literal2.code shouldBe "2"

      cpg.identifier.size shouldBe 1
      val List(identifierNode) = cpg.identifier.l
      identifierNode.code shouldBe "a"
    }

    "be correct when a string array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [5]string{"hello","world"}
          |}
          |""".stripMargin)

      val List(assignmentCallNode, arrayInitializerCallNode) = cpg.call.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [5]string{\"hello\",\"world\"}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[5]string{\"hello\",\"world\"}"

      cpg.literal.size shouldBe 2
      val List(literal1, literal2) = cpg.literal.l
      literal1.code shouldBe "\"hello\""
      literal2.code shouldBe "\"world\""

      cpg.identifier.size shouldBe 1
      val List(identifierNode) = cpg.identifier.l
      identifierNode.code shouldBe "a"
    }

    "be correct when a dynamic length array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [...]int{1,2}
          |}
          |""".stripMargin)

      val List(assignmentCallNode, arrayInitializerCallNode) = cpg.call.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [...]int{1,2}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[...]int{1,2}"

      cpg.literal.size shouldBe 2
      val List(literal1, literal2) = cpg.literal.l
      literal1.code shouldBe "1"
      literal2.code shouldBe "2"

      cpg.identifier.size shouldBe 1
      val List(identifierNode) = cpg.identifier.l
      identifierNode.code shouldBe "a"
    }

    "be correct when an empty array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [2]string{}
          |}
          |""".stripMargin)

      val List(assignmentCallNode, arrayInitializerCallNode) = cpg.call.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [2]string{}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[2]string{}"
      arrayInitializerCallNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      cpg.literal.size shouldBe 0

      cpg.identifier.size shouldBe 1
      val List(identifierNode) = cpg.identifier.l
      identifierNode.code shouldBe "a"

    }

    "be correct when initialized using array length" in {
      val cpg = code("""
          |package main
          |func main() {
          |	var a[2]int
          |}
          |""".stripMargin)

      cpg.local.size shouldBe 1
      val List(localNode) = cpg.local.l
      localNode.code shouldBe "a"
      localNode.lineNumber shouldBe Some(4)

      val List(arrayInitializerNode) = cpg.call.l
      arrayInitializerNode.name shouldBe Operators.arrayInitializer
      arrayInitializerNode.code shouldBe "[2]int"
      arrayInitializerNode.lineNumber shouldBe Some(4)
      arrayInitializerNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
