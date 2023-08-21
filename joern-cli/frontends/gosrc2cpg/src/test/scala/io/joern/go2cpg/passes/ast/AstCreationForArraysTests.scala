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

      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l

      val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [5]int{1,2}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[5]int{1,2}"
      arrayInitializerCallNode.typeFullName shouldBe "int[]"

      assignmentCallNode.astChildren.isLiteral.l.size shouldBe 2
      val List(literal1, literal2) = assignmentCallNode.astChildren.isLiteral.l
      literal1.code shouldBe "1"
      literal2.code shouldBe "2"
      literal1.typeFullName shouldBe "int"
      literal2.typeFullName shouldBe "int"

      assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
      val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l.l
      identifierNode.code shouldBe "a"
    }

    "be correct when a string array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [5]string{"hello","world"}
          |}
          |""".stripMargin)

      val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(4).l

      val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [5]string{\"hello\",\"world\"}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[5]string{\"hello\",\"world\"}"
      arrayInitializerCallNode.typeFullName shouldBe "string[]"

      assignmentCallNode.astChildren.isLiteral.l.size shouldBe 2
      val List(literal1, literal2) = assignmentCallNode.astChildren.isLiteral.l
      literal1.code shouldBe "\"hello\""
      literal2.code shouldBe "\"world\""
      literal1.typeFullName shouldBe "string"
      literal2.typeFullName shouldBe "string"

      assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
      val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l
      identifierNode.code shouldBe "a"
    }

    "be correct when a dynamic length array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [...]int{1,2}
          |}
          |""".stripMargin)

      val List(assignmentCallNode)       = cpg.call(Operators.assignment).lineNumber(4).l
      val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [...]int{1,2}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[...]int{1,2}"
      arrayInitializerCallNode.typeFullName shouldBe "int[]"

      assignmentCallNode.astChildren.isLiteral.l.size shouldBe 2
      val List(literal1, literal2) = assignmentCallNode.astChildren.isLiteral.l
      literal1.code shouldBe "1"
      literal2.code shouldBe "2"
      literal1.typeFullName shouldBe "int"
      literal2.typeFullName shouldBe "int"

      assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
      val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l
      identifierNode.code shouldBe "a"
    }

    "be correct when an empty array is initialized" in {
      val cpg = code("""
          |package main
          |func main() {
          |	a := [2]string{}
          |}
          |""".stripMargin)

      val List(assignmentCallNode)       = cpg.call(Operators.assignment).lineNumber(4).l
      val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
      assignmentCallNode.name shouldBe Operators.assignment
      assignmentCallNode.code shouldBe "a := [2]string{}"

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[2]string{}"
      arrayInitializerCallNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayInitializerCallNode.typeFullName shouldBe "string[]"

      assignmentCallNode.astChildren.isLiteral.l.size shouldBe 0

      assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
      val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l
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

      val List(arrayInitializerNode) = cpg.method("main").ast.isCall.l
      arrayInitializerNode.name shouldBe Operators.arrayInitializer
      arrayInitializerNode.code shouldBe "[2]int"
      arrayInitializerNode.lineNumber shouldBe Some(4)
      arrayInitializerNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayInitializerNode.typeFullName shouldBe "int[]"

    }

    "be correct when global variable is initialized using array length" in {
      val cpg = code("""
          |package main
          |var a[2]int
          |func main() {
          |}
          |""".stripMargin)

      cpg.local.size shouldBe 1
      val List(localNode) = cpg.local.l
      localNode.code shouldBe "a"
      localNode.lineNumber shouldBe Some(3)

      val List(arrayInitializerNode) = cpg.call.l
      arrayInitializerNode.name shouldBe Operators.arrayInitializer
      arrayInitializerNode.code shouldBe "[2]int"
      arrayInitializerNode.lineNumber shouldBe Some(3)
      arrayInitializerNode.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      arrayInitializerNode.typeFullName shouldBe "int[]"

    }

    // Might need to change comparisons of this test case
    "be correct when a global array is initialized" in {
      val cpg = code("""
          |package main
          |var a = [5]int{1,2}
          |func main() {
          |}
          |""".stripMargin)

      val List(assignmentCallNode)       = cpg.call(Operators.assignment).lineNumber(3).l
      val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
      assignmentCallNode.name shouldBe Operators.assignment

      arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
      arrayInitializerCallNode.code shouldBe "[5]int{1,2}"
      arrayInitializerCallNode.typeFullName shouldBe "int[]"

      assignmentCallNode.astChildren.isLiteral.l.size shouldBe 2
      val List(literal1, literal2) = assignmentCallNode.astChildren.isLiteral.l
      literal1.code shouldBe "1"
      literal2.code shouldBe "2"
      literal1.typeFullName shouldBe "int"
      literal2.typeFullName shouldBe "int"

      assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
      val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l
      identifierNode.code shouldBe "a"
    }
  }

  "code field for assignment operator" should {
    "be correct" ignore {
      val cpg = code("""
          |package main
          |var a = [5]int{1,2}
          |func main() {
          |}
          |""".stripMargin)
      val List(assignmentCallNode) = cpg.call(Operators.assignment).l
      // TODO: Fix the code format - there should be a = in between
      assignmentCallNode.code shouldBe "var a = [5]int{1,2}"
    }
  }
}
