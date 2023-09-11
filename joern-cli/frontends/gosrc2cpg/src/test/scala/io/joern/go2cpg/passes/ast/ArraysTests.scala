package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class ArraysTests extends GoCodeToCpgSuite {
  "AST Creation for Array Initialization" should {
    "be correct when a int array is declared" in {
      val cpg = code("""
          |package main
          |func main() {
          |	var a []int 
          |}
          |""".stripMargin)
      cpg.local("a").size shouldBe 1
      cpg.identifier("a").size shouldBe 1
      val List(x) = cpg.identifier("a").l
      x.code shouldBe "a"
    }
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
      arrayInitializerCallNode.typeFullName shouldBe "[]int"

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
      arrayInitializerCallNode.typeFullName shouldBe "[]string"

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
      arrayInitializerCallNode.typeFullName shouldBe "[]int"

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
      arrayInitializerCallNode.typeFullName shouldBe "[]string"

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
      localNode.typeFullName shouldBe "[]int"
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
      localNode.typeFullName shouldBe "[]int"

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
      arrayInitializerCallNode.typeFullName shouldBe "[]int"

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

  "be correct when array of struct within package same file" in {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        |name string
        |}
        |
        |func main() {
        | var a Node = Node{"value1"}
        | var b Node = Node{"value2"}
        |
        | c := []Node{a, b}
        |}
        |""".stripMargin)

    cpg.file.size shouldBe 2

    val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(12).l

    val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
    assignmentCallNode.name shouldBe Operators.assignment
    assignmentCallNode.code shouldBe "c := []Node{a, b}"

    arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
    arrayInitializerCallNode.code shouldBe "[]Node{a, b}"
    arrayInitializerCallNode.typeFullName shouldBe "[]main.Node"

    assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 3
    val List(identifier1, identifier2, identifier3) = assignmentCallNode.astChildren.isIdentifier.l
    identifier1.code shouldBe "c"
    identifier2.code shouldBe "a"
    identifier3.code shouldBe "b"
  }

  "be correct when array of struct within package but different file" in {
    val cpg = code(
      """
        |package main
        |
        |type Node struct {
        |name string
        |}
        |
        |""".stripMargin,
      "first.go"
    )
      .moreCode(
        """
          |package main
          |
          |func main() {
          | var a Node = Node{"value1"}
          | var b Node = Node{"value2"}
          |
          | c := []Node{a, b}
          |}
          |""".stripMargin,
        "second.go"
      )

    cpg.file.size shouldBe 3

    val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(8).l

    val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
    assignmentCallNode.name shouldBe Operators.assignment
    assignmentCallNode.code shouldBe "c := []Node{a, b}"

    arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
    arrayInitializerCallNode.code shouldBe "[]Node{a, b}"
    arrayInitializerCallNode.typeFullName shouldBe "[]main.Node"

    assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 3
    val List(identifier1, identifier2, identifier3) = assignmentCallNode.astChildren.isIdentifier.l
    identifier1.code shouldBe "c"
    identifier2.code shouldBe "a"
    identifier3.code shouldBe "b"
  }

  "be correct when array of struct in different package but same project" in {

    val cpg = code(
      """
        |package mypackage
        |
        |type Node struct {
        |name string
        |}
        |
        |""".stripMargin,
      "first.go"
    )
      .moreCode(
        """
          |package main
          |
          |import (
          | "mypackage"
          |)
          |
          |func main() {
          | var a mypackage.Node = mypackage.Node{"value1"}
          | var b mypackage.Node = mypackage.Node{"value2"}
          |
          | c := []mypackage.Node{a, b}
          |}
          |""".stripMargin,
        "second.go"
      )

    cpg.file.size shouldBe 3

    val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(12).l

    val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.l
    assignmentCallNode.name shouldBe Operators.assignment
    assignmentCallNode.code shouldBe "c := []mypackage.Node{a, b}"

    arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
    arrayInitializerCallNode.code shouldBe "[]mypackage.Node{a, b}"
    arrayInitializerCallNode.typeFullName shouldBe "[]mypackage.Node"

    assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 3
    val List(identifier1, identifier2, identifier3) = assignmentCallNode.astChildren.isIdentifier.l
    identifier1.code shouldBe "c"
    identifier2.code shouldBe "a"
    identifier3.code shouldBe "b"
  }

  "be correct when when a int array id initialized having pointer" in {
    val cpg = code("""
        |package main
        |func main() {
        | var a int = 1
        | var b int = 2
        | c := [5]*int{&a,&b}
        |}
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(6).l

    val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.name(Operators.arrayInitializer).l
    assignmentCallNode.name shouldBe Operators.assignment
    assignmentCallNode.code shouldBe "c := [5]*int{&a,&b}"

    arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
    arrayInitializerCallNode.code shouldBe "[5]*int{&a,&b}"
    arrayInitializerCallNode.typeFullName shouldBe "[]*int"

    assignmentCallNode.astChildren.isCall.name(Operators.addressOf).l.size shouldBe 2
    val List(call1, call2) = assignmentCallNode.astChildren.isCall.name(Operators.addressOf).l
    call1.code shouldBe "&a"
    call2.code shouldBe "&b"

    assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
    val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l.l
    identifierNode.code shouldBe "c"
  }

  "be correct when array of struct having pointer within package same file" in {
    val cpg = code("""
        |package main
        |
        |type Node struct {
        | name string
        |}
        |
        |func main() {
        | var a Node = Node{"value1"}
        | var b Node = Node{"value2"}
        |
        | c := []*Node{&a,&b}
        |}
        |""".stripMargin)

    val List(assignmentCallNode) = cpg.call(Operators.assignment).lineNumber(12).l

    val List(arrayInitializerCallNode) = assignmentCallNode.astChildren.isCall.name(Operators.arrayInitializer).l
    assignmentCallNode.name shouldBe Operators.assignment
    assignmentCallNode.code shouldBe "c := []*Node{&a,&b}"

    arrayInitializerCallNode.name shouldBe Operators.arrayInitializer
    arrayInitializerCallNode.code shouldBe "[]*Node{&a,&b}"
    arrayInitializerCallNode.typeFullName shouldBe "[]*main.Node"

    assignmentCallNode.astChildren.isCall.name(Operators.addressOf).l.size shouldBe 2
    val List(call1, call2) = assignmentCallNode.astChildren.isCall.name(Operators.addressOf).l
    call1.code shouldBe "&a"
    call2.code shouldBe "&b"

    assignmentCallNode.astChildren.isIdentifier.l.size shouldBe 1
    val List(identifierNode) = assignmentCallNode.astChildren.isIdentifier.l.l
    identifierNode.code shouldBe "c"
  }
}
