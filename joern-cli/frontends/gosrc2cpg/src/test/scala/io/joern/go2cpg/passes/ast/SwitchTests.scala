package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class SwitchTests extends GoCodeToCpgSuite {
  "AST Creation for switch case" should {
    "be correct for switch case 1" in {
      val cpg = code("""
          |package main
          |func method() {
          |  var marks int = 90
          |  var grade string = "B"
          |  switch marks {
          |      case 90: grade = "A"
          |      case 50,60,70: grade = "C"
          |      default: grade = "D"
          |   }
          |}
    """.stripMargin)
      inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
        controlStruct.code shouldBe "switch marks"
        controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
        inside(controlStruct.astChildren.l) { case List(cond: Identifier, switchBlock: Block) =>
          cond.code shouldBe "marks"
          switchBlock.astChildren.size shouldBe 12
          switchBlock.astChildren.code.l shouldBe List(
            "case 90",
            "90",
            "grade = \"A\"",
            "case 50",
            "50",
            "case 60",
            "60",
            "case 70",
            "70",
            "grade = \"C\"",
            "default",
            "grade = \"D\""
          )
        }
      }
    }
  }

  "be correct for switch case 2" in {

    val cpg = code("""
        |package main
        |func method() {
        |  var marks int = 90
        |  var grade string = "B"
        |  switch {
        |      case grade == "A" :
        |         marks = 95
        |      case grade == "B":
        |         marks = 80
        |   }
        |}
    """.stripMargin)
    inside(cpg.method.name("method").controlStructure.l) { case List(controlStruct: ControlStructure) =>
      controlStruct.code shouldBe "switch "
      controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
      inside(controlStruct.astChildren.l) { case List(switchBlock: Block) =>
        switchBlock.astChildren.size shouldBe 6
        switchBlock.astChildren.code.l shouldBe List(
          "case grade == \"A\"",
          "grade == \"A\"",
          "marks = 95",
          "case grade == \"B\"",
          "grade == \"B\"",
          "marks = 80"
        )
      }
    }
  }

  "be correct for switch case 3" in {

    val cpg = code("""
        |package main
        |func method() {
        |   var x interface{}
        |   var y int = 6
        |   switch i := x.(type) {
        |      case nil:
        |         y = 5
        |      case int:
        |         y = 8
        |      case float64:
        |         y = 12
        |   }
        |}
    """.stripMargin)
    val List(controlStruct: ControlStructure) = cpg.method.name("method").controlStructure.l
    controlStruct.code shouldBe "switch i := x.(type)"
    controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
    val List(assignment: Call, switchBlock: Block) = controlStruct.astChildren.l: @unchecked
    switchBlock.astChildren.size shouldBe 9
    switchBlock.astChildren.code.l shouldBe List(
      "case nil",
      "nil",
      "y = 5",
      "case int",
      "int",
      "y = 8",
      "case float64",
      "float64",
      "y = 12"
    )
  }

  "be correct for switch case 4" in {

    val cpg = code("""
        |package main
        |func method() {
        |   var x interface{}
        |   var y int = 6
        |   switch x.(type) {
        |      case nil:
        |         y = 5
        |      case int:
        |         y = 8
        |      case float64:
        |         y = 12
        |   }
        |}
    """.stripMargin)
    val List(controlStruct: ControlStructure) = cpg.method.name("method").controlStructure.l
    controlStruct.code shouldBe "switch x.(type)"
    controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
    val List(identifier: Call, switchBlock: Block) = controlStruct.astChildren.l: @unchecked
    switchBlock.astChildren.size shouldBe 9
    // TODO: something is wrong here. Identifier is being created for int, nil and float64
    switchBlock.astChildren.code.l shouldBe List(
      "case nil",
      "nil",
      "y = 5",
      "case int",
      "int",
      "y = 8",
      "case float64",
      "float64",
      "y = 12"
    )
  }

  "an unbound type switch on a non-identifier subject" should {
    val cpg = code("""
        |package main
        |type S struct { f any }
        |func g() any { return 1 }
        |func Helper() int { return 1 }
        |func Index(m map[string]any) int {
        |  switch m["k"].(type) {
        |    case int: return 1
        |  }
        |  return 0
        |}
        |func Slice(xs []any) int {
        |  switch xs[0].(type) {
        |    case int: return 1
        |  }
        |  return 0
        |}
        |func Selector(s S) int {
        |  switch s.f.(type) {
        |    case int: return 1
        |  }
        |  return 0
        |}
        |func Call() int {
        |  switch g().(type) {
        |    case int: return 1
        |  }
        |  return 0
        |}
        |func Paren(m map[string]any) int {
        |  switch (m["k"]).(type) {
        |    case int: return 1
        |  }
        |  return 0
        |}
        |""".stripMargin)

    "keep every method of the file in the CPG" in {
      cpg.method.name("Helper", "Index", "Slice", "Selector", "Call", "Paren").name.sorted.l shouldBe
        List("Call", "Helper", "Index", "Paren", "Selector", "Slice")
    }

    "create a switch with an `is` condition over the subject" in {
      // the parentheses are not part of the argument
      val expected = List(
        ("Call", "g()", "g()"),
        ("Index", "m[\"k\"]", "m[\"k\"]"),
        ("Paren", "(m[\"k\"])", "m[\"k\"]"),
        ("Selector", "s.f", "s.f"),
        ("Slice", "xs[0]", "xs[0]")
      )
      expected.foreach { case (method, subject, argument) =>
        val List(switch) = cpg.method.name(method).controlStructure.l
        switch.controlStructureType shouldBe ControlStructureTypes.SWITCH
        switch.code shouldBe s"switch $subject.(type)"
        val List(isCall) = switch.condition.isCall.l
        isCall.name shouldBe Operators.is
        isCall.argument.code.l shouldBe List(argument)
      }
    }
  }

  // TODO Need to handle `fallthrough` statements
  "ast creation for fallthrough" ignore {
    "be correct" in {

      val cpg = code("""package main
          |import "fmt"
          |func main() {
          |	num := 2
          |
          |	switch num {
          |	case 1:
          |		fmt.Println("Number is 1.")
          |	case 2:
          |		fmt.Println("Number is 2.")
          |		fallthrough
          |	case 3:
          |		fmt.Println("Number is 3.")
          |	default:
          |		fmt.Println("Number is not 1, 2, or 3.")
          |	}
          |}""".stripMargin)

      inside(cpg.method("main").controlStructure.l) { case List(switchStmt, fallThrough) =>
        fallThrough.controlStructureType shouldBe "fallthrough"
        fallThrough.code shouldBe "fallthrough"

      }

    }
  }

  "AST Creation for switch with an empty case" should {
    "not drop the file, and keep the cases that follow" in {
      val cpg = code("""
          |package main
          |func classify(x int) string {
          |  switch x {
          |  case 1:
          |  case 2:
          |    return "two"
          |  }
          |  return "other"
          |}
          |""".stripMargin)
      inside(cpg.method.name("classify").controlStructure.l) { case List(controlStruct: ControlStructure) =>
        controlStruct.controlStructureType shouldBe ControlStructureTypes.SWITCH
        inside(controlStruct.astChildren.l) { case List(cond: Identifier, switchBlock: Block) =>
          cond.code shouldBe "x"
          switchBlock.astChildren.code.l shouldBe List("case 1", "1", "case 2", "2", "return \"two\"")
        }
      }
    }
  }

}
