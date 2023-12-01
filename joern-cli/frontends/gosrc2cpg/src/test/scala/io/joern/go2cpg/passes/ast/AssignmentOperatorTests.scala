package io.joern.go2cpg.passes.ast

import io.joern.dataflowengineoss.language.*
import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class AssignmentOperatorTests extends GoCodeToCpgSuite {
  "Multiple varables declared and assigned with value in single statement" should {
    val cpg = code("""
          |package main
          |func main() {
          |	var a, b = "first", "second"
          | c, d := 10, "forth"
          |}
          |
          |""".stripMargin)
    "Check CALL Nodes" in {
      cpg.call(Operators.assignment).size shouldBe 4
      val List(a, b, c, d) = cpg.call(Operators.assignment).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "int"
      d.typeFullName shouldBe "string"
    }
  }

  "Multiple varables declared and assigned with value in single statement with one function call" should {
    val cpg = code("""
        |package main
        |func foo() int{
        |  return 0
        |}
        |func main() {
        |	var a, b = "first", "second"
        |   c, d, e := 10, "forth", foo()
        |}
        |""".stripMargin)
    "Check CALL Nodes" in {
      cpg.call(Operators.assignment).size shouldBe 5
      val List(a, b, c, d, e) = cpg.call(Operators.assignment).l
      a.typeFullName shouldBe "string"
      b.typeFullName shouldBe "string"
      c.typeFullName shouldBe "int"
      d.typeFullName shouldBe "string"
      e.typeFullName shouldBe "int"
    }
  }
}
