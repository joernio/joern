package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class TypeFullNameTests extends GoCodeToCpgSuite {
  "Type check for declared primitive types" should {
    val cpg = code("""
        |package main
        |func main() {
        |   var a int = 1
        |   var b, c float32
        |   var d []int
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(a, b, c, d) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c, d) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "float32"
      d.typeFullName shouldBe "[]int"
    }
  }

  "Type check for implicit Type based on assigned literal" ignore {
    val cpg = code("""
        |package main
        |func main() {
        |   var a = 10
        |   var b = 20.5
        |   var c = [5]int{1,2}
        |}
        |""".stripMargin)

    "Check for local nodes" in {
      val List(a, b, c) = cpg.local.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "[]int"
    }

    "check for identifier nodes" in {
      val List(a, b, c) = cpg.identifier.l
      a.typeFullName shouldBe "int"
      b.typeFullName shouldBe "float32"
      c.typeFullName shouldBe "[]int"
    }
  }
}
