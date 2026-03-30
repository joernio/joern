package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class E2EGoValidationTests extends GoCodeToCpgSuite {

  "Type assertion expressions" should {
    val cpg = code("""
        |package main
        |func foo(x interface{}) {
        |  y := x.(int)
        |  _ = y
        |}
        |""".stripMargin)

    "produce a cast operator call node" in {
      val List(castCall) = cpg.call(Operators.cast).l
      castCall.typeFullName shouldBe "int"
    }
  }

  "Literal type inference per Go spec" should {
    val cpg = code("""
        |package main
        |func main() {
        |  a := 42
        |  b := 3.14
        |  c := "hello"
        |}
        |""".stripMargin)

    "infer int for integer literals" in {
      val List(lit) = cpg.literal.code("42").l
      lit.typeFullName shouldBe "int"
    }

    "infer float64 for float literals" in {
      val List(lit) = cpg.literal.code("3.14").l
      lit.typeFullName shouldBe "float64"
    }

    "infer string for string literals" in {
      val List(lit) = cpg.literal.code("\"hello\"").l
      lit.typeFullName shouldBe "string"
    }
  }
}
