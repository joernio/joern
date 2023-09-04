package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class AstCreationForGenericTests extends GoCodeToCpgSuite {

  "AST Creation for generic class having single generic Type" should {
    "no generic method" in {
      val cpg = code("""
          |package main
          |func foo(value int64) {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)"
      val List(param) = method.parameter.name("value").l
      param.typeFullName shouldBe "int64"
    }

    "be correct method signature" in {
      val cpg = code("""
          |package main
          |func foo[T int64](value T) {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)"
    }

    "be correct method with bounded type parameter" in {
      val cpg = code("""
          |package main
          |func foo[T float](value T) {}
          |""".stripMargin)

      val List(param) = cpg.method.name("foo").parameter.name("value").l
      param.typeFullName shouldBe "float"
    }
  }
}
