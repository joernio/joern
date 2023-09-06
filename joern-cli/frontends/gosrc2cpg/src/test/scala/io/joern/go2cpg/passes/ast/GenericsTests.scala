package io.joern.go2cpg.passes.ast

import io.joern.go2cpg.testfixtures.GoCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class GenericsTests extends GoCodeToCpgSuite {

  "AST Creation for generic method having single type" should {
    "no generic method having single Type" in {
      val cpg = code("""
          |package main
          |func foo(value int64) {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)"
      val List(param) = method.parameter.name("value").l
      param.typeFullName shouldBe "int64"
    }

    "be correct method signature having single type" in {
      val cpg = code("""
              |package main
              |func foo[T int64](value T) {}
              |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)"
    }

    "be correct method with bounded type parameter having single type" in {
      val cpg = code("""
              |package main
              |func foo[T float32](value T) {}
              |""".stripMargin)

      val List(param) = cpg.method.name("foo").parameter.name("value").l
      param.typeFullName shouldBe "float32"
    }

    "be correct method with bounded type parameter with two method having single type" in {
      val cpg = code("""
              |package main
              |func foo[T float32](p1 T) {}
              |func boo[T int](p2 T) {}
              |""".stripMargin)

      val method1 = cpg.method.name("foo").head
      method1.signature shouldBe "main.foo(float32)"
      val param1 = method1.parameter.head
      param1.typeFullName shouldBe "float32"

      val method2 = cpg.method.name("boo").head
      method2.signature shouldBe "main.boo(int)"
      val param2 = method2.parameter.head
      param2.typeFullName shouldBe "int"
    }
  }

  "AST Creation for generic class having multiple type" should {
    "no generic method having multiple type" in {
      val cpg = code("""
              |package main
              |func foo(p1 int64, p2 float32) {}
              |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64, float32)"
      method.parameter.size shouldBe 2
    }

    "be correct method signature having multiple type" in {
      val cpg = code("""
              |package main
              |func foo[T int64 | float32](value T) {}
              |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64|float32)"
    }

    "be correct method with bounded type parameter having multiple generic type" in {
      val cpg = code("""
              |package main
              |func foo[T float32 | int64](value T) {}
              |""".stripMargin)

      val List(param) = cpg.method.name("foo").parameter.name("value").l
      param.typeFullName shouldBe "float32|int64"
    }

    "be correct method with bounded type parameter with two parameter" in {
      val cpg = code("""
              |package main
              |func foo[T float32 | int64](p1 T, p2 T) {}
              |""".stripMargin)

      val method = cpg.method.name("foo").head
      method.signature shouldBe "main.foo(float32|int64, float32|int64)"
      val param1 = method.parameter.head
      param1.typeFullName shouldBe "float32|int64"
      val param2 = method.parameter.head
      param2.typeFullName shouldBe "float32|int64"
    }

    "be correct method bounded type parameter with multiple type and single parameter" in {
      val cpg = code("""
          |package main
          |func foo[T float32, U int64](p1 U) {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)"
      val List(param) = method.parameter.name("p1").l
      param.typeFullName shouldBe "int64"
    }
  }

  "AST Creation for generic class having return type" should {
    "no generic return" in {
      val cpg = code("""
          |package main
          |func foo(value int64) string {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)string"
    }

    "be correct method with bounded generic type return" in {
      val cpg = code("""
          |package main
          |func foo[T int64](value T) T {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)int64"
    }

    "be correct method with multiple generic type having single return" in {
      val cpg = code("""
          |package main
          |func foo[T int64, U float32](value T) U {}
          |""".stripMargin)

      val List(method) = cpg.method.name("foo").l
      method.signature shouldBe "main.foo(int64)float32"
    }
  }
}
