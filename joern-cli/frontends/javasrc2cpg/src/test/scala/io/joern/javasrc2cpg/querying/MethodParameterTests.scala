package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language.*

class MethodParameterTests2 extends JavaSrcCode2CpgFixture {
  "non generic method" should {
    val cpg = code("""
        |class Foo {
        |  int foo(int p1, int p2) {
        |     return 1;
        |  }
        |}
        |""".stripMargin)

    "have correct parameter properties for 'this'" in {
      val List(param) = cpg.method.name("foo").parameter.name("this").l
      param.order shouldBe 0
      param.index shouldBe 0
      param.lineNumber shouldBe Some(3)
      param.columnNumber shouldBe None
      param.typeFullName shouldBe "Foo"
      param.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
    }

    "have correct parameter properties for p1" in {
      val List(param) = cpg.method.name("foo").parameter.name("p1").l
      param.order shouldBe 1
      param.index shouldBe 1
      param.lineNumber shouldBe Some(3)
      param.columnNumber shouldBe Some(11)
      param.typeFullName shouldBe "int"
      param.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "have correct parameter properties for p2" in {
      val List(param) = cpg.method.name("foo").parameter.name("p2").l
      param.order shouldBe 2
      param.index shouldBe 2
      param.lineNumber shouldBe Some(3)
      param.columnNumber shouldBe Some(19)
      param.typeFullName shouldBe "int"
      param.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
    }

    "should allow traversing from parameter to method" in {
      cpg.parameter.name("p1").method.filter(_.isExternal == false).name.l shouldBe List("foo")
    }
  }

  "generic method with unbound type" should {
    val cpg = code("""
        |class Foo {
        |  <T> int foo(T p1) {
        |     return 1;
        |  }
        |}
        |""".stripMargin)
    "have correct type for parameter p1" in {
      val List(param) = cpg.method.name("foo").parameter.name("p1").l
      param.typeFullName shouldBe "java.lang.Object"
    }

    "generic method with type bound" should {
      val cpg = code("""
          |class Foo {
          |  <T extends java.lang.Number> int foo(T p1) {
          |     return 1;
          |  }
          |}
          |""".stripMargin)
      "have correct type for parameter p1" in {
        val List(param) = cpg.method.name("foo").parameter.name("p1").l
        param.typeFullName shouldBe "java.lang.Number"
      }
    }

    "generic method with bounded type parameter as type bound" should {
      val cpg = code("""
          |class Foo {
          |  <U extends java.lang.Number, T extends U> int foo(T p1) {
          |     return 1;
          |  }
          |}
          |""".stripMargin)
      "have correct type for parameter p1" in {
        val List(param) = cpg.method.name("foo").parameter.name("p1").l
        param.typeFullName shouldBe "java.lang.Number"
      }
    }

    "method with type parameter from generic class" should {
      val cpg = code("""
          |class Foo<T> {
          |  int foo(T p1) {
          |     return 1;
          |  }
          |}
          |""".stripMargin)

      "have correct type for parameter p1" in {
        val List(param) = cpg.method.name("foo").parameter.name("p1").l
        param.typeFullName shouldBe "java.lang.Object"
      }
    }

    "method with bounded type parameter from generic class" should {
      val cpg = code("""
          |class Foo<T extends java.lang.Number> {
          |  int foo(T p1) {
          |     return 1;
          |  }
          |}
          |""".stripMargin)

      "have correct type for parameter p1" in {
        val List(param) = cpg.method.name("foo").parameter.name("p1").l
        param.typeFullName shouldBe "java.lang.Number"
      }
    }

    "method with bounded type parameter as type bound from generic class" should {
      val cpg = code("""
          |class Foo<U extends java.lang.Number, T extends U> {
          |  int foo(T p1) {
          |     return 1;
          |  }
          |}
          |""".stripMargin)

      "have correct type for parameter p1" in {
        val List(param) = cpg.method.name("foo").parameter.name("p1").l
        param.typeFullName shouldBe "java.lang.Number"
      }
    }
  }

  "imported method parameter's external, non-generic type" should {
    val cpg = code("""
        |import foo.bar.Baz;
        |class Main {
        | void run(Baz p1) {}
        |}
        |""".stripMargin)

    "have correct type for parameter p1" in {
      val List(param) = cpg.method.name("run").parameter.name("p1").l
      param.typeFullName shouldBe "foo.bar.Baz"
    }
  }

  "imported method parameter's internal, generic type" should {
    val cpg = code("""
        |import java.util.*;
        |class Main {
        | void run(List<String> p1) {}
        |}
        |""".stripMargin)

    "have correct type for parameter p1" in {
      val List(param) = cpg.method.name("run").parameter.name("p1").l
      param.typeFullName shouldBe "java.util.List"
    }
  }

  "imported method parameter's external, generic type" should {
    val cpg = code("""
        |import foo.bar.Baz;
        |class Main {
        |  void run(Baz<String> p1) {}
        |}
        |""".stripMargin)

    "have correct type for parameter p1" in {
      val List(param) = cpg.method.name("run").parameter.name("p1").l
      param.typeFullName shouldBe "foo.bar.Baz"
    }
  }
}
