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
      param.columnNumber shouldBe Some(7)
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

  "method with a normal single-line declaration and no annotation" should {
    val cpg = code("""
        |class Foo {
        |  void foo() {
        |  }
        |}
        |""".stripMargin)

    "position the method node at its name" in {
      val List(method) = cpg.method.name("foo").l
      method.lineNumber shouldBe Some(3)
      method.columnNumber shouldBe Some(8)
    }

    "position the implicit 'this' parameter at the method name" in {
      val List(thisParam) = cpg.method.name("foo").parameter.name("this").l
      thisParam.lineNumber shouldBe Some(3)
      thisParam.columnNumber shouldBe Some(8)
    }
  }

  "method with a normal single-line declaration and a same-line annotation" should {
    val cpg = code("""
        |class Foo {
        |  @Override void foo() {
        |  }
        |}
        |""".stripMargin)

    "position the method node at its name, after the annotation" in {
      val List(method) = cpg.method.name("foo").l
      method.lineNumber shouldBe Some(3)
      method.columnNumber shouldBe Some(18)
    }

    "position the implicit 'this' parameter at the method name, after the annotation" in {
      val List(thisParam) = cpg.method.name("foo").parameter.name("this").l
      thisParam.lineNumber shouldBe Some(3)
      thisParam.columnNumber shouldBe Some(18)
    }
  }

  "constructor with a normal single-line declaration and no annotation" should {
    val cpg = code("""
        |class Bar {
        |  Bar() {
        |  }
        |}
        |""".stripMargin)

    "position the constructor node and the implicit 'this' parameter at the same place" in {
      val List(constructor) = cpg.method.isConstructor.l
      constructor.lineNumber shouldBe Some(3)
      constructor.columnNumber shouldBe Some(3)

      val List(thisParam) = cpg.method.isConstructor.parameter.name("this").l
      thisParam.lineNumber shouldBe Some(3)
      thisParam.columnNumber shouldBe Some(3)
    }
  }

  "constructor with a normal single-line declaration and a same-line annotation" should {
    val cpg = code("""
        |class Bar {
        |  @Deprecated Bar() {
        |  }
        |}
        |""".stripMargin)

    "position the constructor node at its name, after the annotation" in {
      val List(constructor) = cpg.method.isConstructor.l
      constructor.lineNumber shouldBe Some(3)
      constructor.columnNumber shouldBe Some(15)
    }

    "position the implicit 'this' parameter at the constructor name, after the annotation" in {
      val List(thisParam) = cpg.method.isConstructor.parameter.name("this").l
      thisParam.lineNumber shouldBe Some(3)
      thisParam.columnNumber shouldBe Some(15)
    }
  }

  "method with a leading annotation and a multi-line signature" should {
    val cpg = code("""
        |class Foo {
        |  @Override
        |  public
        |  void
        |  foo() {
        |  }
        |}
        |""".stripMargin)

    "move the method node's own position to the name, past the annotation and return type" in {
      val List(method) = cpg.method.name("foo").l
      method.lineNumber shouldBe Some(6)
      method.columnNumber shouldBe Some(3)
    }

    "position the implicit 'this' parameter at the method name rather than the declaration start" in {
      val List(thisParam) = cpg.method.name("foo").parameter.name("this").l
      thisParam.lineNumber shouldBe Some(6)
      thisParam.columnNumber shouldBe Some(3)
    }

    "agree on the exact same position for the method node and the implicit 'this' parameter" in {
      val List(method)    = cpg.method.name("foo").l
      val List(thisParam) = cpg.method.name("foo").parameter.name("this").l
      thisParam.lineNumber shouldBe method.lineNumber
      thisParam.columnNumber shouldBe method.columnNumber
    }
  }

  "constructor with a leading annotation and a multi-line signature" should {
    val cpg = code("""
        |class Bar {
        |  @Deprecated
        |  public
        |  Bar() {
        |  }
        |}
        |""".stripMargin)

    "move the constructor node's own position to the name, past the annotation" in {
      val List(constructor) = cpg.method.isConstructor.l
      constructor.lineNumber shouldBe Some(5)
      constructor.columnNumber shouldBe Some(3)
    }

    "position the implicit 'this' parameter at the constructor name rather than the declaration start" in {
      val List(thisParam) = cpg.method.isConstructor.parameter.name("this").l
      thisParam.lineNumber shouldBe Some(5)
      thisParam.columnNumber shouldBe Some(3)
    }

    "agree on the exact same position for the constructor node and the implicit 'this' parameter" in {
      val List(constructor) = cpg.method.isConstructor.l
      val List(thisParam)   = cpg.method.isConstructor.parameter.name("this").l
      thisParam.lineNumber shouldBe constructor.lineNumber
      thisParam.columnNumber shouldBe constructor.columnNumber
    }
  }

  "method with an explicitly declared receiver ('this') parameter" should {
    val cpg = code("""
        |class Foo {
        |  @Override
        |  public
        |  void
        |  foo(Foo this, int x) {
        |  }
        |}
        |""".stripMargin)

    "still position the method node at the method name" in {
      val List(method) = cpg.method.name("foo").l
      method.lineNumber shouldBe Some(6)
      method.columnNumber shouldBe Some(3)
    }

    "still synthesize exactly one 'this' parameter, positioned at the explicit receiver parameter rather than the method name" in {
      val List(thisParam) = cpg.method.name("foo").parameter.name("this").l
      thisParam.lineNumber shouldBe Some(6)
      thisParam.columnNumber shouldBe Some(7)
      thisParam.index shouldBe 0
    }

    "still create the explicit 'x' parameter unaffected by the receiver parameter" in {
      val List(xParam) = cpg.method.name("foo").parameter.name("x").l
      xParam.index shouldBe 1
      xParam.typeFullName shouldBe "int"
    }
  }
}
