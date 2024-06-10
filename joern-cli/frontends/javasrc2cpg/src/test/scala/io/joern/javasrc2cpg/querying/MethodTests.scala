package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.semanticcpg.language._

import java.io.File

class MethodTests extends JavaSrcCode2CpgFixture {

  val cpg = code(
    """ class Foo {
      |   int foo(int param1, int param2) {
      |     return 1;
      |   }
      | }
      |
      | class Bar {
      |   static int bar(int param1, int param2) {
      |     return 1;
      |   }
      | }
      |
      | class Baz {
      |   void baz() {}
      | }
      |""".stripMargin,
    fileName = "Foo.java"
  )

  "should contain exactly one non-stub method node in Foo with correct fields" in {
    val List(x) =
      cpg.typeDecl.name("Foo").method.nameNot(io.joern.x2cpg.Defines.ConstructorMethodName).isExternal(false).l
    x.name shouldBe "foo"
    x.fullName shouldBe "Foo.foo:int(int,int)"
    x.code shouldBe "int foo(int param1, int param2)"
    x.signature shouldBe "int(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.filename shouldBe "Foo.java"
    x.lineNumber shouldBe Some(2)
    x.lineNumberEnd shouldBe Some(4)
    x.columnNumber shouldBe Some(4)
    x.columnNumberEnd shouldBe Some(4)
  }

  "should create correct method node for empty param list to non-static method" in {
    val x :: Nil = cpg.typeDecl.name("Baz").method.name("baz").l: @unchecked
    x.name shouldBe "baz"
    x.fullName shouldBe "Baz.baz:void()"
    x.code shouldBe "void baz()"
    x.signature shouldBe "void()"
    x.isExternal shouldBe false
  }

  "should return correct number of lines" in {
    cpg.method.name("foo").numberOfLines.l shouldBe List(3)
  }

  "should allow traversing to parameters for non-static methods" in {
    cpg.method.name("foo").parameter.name.toSetMutable shouldBe Set("this", "param1", "param2")
  }

  "should allow traversing to parameters for static methods" in {
    cpg.method.name("bar").parameter.name.toSetMutable shouldBe Set("param1", "param2")
  }

  "should allow traversing to methodReturn" in {
    cpg.method.name("foo").methodReturn.typeFullName.l shouldBe List("int")
  }

  "should allow traversing to file" in {
    cpg.method.name("foo").file.name.l should not be empty
  }

}

class MethodTests2 extends JavaSrcCode2CpgFixture {
  val cpg = code("""
      |class Foo {
      |  static class Sub {
      |    void foo() {
      |      method(1);
      |    }
      |  }
      |  static void method(Integer x) {
      |  }
      |}
      |""".stripMargin)
  "test methodFullName for call to static method of different class without scope" in {
    cpg.call("method").methodFullName.head shouldBe "Foo.method:void(java.lang.Integer)"
  }
}

class MethodTests3 extends JavaSrcCode2CpgFixture {
  val cpg = code("""
      |class Foo {
      |  static void staticMethod(Integer x) { }
      |  void virtualMethod(Integer x) { }
      |}
      |""".stripMargin)
  "test method virtual modifier" in {
    cpg.method("staticMethod").isVirtual.size shouldBe 0
    cpg.method("virtualMethod").isVirtual.fullName.head shouldBe "Foo.virtualMethod:void(java.lang.Integer)"
  }
}

class MethodTests4 extends JavaSrcCode2CpgFixture {

  "List<String> in the method return type" should {
    val cpg = code("""
        |import java.util.*;
        |class Foo {
        | List<String> run() {
        |   return null;
        | }
        |}
        |""".stripMargin)

    "have correct signature and full name" in {
      val List(method) = cpg.method("run").l
      method.signature shouldBe "java.util.List()"
      method.fullName shouldBe "Foo.run:java.util.List()"
    }
  }

  "Baz<String> in the method return type" should {
    val cpg = code("""
        |import foo.bar.Baz;
        |class Foo {
        | Baz<String> run() {
        |   return null;
        | }
        |}
        |""".stripMargin)

    "have correct signature and full name" in {
      val List(method) = cpg.method("run").l
      method.signature shouldBe "foo.bar.Baz()"
      method.fullName shouldBe "Foo.run:foo.bar.Baz()"
    }
  }

  "Identity method for Baz<String>" should {
    val cpg = code("""
        |import foo.bar.Baz;
        |class Foo {
        | Baz<String> run(Baz<String> x) {
        |   return x;
        | }
        |}
        |""".stripMargin)

    "have correct signature and full name" in {
      val List(method) = cpg.method("run").l
      method.signature shouldBe "foo.bar.Baz(foo.bar.Baz)"
      method.fullName shouldBe "Foo.run:foo.bar.Baz(foo.bar.Baz)"
    }
  }

  "Generic identity method for Baz<T>" should {
    val cpg = code("""
        |import foo.bar.Baz;
        |class Foo {
        | <T> Baz<T> run(Baz<T> x) {
        |   return x;
        | }
        |}
        |""".stripMargin)

    "have correct signature and full name" in {
      val List(method) = cpg.method("run").l
      method.signature shouldBe "foo.bar.Baz(foo.bar.Baz)"
      method.fullName shouldBe "Foo.run:foo.bar.Baz(foo.bar.Baz)"
    }
  }

  "Method parameters interspersed with comments" should {
    val cpg = code("""
        |class Foo {
        | abstract void run(
        |   /* comment for 1st argument */
        |   int arg1,
        |   int arg2, // comment for arg2
        |   int arg3);
        |}
        |""".stripMargin)

    "have correct `code` fields" in {
      cpg.method("run").parameter.indexFrom(1).l match
        case List(arg1, arg2, arg3) =>
          arg1.code shouldBe "int arg1"
          arg2.code shouldBe "int arg2"
          arg3.code shouldBe "int arg3"
        case result => fail(s"Expected 3 parameters but got $result")
    }
  }

}
