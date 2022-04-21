package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCodeToCpgFixture
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._

import java.io.File

class MethodTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
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
      |""".stripMargin

  "should contain exactly one non-stub method node in Foo with correct fields" in {
    val List(x) = cpg.typeDecl.name("Foo").method.nameNot("<init>").isExternal(false).l
    x.name shouldBe "foo"
    x.fullName shouldBe "Foo.foo:int(int,int)"
    x.code shouldBe "int foo(int param1, int param2)"
    x.signature shouldBe "int(int,int)"
    x.isExternal shouldBe false
    x.order shouldBe 1
    x.filename should (
      startWith(File.separator) or // Unix
        startWith regex "[A-Z]:"   // Windows
    )
    x.filename.endsWith(".java") shouldBe true
    x.lineNumber shouldBe Some(2)
    x.lineNumberEnd shouldBe Some(4)
    x.columnNumber shouldBe Some(4)
    x.columnNumberEnd shouldBe Some(4)
  }

  "should create correct method node for empty param list to non-static method" in {
    val x :: Nil = cpg.typeDecl.name("Baz").method.name("baz").l
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

class MethodTests2 extends JavaSrcCodeToCpgFixture {
  override val code: String =
    """
      |class Foo {
      |  static class Sub {
      |    void foo() {
      |      method(1);
      |    }
      |  }
      |  static void method(Integer x) {
      |  }
      |}
      |""".stripMargin
  "test methodFullName for call to static method of different class without scope" in {
    cpg.call("method").methodFullName.head shouldBe "Foo.method:void(java.lang.Integer)"
  }
}

class MethodTests3 extends JavaSrcCodeToCpgFixture {
  override val code: String =
    """
      |class Foo {
      |  static void staticMethod(Integer x) { }
      |  void virtualMethod(Integer x) { }
      |}
      |""".stripMargin
  "test method virtual modifier" in {
    cpg.method("staticMethod").isVirtual.size shouldBe 0
    cpg.method("virtualMethod").isVirtual.fullName.head shouldBe "Foo.virtualMethod:void(java.lang.Integer)"
  }
}
