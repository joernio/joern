package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.{JavaSrcCode2CpgFixture, JavaSrcCodeToCpgFixture}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.semanticcpg.language._

class NewTypeInferenceTests extends JavaSrcCode2CpgFixture {

  "constructors created from import info" should {
    val cpg = code("""
        |import a.b.c.Bar;
        |
        |class Foo {
        |  public void test2() {
        |    // Should create constructor for a.b.c.Bar
        |    Bar b = new Bar(0);
        |  }
        |}
        |""".stripMargin)

    "create a constructor based on import info" in {
      cpg.method.name("test2").call.nameExact("<operator>.alloc").l match {
        case alloc :: Nil =>
          alloc.typeFullName shouldBe "a.b.c.Bar"
          alloc.signature shouldBe "a.b.c.Bar()"
          alloc.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        case res => fail(s"Expected single alloc call but got $res")
      }

      val init = cpg.method.name("test2").call.nameExact("<init>").l match {
        case init :: Nil => init
        case res         => fail(s"Expected single init call but got $res")

      }

      init.typeFullName shouldBe "void"
      init.signature shouldBe "void(int)"
      init.methodFullName shouldBe "a.b.c.Bar.<init>:void(int)"

      init.argument.size shouldBe 2

      init.argument.l match {
        case List(obj: Identifier, arg: Literal) =>
          obj.name shouldBe "b"
          obj.code shouldBe "b"
          obj.typeFullName shouldBe "a.b.c.Bar"
          obj.order shouldBe 1
          obj.argumentIndex shouldBe 0

          arg.typeFullName shouldBe "int"
          arg.code shouldBe "0"
          arg.order shouldBe 2
          arg.argumentIndex shouldBe 1

        case res => fail(s"Expected identifier and literal arguments for init but got $res")
      }
    }
  }
}

class TypeInferenceTests extends JavaSrcCodeToCpgFixture {

  override val code: String =
    """
      |package pakfoo;
      |
      |import a.b.c.Bar;
      |import d.*;
      |import e.Unknown;
      |
      |class Foo extends Unknown {
      |
      |    public static void foo(int x) {}
      |
      |    public void test1() {
      |        // Should find a.b.c.Bar
      |        Bar b;
      |    }
      |
      |    public void test2() {
      |        // Should create constructor for a.b.c.Bar
      |        Bar b = new Bar(0);
      |    }
      |
      |    // Should find a.b.c.Bar for parameter type
      |    public void test3(Bar b) {}
      |
      |    public void test4(Bar b) {
      |        // Should find methodFullName a.b.c.Bar.bar:int()
      |        int x = b.bar();
      |    }
      |
      |    public void test6(Baz z) {}
      |
      |    public void test7(Bar b, Baz z) {
      |        // Should find methodFullName a.b.c.Bar.bar:void(d.Baz, int)
      |        b.bar(z, 1);
      |    }
      |
      |    public void test8() {
      |        // Should find methodFullName pakfoo.Foo.missing:void()
      |        this.missing();
      |    }
      |
      |    public void test9() {
      |        // Should find methodFullName e.Unknown.missing:void()
      |        super.missing();
      |    }
      |
      |    public void test10() {
      |        // Should find arg type
      |        Foo f = new Foo();
      |    }
      |}
      |""".stripMargin

  "should find typeFullName from matching import" in {
    cpg.method.name("test1").local.nameExact("b").l match {
      case b :: Nil => b.typeFullName shouldBe "a.b.c.Bar"

      case res => fail(s"Expected identifier b but got $res")
    }
  }

  "should find typeFullName for param from import" in {
    cpg.method.name("test3").parameter.index(1).l match {
      case param :: Nil =>
        param.name shouldBe "b"
        param.typeFullName shouldBe "a.b.c.Bar"

      case res => fail(s"Expected single parameter but got $res")
    }
  }

  "should find methodFullName for unresolved call in assignment" in {
    val call = cpg.method.name("test4").call.name("bar").l match {
      case call :: Nil => call

      case res => fail(s"Expected single call to bar but got $res")
    }

    call.typeFullName shouldBe "int"
    call.methodFullName shouldBe "a.b.c.Bar.bar:int()"
    call.signature shouldBe "int()"

    call.argument.l match {
      case (obj: Identifier) :: Nil =>
        obj.name shouldBe "b"
        obj.code shouldBe "b"
        obj.typeFullName shouldBe "a.b.c.Bar"

      case res => fail(s"Expected single argument b but got $res")
    }
  }

  "should find typeFullName for unresolved param from single wildcard import" in {
    cpg.method.name("test6").parameter.l match {
      case List(_, bazParam) =>
        bazParam.name shouldBe "z"
        bazParam.typeFullName shouldBe "d.Baz"

      case res => fail(s"Expected param of type d.Baz but got $res")
    }
  }

  "should find methodFullName for unresolved call with unresolved argument" in {
    val call = cpg.method.name("test7").call.name("bar").l match {
      case call :: Nil => call

      case res => fail(s"Expected single call to bar but got $res")
    }

    call.typeFullName shouldBe "void"
    call.signature shouldBe "void(d.Baz,int)"
    call.methodFullName shouldBe "a.b.c.Bar.bar:void(d.Baz,int)"

    call.argument.l match {
      case List(obj: Identifier, arg1: Identifier, arg2: Literal) =>
        obj.name shouldBe "b"
        obj.typeFullName shouldBe "a.b.c.Bar"

        arg1.name shouldBe "z"
        arg1.typeFullName shouldBe "d.Baz"

        arg2.code shouldBe "1"
        arg2.typeFullName shouldBe "int"

      case res => fail(s"Expected 3 arguments but got $res")
    }
  }

  "should guess the enclosing typeDecl type for unresolved explicit this calls" in {
    val call = cpg.method.name("test8").call.name("missing").l match {
      case call :: Nil => call

      case res => fail(s"Expected single call to missing but got $res")
    }

    call.typeFullName shouldBe "void"
    call.signature shouldBe "void()"
    call.methodFullName shouldBe "pakfoo.Foo.missing:void()"
    call.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

    call.argument.l match {
      case (obj: Identifier) :: Nil =>
        obj.name shouldBe "this"
        obj.order shouldBe 1
        obj.argumentIndex shouldBe 0
        obj.typeFullName shouldBe "pakfoo.Foo"

      case res => fail(s"Expected single this identifier but found $res")
    }
  }

  "should find type and methodFullName for unresolved super call" in {
    pendingUntilFixed {

      val call = cpg.method.name("test9").call.name("missing").l match {
        case call :: Nil => call

        case res => fail(s"Expected single call to missing but got $res")
      }

      call.typeFullName shouldBe "void"
      call.signature shouldBe "void()"
      call.methodFullName shouldBe "e.Unknown.missing:void()"
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      call.argument.l match {
        case (obj: Identifier) :: Nil =>
          obj.name shouldBe "super"
          obj.order shouldBe 1
          obj.argumentIndex shouldBe 0
          obj.typeFullName shouldBe "e.Unknown"

        case res => fail(s"Expected single super identifier but found $res")
      }
    }
  }

}
