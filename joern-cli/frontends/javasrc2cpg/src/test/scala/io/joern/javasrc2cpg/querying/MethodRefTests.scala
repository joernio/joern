package io.joern.javasrc2cpg.querying

import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef}
import io.shiftleft.semanticcpg.language.*

class MethodRefTests extends JavaSrcCode2CpgFixture {

  "resolved method refs as call argument should be handled correctly" in {
    val cpg = code("""package b;
        |class Bar {
        |  void bar(int x);
        |}
        |""".stripMargin)
      .moreCode("""import b.Bar;
          |import java.util.function.Consumer;
          |
          |class Foo {
          |  void doNothing(Consumer<Integer> c) {}
          |
          |  void foo() {
          |    doNothing(Bar::bar);
          |  }
          |}
          |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "b.Bar.bar:void(int)"
      methodRef.typeFullName shouldBe "b.Bar"
      methodRef.code shouldBe "Bar::bar"
    }
  }

  "method refs with a known type decl but unresolved signature should be handled correctly" in {
    val cpg = code("""package b;
        |class Bar {
        |  void bar(int x);
        |}
        |""".stripMargin)
      .moreCode("""import b.Bar;
          |import java.util.function.Consumer;
          |
          |class Foo {
          |  void foo() {
          |    doNothing(Bar::bar);
          |  }
          |}
          |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "b.Bar.bar:<unresolvedSignature>"
      methodRef.typeFullName shouldBe "b.Bar"
      methodRef.code shouldBe "Bar::bar"
    }
  }

  "unresolved method refs should be handled correctly" in {
    val cpg = code("""
        |class Foo {
        |  void foo() {
        |    doNothing(Bar::bar);
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "<unresolvedNamespace>.bar:<unresolvedSignature>"
      methodRef.typeFullName shouldBe "ANY"
      methodRef.code shouldBe "Bar::bar"
    }
  }

  "resolved instance method refs should be handled correctly" in {
    val cpg = code("""package foo;
        |
        |import java.util.function.Consumer;
        |
        |public class Foo {
        |  void doNothing(Consumer<Integer> c) {}
        |
        |  void func(int x) {}
        |
        |  void foo() {
        |    Foo f = new Foo();
        |    doNothing(f::func);
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "foo.Foo.func:void(int)"
      methodRef.typeFullName shouldBe "foo.Foo"
      methodRef.code shouldBe "f::func"
    }
  }

  "instance method refs with types from imports should be handled correctly" in {
    val cpg = code("""package foo;
        |
        |import java.util.function.Consumer;
        |import bar.Bar;
        |
        |public class Foo {
        |  void doNothing(Consumer<Integer> c) {}
        |
        |  void foo() {
        |    Bar b = new Bar();
        |    doNothing(b::bar);
        |  }
        |}
        |""".stripMargin)

    inside(cpg.call.name("doNothing").argument.l) { case List(thisArg: Identifier, methodRef: MethodRef) =>
      thisArg.name shouldBe "this"

      methodRef.methodFullName shouldBe "bar.Bar.bar:<unresolvedSignature>"
      methodRef.typeFullName shouldBe "bar.Bar"
      methodRef.code shouldBe "b::bar"
    }
  }
}
