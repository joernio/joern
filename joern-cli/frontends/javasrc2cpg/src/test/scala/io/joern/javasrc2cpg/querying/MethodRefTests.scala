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
}
