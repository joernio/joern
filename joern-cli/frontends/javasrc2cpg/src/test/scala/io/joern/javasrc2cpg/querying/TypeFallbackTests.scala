package io.joern.javasrc2cpg.queryinfieldsg

import io.joern.javasrc2cpg.Config
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class TypeFallbackTests extends JavaSrcCode2CpgFixture {
  private val typeFallbackDisabledConfig = Config().withDisableTypeFallback(true)

  "cpgs generated with type fallbacks disabled" should {

    "set the type of unresolved locals to <unresolvedNamespace>.Type" in {
      val cpg = code("""
          |class Foo {
          |  void foo() {
          |    Bar b = new Bar();
          |  }
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      cpg.call(".*alloc.*").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
      cpg.method("foo").local.name("b").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
    }

    "set the type of call receivers to <unresolvedNamespace>.Type" in {
      val cpg = code("""
          |class Foo {
          |  void foo(Bar b) {
          |    b.bar();
          |  }
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      inside(cpg.call.name("bar").l) { case List(barCall: Call) =>
        barCall.methodFullName shouldBe "<unresolvedNamespace>.Bar.bar:<unresolvedSignature>(0)"

        inside(barCall.receiver.l) { case List(bIdentifier: Identifier) =>
          bIdentifier.name shouldBe "b"
          bIdentifier.typeFullName shouldBe "<unresolvedNamespace>.Bar"
        }
      }
    }

    "set the type of unresolved parameters to <unresolvedNamespace>.Type" in {
      val cpg = code("""
          |class Foo {
          |  void foo(Bar b) { }
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      cpg.method("foo").parameter.name("b").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
    }

    "set the type of unresolved fields to <unresolvedNamespace>.Type" in {
      val cpg = code("""
          |class Foo {
          |  Bar b;
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      cpg.member("b").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
    }

    "set the type of unresolved pattern variables to <unresolvedNamespace>.Type" in {
      val cpg = code("""
          |class Foo {
          |  void foo(Object o) {
          |    if (o instanceof Bar b) {}
          |  }
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      cpg.method("foo").local.name("b").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
    }

    "not use wildcard imports as a fallback" in {
      val cpg = code("""
          |import testpackage.*;
          |
          |class Foo {
          |  void foo() {
          |    Bar b = new Bar();
          |  }
          |}
          |""".stripMargin)
        .withConfig(typeFallbackDisabledConfig)

      cpg.call(".*alloc.*").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
      cpg.method("foo").local.name("b").typeFullName.l shouldBe List("<unresolvedNamespace>.Bar")
    }
  }
}
