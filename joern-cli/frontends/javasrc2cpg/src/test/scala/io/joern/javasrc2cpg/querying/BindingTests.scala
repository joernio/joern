package io.joern.javasrc2cpg.querying

import io.shiftleft.semanticcpg.language._
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture

class BindingTests extends JavaSrcCode2CpgFixture {
  "override for generic method" should {
    val cpg = code("""
        |import java.util.function.Consumer;
        |
        |class SomeConsumer implements Consumer<Integer> {
        |  @Override
        |  public void accept(Integer i) {}
        |}
        |""".stripMargin)
    "have two bindings" in {
      val typeDecl = cpg.typeDecl(".*SomeConsumer.*").head
      val methodBinding = typeDecl.methodBinding
        .name("accept")
        .map(binding => (binding.methodFullName, binding.name, binding.signature))
        .l
      methodBinding should contain theSameElementsAs List(
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Integer)"),
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Object)")
      )
    }
  }

  "override for generic method" should {
    val cpg = code(
      """
        |import java.util.function.Consumer;
        |import a.b.C
        |
        |class SomeConsumer<I extends Number> implements Consumer<I> {
        |  @Override
        |  public void accept(I i) {}
        |
        |  void foo(C c) {}
        |}
        |""".stripMargin,
      "SomeConsumer.java"
    ).moreCode(
      """
        |import java.util.function.Consumer;
        |
        |class OtherConsumer<U extends Integer> implements SomeConsumer<U> {
        |  @Override
        |  public void accept(U i) {}
        |}
        |""".stripMargin,
      "OtherConsumer.java"
    )
    // TODO remove ignore. Currently this does not pass because of invalid
    // method full name.
    "have two bindings for SomeConsumer" ignore {
      val typeDecl = cpg.typeDecl(".*SomeConsumer.*").head
      val methodBinding = typeDecl.methodBinding
        .name("accept")
        .map(binding => (binding.methodFullName, binding.name, binding.signature))
        .l
      methodBinding should contain theSameElementsAs List(
        ("SomeConsumer.accept:void(java.lang.Number)", "accept", "void(java.lang.Number)"),
        ("SomeConsumer.accept:void(java.lang.Number)", "accept", "void(java.lang.Object)")
      )
    }
    // TODO remove ignore. Currently this does not pass because of invalid
    // method full name.
    "have three bindings for OtherConsumer" ignore {
      val typeDecl = cpg.typeDecl(".*SomeConsumer.*").head
      val methodBinding = typeDecl.methodBinding
        .name("accept")
        .map(binding => (binding.methodFullName, binding.name, binding.signature))
        .l
      methodBinding should contain theSameElementsAs List(
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Integer)"),
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Number)"),
        ("SomeConsumer.accept:void(java.lang.Integer)", "accept", "void(java.lang.Object)")
      )
    }
  }
}
