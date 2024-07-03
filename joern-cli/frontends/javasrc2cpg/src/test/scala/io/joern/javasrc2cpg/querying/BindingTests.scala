package io.joern.javasrc2cpg.querying

import io.shiftleft.semanticcpg.language.*
import io.joern.javasrc2cpg.testfixtures.JavaSrcCode2CpgFixture

class BindingTests extends JavaSrcCode2CpgFixture {
  "override for generic method" should {
    lazy val cpg = code("""
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
        .map(binding => (binding.name, binding.signature, binding.methodFullName))
        .l
      methodBinding should contain theSameElementsAs List(
        ("accept", "void(java.lang.Integer)", "SomeConsumer.accept:void(java.lang.Integer)"),
        ("accept", "void(java.lang.Object)", "SomeConsumer.accept:void(java.lang.Integer)")
      )
    }
  }

  "override for generic method" should {
    lazy val cpg = code(
      """
        |import java.util.function.Consumer;
        |
        |class SomeConsumer<I extends Number> implements Consumer<I> {
        |  @Override
        |  public void accept(I i) {}
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

    "have two bindings for SomeConsumer" in {
      val typeDecl = cpg.typeDecl(".*SomeConsumer.*").head
      val methodBinding = typeDecl.methodBinding
        .name("accept")
        .map(binding => (binding.name, binding.signature, binding.methodFullName))
        .l
      methodBinding should contain theSameElementsAs List(
        ("accept", "void(java.lang.Number)", "SomeConsumer.accept:void(java.lang.Number)"),
        ("accept", "void(java.lang.Object)", "SomeConsumer.accept:void(java.lang.Number)")
      )
    }

    "have three bindings for OtherConsumer" in {

      val typeDecl = cpg.typeDecl(".*OtherConsumer.*").head
      val methodBinding = typeDecl.methodBinding
        .name("accept")
        .map(binding => (binding.name, binding.signature, binding.methodFullName))
        .l
      methodBinding should contain theSameElementsAs List(
        ("accept", "void(java.lang.Integer)", "OtherConsumer.accept:void(java.lang.Integer)"),
        ("accept", "void(java.lang.Number)", "OtherConsumer.accept:void(java.lang.Integer)"),
        ("accept", "void(java.lang.Object)", "OtherConsumer.accept:void(java.lang.Integer)")
      )
    }
  }
}
