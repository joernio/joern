package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.semanticcpg.language.*

class TraitTests extends Rust2CpgSuite(noSysRoot = true) {

  "a top-level trait" should {
    val cpg = code("""
        |trait Foo {
        |  fn a(&self);
        |  fn b(&self) -> i32 { 4 }
        |}
        |""".stripMargin)

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::Foo")
    }

    "lower each as an AST child of the corresponding TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").method.fullName.sorted.l shouldBe List(
        "rust2cpgtest::Foo::a",
        "rust2cpgtest::Foo::b"
      )
    }

    "have correct self properties" in {
      inside(cpg.method.nameExact("a").parameter.nameExact("self").l) { case self :: Nil =>
        self.index shouldBe 0
        self.order shouldBe 0
        self.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
        self.typeFullName shouldBe "&rust2cpgtest::Foo"
      }
    }

    "have correct return typeFullName" in {
      cpg.method.nameExact("a").methodReturn.typeFullName.l shouldBe List("()")
      cpg.method.nameExact("b").methodReturn.typeFullName.l shouldBe List("i32")
    }

    "lower the method without a body" in {
      cpg.method.nameExact("a").block.astChildren shouldBe empty
    }

    "lower the method with a body" in {
      cpg.method.nameExact("b").block.astChildren.isReturn.code.l shouldBe List("4")
    }
  }

  "a trait associated function without a receiver" should {
    val cpg = code("""
        |trait Foo {
        |  fn new() -> Self;
        |}
        |""".stripMargin)

    "have no parameters" in {
      cpg.method.nameExact("new").parameter shouldBe empty
    }

    "have correct return typeFullName" in {
      // TODO(rust_ast_gen): even though `Self` is correct, for the lowering its not what we want to show.
      pendingUntilFixed(cpg.method.nameExact("new").methodReturn.typeFullName.l shouldBe List("rust2cpgtest::Foo"))
    }
  }

  "an empty trait" should {
    val cpg = code("trait Empty {}")

    "have a crate-prefixed fullName TYPE_DECL" in {
      cpg.typeDecl.nameExact("Empty").fullName.l shouldBe List("rust2cpgtest::Empty")
    }

    "have no methods" in {
      cpg.typeDecl.nameExact("Empty").method shouldBe empty
    }
  }
}
