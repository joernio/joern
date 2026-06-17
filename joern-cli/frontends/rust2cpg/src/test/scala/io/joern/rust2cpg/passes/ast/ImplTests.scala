package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ImplTests extends Rust2CpgSuite(noSysRoot = true) {

  "an inherent method in an impl block" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn bar(&self) {}
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.method.nameExact("bar").fullName.l shouldBe List("rust2cpgtest::Foo::bar")
    }

    "have correct return typeFullName" in {
      cpg.method.nameExact("bar").methodReturn.typeFullName.l shouldBe List("()")
    }

    "have correct self properties" in {
      inside(cpg.method.nameExact("bar").parameter.nameExact("self").l) { case self :: Nil =>
        self.index shouldBe 0
        self.order shouldBe 0
        self.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
        self.typeFullName shouldBe "&rust2cpgtest::Foo"
      }
    }

    "be an AST child of the corresponding TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").method.nameExact("bar").fullName.l shouldBe List("rust2cpgtest::Foo::bar")
    }

    "not create a duplicate TYPE_DECL" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").size shouldBe 1
    }
  }

  "an inherent method with a `&mut self` and an explicit parameter" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn bar(&mut self, x: i32) {}
        |}
        |""".stripMargin)

    "have correct self properties" in {
      inside(cpg.method.nameExact("bar").parameter.nameExact("self").l) { case self :: Nil =>
        self.index shouldBe 0
        self.order shouldBe 0
        self.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
        self.typeFullName shouldBe "&mut rust2cpgtest::Foo"
      }
    }

    "have correct explicit parameter properties" in {
      inside(cpg.method.nameExact("bar").parameter.nameExact("x").l) { case param :: Nil =>
        param.index shouldBe 1
        param.order shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }
  }

  "an associated function without a receiver" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn new() -> Foo { Foo }
        |}
        |""".stripMargin)

    "have no parameters" in {
      cpg.method.nameExact("new").parameter shouldBe empty
    }

    "have correct return typeFullName" in {
      cpg.method.nameExact("new").methodReturn.typeFullName.l shouldBe List("rust2cpgtest::Foo")
    }
  }

  "multiple methods in a single impl block" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn a(&self) {}
        |  fn b(&self) {}
        |}
        |""".stripMargin)

    "lower each as an AST child of the corresponding TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").method.fullName.sorted.l shouldBe List(
        "rust2cpgtest::Foo::a",
        "rust2cpgtest::Foo::b"
      )
    }
  }

  "multiple impl blocks for the same type" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn a(&self) {}
        |}
        |impl Foo {
        |  fn b(&self) {}
        |}
        |""".stripMargin)

    "merge all methods under the same TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").method.fullName.sorted.l shouldBe List(
        "rust2cpgtest::Foo::a",
        "rust2cpgtest::Foo::b"
      )
    }

    "not create a duplicate TYPE_DECL" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").size shouldBe 1
    }
  }

  "a call to an inherent method" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo { fn bar(&self) {} }
        |fn run(f: Foo) { f.bar(); }
        |""".stripMargin)

    "have correct properties and callee" in {
      inside(cpg.call.nameExact("bar").l) { case call :: Nil =>
        implicit val callResolver: NoResolve.type = NoResolve
        call.methodFullName shouldBe "rust2cpgtest::Foo::bar"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.callee.l shouldBe cpg.method.fullNameExact("rust2cpgtest::Foo::bar").l
      }
    }

    "have correct arguments" in {
      inside(cpg.call.nameExact("bar").argument.l) { case (base: Identifier) :: Nil =>
        base.name shouldBe "f"
        base.argumentIndex shouldBe 0
        base.typeFullName shouldBe "&rust2cpgtest::Foo"
      }
    }

    "have correct self typeFullName" in {
      cpg.method.nameExact("bar").parameter.nameExact("self").typeFullName.l shouldBe List("&rust2cpgtest::Foo")
    }
  }

  "a call to an associated function" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo { fn new() -> Foo { Foo } }
        |fn run() { Foo::new(); }
        |""".stripMargin)

    "have correct properties and callee" in {
      inside(cpg.call.nameExact("new").l) { case call :: Nil =>
        implicit val callResolver: NoResolve.type = NoResolve
        call.methodFullName shouldBe "rust2cpgtest::Foo::new"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        call.callee.l shouldBe cpg.method.fullNameExact("rust2cpgtest::Foo::new").l
      }
    }

    "have no arguments" in {
      cpg.call.nameExact("new").argument shouldBe empty
    }
  }
}
