package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil.PathExt

import java.nio.file.Paths

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

    "have no modifiers" in {
      cpg.method.nameExact("bar").modifier shouldBe empty
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

  "an inherent method with a by-value `self` receiver" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn bar(self) {}
        |}
        |""".stripMargin)

    "have correct self properties" in {
      inside(cpg.method.nameExact("bar").parameter.nameExact("self").l) { case self :: Nil =>
        self.index shouldBe 0
        self.order shouldBe 0
        self.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
        self.typeFullName shouldBe "rust2cpgtest::Foo"
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

  "an associated function returning `Self`" should {
    val cpg = code("""
        |struct Foo;
        |impl Foo {
        |  fn new() -> Self { Foo }
        |}
        |""".stripMargin)

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
        "rust2cpgtest::Foo::<init>",
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
        "rust2cpgtest::Foo::<init>",
        "rust2cpgtest::Foo::a",
        "rust2cpgtest::Foo::b"
      )
    }

    "not create a duplicate TYPE_DECL" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").size shouldBe 1
    }
  }

  "multiple impl blocks for the same type spread across files" should {
    val cpg = code(
      """
        |struct Foo;
        |mod a;
        |mod b;
        |""".stripMargin,
      fileName = (Paths.get("src") / "lib.rs").toString
    ).moreCode(
      """
          |impl crate::Foo {
          | fn a(&self) {}
          |}
          |""".stripMargin,
      fileName = (Paths.get("src") / "a.rs").toString
    ).moreCode(
      """
        |impl crate::Foo {
        | fn b(&self) {}
        |}""".stripMargin,
      fileName = (Paths.get("src") / "b.rs").toString
    )

    "merge all methods under the same TYPE_DECL" in {
      cpg.typeDecl.nameExact("Foo").method.fullName.sorted.l shouldBe List(
        "rust2cpgtest::Foo::<init>",
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
      inside(cpg.call.nameExact("bar").argument.l) { case (base: Call) :: Nil =>
        base.name shouldBe Operators.addressOf
        base.code shouldBe "&f"
        base.argumentIndex shouldBe 0
        base.typeFullName shouldBe "&rust2cpgtest::Foo"
        inside(base.argument.l) { case (inner: Identifier) :: Nil =>
          inner.name shouldBe "f"
          inner.typeFullName shouldBe "rust2cpgtest::Foo"
        }
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

  "an impl block for a trait" should {
    val cpg = code("""
        |trait Bar {
        |  fn do_stuff(&self) -> i32;
        |  fn do_mut(&mut self);
        |  fn do_take(self);
        |}
        |struct Foo;
        |impl Bar for Foo {
        |  fn do_stuff(&self) -> i32 { 1 }
        |  fn do_mut(&mut self) {}
        |  fn do_take(self) {}
        |}
        |""".stripMargin)

    "have correct typeDecl fullnames" in {
      inside(cpg.typeDecl.nameExact("Foo").fullName.sorted.l) { case traitImplFullName :: typeFullName :: Nil =>
        typeFullName shouldBe "rust2cpgtest::Foo"
        traitImplFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>"
      }
    }

    "have correct inheritsFromTypeFullName" in {
      cpg.typeDecl
        .fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>")
        .inheritsFromTypeFullName
        .l shouldBe List("rust2cpgtest::Bar")
    }

    "have correct inheritsFromTypeFullName on the struct" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").inheritsFromTypeFullName.l shouldBe List(
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>"
      )
    }

    "have correct methodFullName" in {
      cpg.typeDecl
        .fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>")
        .method
        .fullName
        .sorted
        .l shouldBe List(
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_mut",
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff",
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_take"
      )
    }

    "have correct `&self` properties" in {
      inside(cpg.method.fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff").parameter.l) {
        case self :: Nil =>
          self.name shouldBe "self"
          self.index shouldBe 0
          self.order shouldBe 0
          self.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
          self.typeFullName shouldBe "&rust2cpgtest::Foo"
      }
    }

    "have correct `&mut self` properties" in {
      inside(cpg.method.fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_mut").parameter.l) {
        case self :: Nil =>
          self.name shouldBe "self"
          self.index shouldBe 0
          self.order shouldBe 0
          self.evaluationStrategy shouldBe EvaluationStrategies.BY_SHARING
          self.typeFullName shouldBe "&mut rust2cpgtest::Foo"
      }
    }

    "have correct `self` properties" in {
      inside(cpg.method.fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_take").parameter.l) {
        case self :: Nil =>
          self.name shouldBe "self"
          self.index shouldBe 0
          self.order shouldBe 0
          self.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
          self.typeFullName shouldBe "rust2cpgtest::Foo"
      }
    }

    "have correct return typeFullName" in {
      cpg.method
        .fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff")
        .methodReturn
        .typeFullName
        .l shouldBe List("i32")
    }

    "have a virtual modifier on the trait-impl method" in {
      cpg.method
        .fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff")
        .modifier
        .modifierType
        .l shouldBe List(ModifierTypes.VIRTUAL)
    }

    "create binding node for the method" in {
      inside(cpg.typeDecl.fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>").methodBinding.sortBy(_.name).l) {
        case bindingMut :: bindingStuff :: bindingTake :: Nil =>
          bindingMut.name shouldBe "do_mut"
          bindingMut.signature shouldBe "rust2cpgtest::Bar"
          bindingMut.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_mut"

          bindingStuff.name shouldBe "do_stuff"
          bindingStuff.signature shouldBe "rust2cpgtest::Bar"
          bindingStuff.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff"

          bindingTake.name shouldBe "do_take"
          bindingTake.signature shouldBe "rust2cpgtest::Bar"
          bindingTake.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_take"

      }
    }
  }

  "an impl overriding a trait default method" should {
    val cpg = code("""
        |trait Bar {
        |  fn a(&self) -> i32 { 0 }
        |  fn b(&self) -> i32 { 0 }
        |}
        |struct Foo;
        |impl Bar for Foo {
        |  fn b(&self) -> i32 { 1 }
        |}
        |""".stripMargin)

    "create a binding node for the overriding method" in {
      inside(cpg.typeDecl.fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>").methodBinding.l) {
        case binding :: Nil =>
          binding.name shouldBe "b"
          binding.signature shouldBe "rust2cpgtest::Bar"
          binding.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::b"
      }
    }
  }

  "a trait impl in a different file" should {
    val cpg = code(
      """
        |mod a;
        |trait Bar {
        |  fn do_stuff(&self) -> i32;
        |}
        |struct Foo;
        |""".stripMargin,
      fileName = (Paths.get("src") / "lib.rs").toString
    ).moreCode(
      """
        |impl crate::Bar for crate::Foo {
        |  fn do_stuff(&self) -> i32 { 1 }
        |}
        |""".stripMargin,
      fileName = (Paths.get("src") / "a.rs").toString
    )

    "have correct inheritsFromTypeFullName on the struct" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").inheritsFromTypeFullName.l shouldBe List(
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>"
      )
    }
  }

  "two trait impls for the same type" should {
    val cpg = code("""
        |trait Bar { fn a(&self) -> i32; }
        |trait Baz { fn b(&self) -> i32; }
        |struct Foo;
        |impl Bar for Foo { fn a(&self) -> i32 { 1 } }
        |impl Baz for Foo { fn b(&self) -> i32 { 2 } }
        |""".stripMargin)

    "have correct inheritsFromTypeFullName on the struct" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").inheritsFromTypeFullName.l shouldBe List(
        "<rust2cpgtest::Foo as rust2cpgtest::Bar>",
        "<rust2cpgtest::Foo as rust2cpgtest::Baz>"
      )
    }
  }

  "a trait-impl function returning `Self`" should {
    val cpg = code("""
        |trait Bar {
        |  fn make() -> Self;
        |}
        |struct Foo;
        |impl Bar for Foo {
        |  fn make() -> Self { Foo }
        |}
        |""".stripMargin)

    "have correct return typeFullName" in {
      cpg.method
        .fullNameExact("<rust2cpgtest::Foo as rust2cpgtest::Bar>::make")
        .methodReturn
        .typeFullName
        .l shouldBe List("rust2cpgtest::Foo")
    }
  }

  "a call to a trait method" should {
    val cpg = code("""
        |trait Bar { fn do_stuff(&self) -> i32; }
        |struct Foo;
        |impl Bar for Foo { fn do_stuff(&self) -> i32 { 1 } }
        |fn run(f: Foo) { f.do_stuff(); }
        |""".stripMargin)

    "have correct methodFullName" in {
      inside(cpg.call.nameExact("do_stuff").l) { case call :: Nil =>
        call.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "have correct arguments" in {
      inside(cpg.call.nameExact("do_stuff").argument.l) { case (base: Call) :: Nil =>
        base.name shouldBe Operators.addressOf
        base.code shouldBe "&f"
        base.argumentIndex shouldBe 0
        base.typeFullName shouldBe "&rust2cpgtest::Foo"
        inside(base.argument.l) { case (inner: Identifier) :: Nil =>
          inner.name shouldBe "f"
          inner.typeFullName shouldBe "rust2cpgtest::Foo"
        }
      }
    }
  }

  "a call to a trait method via fully-qualified syntax" should {
    val cpg = code("""
        |trait Bar { fn do_stuff(&self) -> i32; }
        |struct Foo;
        |impl Bar for Foo { fn do_stuff(&self) -> i32 { 1 } }
        |fn run(f: Foo) { Foo::do_stuff(&f); }
        |""".stripMargin)

    "have correct methodFullName" in {
      inside(cpg.call.nameExact("do_stuff").l) { case call :: Nil =>
        call.methodFullName shouldBe "<rust2cpgtest::Foo as rust2cpgtest::Bar>::do_stuff"
        call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "have correct arguments" in {
      inside(cpg.call.nameExact("do_stuff").argument.l) { case (base: Call) :: Nil =>
        base.name shouldBe Operators.addressOf
        base.code shouldBe "&*&f"
        base.argumentIndex shouldBe 0
        base.typeFullName shouldBe "&rust2cpgtest::Foo"
        inside(base.argument.l) { case (deref: Call) :: Nil =>
          deref.name shouldBe Operators.indirection
          deref.code shouldBe "*&f"
          deref.typeFullName shouldBe "rust2cpgtest::Foo"
          inside(deref.argument.l) { case (inner: Call) :: Nil =>
            inner.name shouldBe Operators.addressOf
            inner.code shouldBe "&f"
            inner.typeFullName shouldBe "&rust2cpgtest::Foo"
          }
        }
      }
    }
  }
}

class ImplTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "an impl block for the `Default` trait resolved against the sysroot" should {
    val cpg = code("""
        |struct Foo;
        |impl Default for Foo {
        |  fn default() -> Foo { Foo }
        |}
        |fn run() {
        |  Foo::default();
        |}
        |""".stripMargin)

    "have correct fullName for the impl" in {
      inside(cpg.typeDecl.fullNameExact("<rust2cpgtest::Foo as core::default::Default>").l) { case typeDecl :: Nil =>
        typeDecl.inheritsFromTypeFullName shouldBe List("core::default::Default")
        typeDecl.method.fullName.l shouldBe List("<rust2cpgtest::Foo as core::default::Default>::default")
      }
    }

    "have correct inheritsFromTypeFullName on the struct" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::Foo").inheritsFromTypeFullName.l shouldBe List(
        "<rust2cpgtest::Foo as core::default::Default>"
      )
    }

    "create binding node for the method" in {
      inside(cpg.typeDecl.fullNameExact("<rust2cpgtest::Foo as core::default::Default>").methodBinding.l) {
        case binding :: Nil =>
          binding.name shouldBe "default"
          binding.signature shouldBe "core::default::Default"
          binding.methodFullName shouldBe "<rust2cpgtest::Foo as core::default::Default>::default"
      }
    }

    "have correct fulName for the trait call" in {
      inside(cpg.call.nameExact("default").l) { case call :: Nil =>
        call.methodFullName shouldBe "<rust2cpgtest::Foo as core::default::Default>::default"
      }
    }

    "have no arguments" in {
      cpg.call.nameExact("default").argument shouldBe empty
    }
  }
}
