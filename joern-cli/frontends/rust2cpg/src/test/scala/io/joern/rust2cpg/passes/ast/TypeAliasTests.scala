package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*

class TypeAliasTests extends Rust2CpgSuite(noSysRoot = true) {

  "chain of struct aliases" should {
    val cpg = code("""
        |struct Foo { x: i32 }
        |type Bar = Foo;
        |type Baz = Bar;
        |struct Qux { f: Bar }
        |fn do_stuff(a: Bar) -> Bar { a }
        |""".stripMargin)

    "have correct alias typeDecl" in {
      inside(cpg.typeDecl.nameExact("Bar").l) { case bar :: Nil =>
        bar.fullName shouldBe "rust2cpgtest::Bar"
        bar.aliasTypeFullName shouldBe Some("rust2cpgtest::Foo")
        bar.canonicalType.fullName.l shouldBe List("rust2cpgtest::Foo")
      }
    }

    "have correct chained alias typeDecl" in {
      inside(cpg.typeDecl.nameExact("Baz").l) { case baz :: Nil =>
        baz.fullName shouldBe "rust2cpgtest::Baz"
        baz.aliasTypeFullName shouldBe Some("rust2cpgtest::Bar")
        baz.canonicalType.fullName.l shouldBe List("rust2cpgtest::Foo")
      }
    }

    "have correct member typeFullName" in {
      cpg.typeDecl.nameExact("Qux").member.nameExact("f").typeFullName.l shouldBe List("rust2cpgtest::Bar")
    }

    "have correct parameter typeFullName" in {
      cpg.method.nameExact("do_stuff").parameter.nameExact("a").typeFullName.l shouldBe List("rust2cpgtest::Bar")
    }

    "have correct method return typeFullName" in {
      cpg.method.nameExact("do_stuff").methodReturn.typeFullName.l shouldBe List("rust2cpgtest::Bar")
    }
  }

  "generic alias of a tuple" should {
    val cpg = code("""
        |type Foo<T> = (T, T);
        |""".stripMargin)

    "have correct typeDecl" in {
      inside(cpg.typeDecl.nameExact("Foo").l) { case foo :: Nil =>
        foo.aliasTypeFullName shouldBe Some("(T, T)")
      }
    }

    "have correct fullName" in {
      cpg.typeDecl.nameExact("Foo").fullName.l shouldBe List("rust2cpgtest::Foo<T>")
    }
  }

  "alias inside a module" should {
    val cpg = code("""
        |struct Foo { x: i32 }
        |mod bar {
        |  pub type Baz = super::Foo;
        |}
        |""".stripMargin)

    "have correct typeDecl" in {
      inside(cpg.typeDecl.nameExact("Baz").l) { case baz :: Nil =>
        baz.fullName shouldBe "rust2cpgtest::bar::Baz"
        baz.aliasTypeFullName shouldBe Some("rust2cpgtest::Foo")
        baz.canonicalType.fullName.l shouldBe List("rust2cpgtest::Foo")
      }
    }
  }

  "same-named aliases in block-local modules" should {
    val cpg = code("""
        |fn outer() {
        |  {
        |    mod util { pub type Mask = u8; }
        |    let first_mask: util::Mask = 1;
        |  }
        |  {
        |    mod util { pub type Mask = u16; }
        |    let second_mask: util::Mask = 2;
        |  }
        |}
        |""".stripMargin)

    "have correct fullNames" in {
      inside(cpg.typeDecl.nameExact("Mask").sortBy(_.lineNumber).l) { case masku8 :: masku16 :: Nil =>
        masku8.fullName shouldBe "rust2cpgtest::outer::util#1::Mask"
        masku8.aliasTypeFullName shouldBe Some("u8")

        masku16.fullName shouldBe "rust2cpgtest::outer::util#2::Mask"
        masku16.aliasTypeFullName shouldBe Some("u16")
      }
    }
  }
}
