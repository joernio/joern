package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*

class AnnotationTests extends Rust2CpgSuite(noSysRoot = true) {

  "struct with path attribute" should {
    val cpg = code("""
        |#[non_exhaustive]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "non_exhaustive"
        attr.fullName shouldBe "non_exhaustive"
        attr.code shouldBe "#[non_exhaustive]"
      }
    }
  }

  "struct with token tree attribute" should {
    val cpg = code("""
        |#[derive(Debug)]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "derive"
        attr.fullName shouldBe "derive"
        attr.code shouldBe "#[derive(Debug)]"
      }
    }
  }

  "struct with key-value attribute" should {
    val cpg = code("""
        |#[doc = "something"]
        |struct Foo {}
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "doc"
        attr.fullName shouldBe "doc"
        attr.code shouldBe """#[doc = "something"]"""
      }
    }
  }

  "struct with cfg attribute" should {
    val cpg = code("""
        |#[cfg(not(feature = "unstable"))]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "cfg"
        attr.fullName shouldBe "cfg"
        attr.code shouldBe """#[cfg(not(feature = "unstable"))]"""
      }
    }
  }

  "struct with cfg_attr attribute" should {
    val cpg = code("""
        |#[cfg_attr(not(feature = "unstable"), derive(Debug))]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "cfg_attr"
        attr.fullName shouldBe "cfg_attr"
        attr.code shouldBe """#[cfg_attr(not(feature = "unstable"), derive(Debug))]"""
      }
    }
  }

  "struct with unsafe attribute" should {
    val cpg = code("""
        |#[unsafe(no_mangle)]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "unsafe"
        attr.fullName shouldBe "unsafe"
        attr.code shouldBe "#[unsafe(no_mangle)]"
      }
    }
  }

  "struct with qualified attribute path" should {
    val cpg = code("""
        |#[some::attr]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "attr"
        attr.fullName shouldBe "some::attr"
        attr.code shouldBe "#[some::attr]"
      }
    }
  }

  "struct with qualified token tree attribute path" should {
    val cpg = code("""
        |#[some::attr(1)]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "attr"
        attr.fullName shouldBe "some::attr"
        attr.code shouldBe "#[some::attr(1)]"
      }
    }
  }

  "struct with qualified key-value attribute path" should {
    val cpg = code("""
        |#[some::attr = 1]
        |struct Foo;
        |""".stripMargin)

    "have correct annotation" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.l) { case attr :: Nil =>
        attr.name shouldBe "attr"
        attr.fullName shouldBe "some::attr"
        attr.code shouldBe "#[some::attr = 1]"
      }
    }
  }

  "struct with two attributes" should {
    val cpg = code("""
        |#[derive(Debug)]
        |#[doc = "foobar"]
        |struct Foo;
        |""".stripMargin)

    "have correct annotations" in {
      inside(cpg.typeDecl.nameExact("Foo").annotation.sortBy(_.lineNumber).l) { case attr1 :: attr2 :: Nil =>
        attr1.name shouldBe "derive"
        attr1.fullName shouldBe "derive"
        attr1.code shouldBe "#[derive(Debug)]"

        attr2.name shouldBe "doc"
        attr2.fullName shouldBe "doc"
        attr2.code shouldBe """#[doc = "foobar"]"""
      }
    }
  }
}
