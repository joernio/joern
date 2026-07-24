package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*

class CfgAttributeTests extends Rust2CpgSuite(noSysRoot = true) {

  "conditional-compilation methods" should {
    val cpg = code("""
        |#[cfg(any())]
        |fn cfg_disabled() {}
        |
        |#[cfg(all())]
        |fn cfg_enabled() {}
        |
        |fn cfg_unconditional() {}
        |""".stripMargin)

    "skip the inactive method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List(
        "rust2cpgtest::cfg_enabled",
        "rust2cpgtest::cfg_unconditional"
      )
    }
  }

  "feature-gated methods" should {
    val cpg = code("""
        |#[cfg(feature = "foo")]
        |fn cfg_enabled() {}
        |
        |#[cfg(feature = "bar")]
        |fn cfg_disabled() {}
        |""".stripMargin)
      .moreCode(
        """
          |[package]
          |name = "rust2cpgtest"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[features]
          |default = ["foo"]
          |foo = []
          |bar = []
          |""".stripMargin,
        fileName = "Cargo.toml"
      )

    "skip the inactive method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List("rust2cpgtest::cfg_enabled")
    }
  }

  "test-gated module" should {
    val cpg = code("""
        |#[cfg(test)]
        |mod tests {
        |    fn cfg_disabled() {}
        |}
        |
        |fn cfg_enabled() {}
        |""".stripMargin)

    "be skipped by default" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List("rust2cpgtest::cfg_enabled")
    }
  }

  "feature-gated modules" should {
    val cpg = code("""
        |#[cfg(feature = "foo")]
        |mod enabled_mod {
        |    fn cfg_enabled() {}
        |}
        |
        |#[cfg(feature = "bar")]
        |mod disabled_mod {
        |    fn cfg_disabled() {}
        |}
        |""".stripMargin)
      .moreCode(
        """
          |[package]
          |name = "rust2cpgtest"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[features]
          |default = ["foo"]
          |foo = []
          |bar = []
          |""".stripMargin,
        fileName = "Cargo.toml"
      )

    "skip the inactive method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List("rust2cpgtest::enabled_mod::cfg_enabled")
    }
  }

  "feature-gated impls" should {
    val cpg = code("""
        |struct Foo;
        |
        |#[cfg(feature = "foo")]
        |impl Foo {
        |    fn cfg_enabled() {}
        |}
        |
        |#[cfg(feature = "bar")]
        |impl Foo {
        |    fn cfg_disabled() {}
        |}
        |""".stripMargin)
      .moreCode(
        """
          |[package]
          |name = "rust2cpgtest"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[features]
          |default = ["foo"]
          |foo = []
          |bar = []
          |""".stripMargin,
        fileName = "Cargo.toml"
      )

    "skip the inactive method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List("rust2cpgtest::Foo::cfg_enabled")
    }
  }

  "feature-gated macro-generated impls" should {
    val cpg = code("""
        |struct Foo;
        |
        |macro_rules! make_impl {
        |    ($name:ident) => {
        |        impl Foo {
        |            fn $name() {}
        |        }
        |    };
        |}
        |
        |#[cfg(feature = "foo")]
        |make_impl!(cfg_enabled);
        |
        |#[cfg(feature = "bar")]
        |make_impl!(cfg_disabled);
        |""".stripMargin)
      .moreCode(
        """
          |[package]
          |name = "rust2cpgtest"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[features]
          |default = ["foo"]
          |foo = []
          |bar = []
          |""".stripMargin,
        fileName = "Cargo.toml"
      )

    "skip the inactive method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List("rust2cpgtest::Foo::cfg_enabled")
    }
  }
}

class CfgAttributeTestsWithoutResolution extends Rust2CpgSuite(noSysRoot = true, noResolveCfg = true) {

  "conditional-compilation methods without cfg resolution" should {
    val cpg = code("""
        |#[cfg(any())]
        |fn cfg_disabled() {}
        |
        |#[cfg(all())]
        |fn cfg_enabled() {}
        |
        |fn cfg_unconditional() {}
        |""".stripMargin)

    "not skip any method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List(
        "rust2cpgtest::cfg_disabled",
        "rust2cpgtest::cfg_enabled",
        "rust2cpgtest::cfg_unconditional"
      )
    }
  }

  "feature-gated methods without cfg resolution" should {
    val cpg = code("""
        |#[cfg(feature = "foo")]
        |fn cfg_enabled() {}
        |
        |#[cfg(feature = "bar")]
        |fn cfg_disabled() {}
        |""".stripMargin)
      .moreCode(
        """
          |[package]
          |name = "rust2cpgtest"
          |version = "0.1.0"
          |edition = "2021"
          |
          |[features]
          |default = ["foo"]
          |foo = []
          |bar = []
          |""".stripMargin,
        fileName = "Cargo.toml"
      )

    "not skip any method" in {
      cpg.method.name("cfg.*").fullName.sorted.l shouldBe List(
        "rust2cpgtest::cfg_disabled",
        "rust2cpgtest::cfg_enabled"
      )
    }
  }
}
