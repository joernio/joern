package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends Rust2CpgSuite(noSysRoot = true) {

  "an integer literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 42;
        |}
        |""".stripMargin)

    "lower to an i32 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "42"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "a usize-suffixed integer literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 0usize;
        |}
        |""".stripMargin)

    "lower to a usize Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "0usize"
        lit.typeFullName shouldBe "usize"
      }
    }
  }

  "a u32-suffixed integer literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 1u32;
        |}
        |""".stripMargin)

    "lower to a u32 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "1u32"
        lit.typeFullName shouldBe "u32"
      }
    }
  }

  "an i64-suffixed integer literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 2i64;
        |}
        |""".stripMargin)

    "lower to an i64 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "2i64"
        lit.typeFullName shouldBe "i64"
      }
    }
  }

  "a float literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 1.5;
        |}
        |""".stripMargin)

    "lower to an f64 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "1.5"
        lit.typeFullName shouldBe "f64"
      }
    }
  }

  "an f32-suffixed float literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 3.14f32;
        |}
        |""".stripMargin)

    "lower to an f32 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "3.14f32"
        lit.typeFullName shouldBe "f32"
      }
    }
  }

  "a string literal" should {
    val cpg = code("""
        |fn main() {
        | let s = "hello";
        |}
        |""".stripMargin)

    "lower to a &str Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "\"hello\""
        lit.typeFullName shouldBe "&str"
      }
    }
  }

  "a char literal" should {
    val cpg = code("""
        |fn main() {
        | let c = 'a';
        |}
        |""".stripMargin)

    "lower to a char Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "'a'"
        lit.typeFullName shouldBe "char"
      }
    }
  }

  "a byte literal" should {
    val cpg = code("""
        |fn main() {
        | let b = b'a';
        |}
        |""".stripMargin)

    "lower to a u8 Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "b'a'"
        lit.typeFullName shouldBe "u8"
      }
    }
  }

  "a byte string literal" should {
    val cpg = code("""
        |fn main() {
        | let s = b"hi";
        |}
        |""".stripMargin)

    "lower to a &[u8; 2] Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "b\"hi\""
        lit.typeFullName shouldBe "&[u8; 2]"
      }
    }
  }

  "a C string literal" should {
    val cpg = code("""
        |fn main() {
        | let s = c"hi";
        |}
        |""".stripMargin)

    "lower to a &CStr Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "c\"hi\""
        lit.typeFullName shouldBe "&CStr"
      }
    }
  }

  "a true literal" should {
    val cpg = code("""
        |fn main() {
        | let b = true;
        |}
        |""".stripMargin)

    "lower to a bool Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "true"
        lit.typeFullName shouldBe "bool"
      }
    }
  }

  "a false literal" should {
    val cpg = code("""
        |fn main() {
        | let b = false;
        |}
        |""".stripMargin)

    "lower to a bool Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "false"
        lit.typeFullName shouldBe "bool"
      }
    }
  }

  "a unit value" should {
    val cpg = code("""
        |fn main() {
        | let x = ();
        |}
        |""".stripMargin)

    "lower to a unit Literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.code shouldBe "()"
        lit.typeFullName shouldBe "()"
      }
    }
  }
}
