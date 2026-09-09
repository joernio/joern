package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class OffsetTests extends Rust2CpgSuite(noSysRoot = true) {

  "top-level fn" should {
    val cpg = code("""
        |fn main() {
        | let x = 42;
        |}
        |""".stripMargin)

    "have correct offsets on the method" in {
      inside(cpg.method.nameExact("main").l) { case main :: Nil =>
        main.offset shouldBe Some(1)
        main.offsetEnd shouldBe Some(27)
      }
    }

    "have correct offsets on the literal" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        lit.offset shouldBe Some(22)
        lit.offsetEnd shouldBe Some(24)
      }
    }
  }

  "literal with an emoji" should {
    val cpg = code("""fn f() { "🙂"; }""")

    "have correct offsets" in {
      pendingUntilFixed {
        inside(cpg.literal.l) { case lit :: Nil =>
          lit.offset shouldBe Some(9)
          lit.offsetEnd shouldBe Some(13)
        }
      }
    }
  }

  "macro-expanded code" should {
    val cpg = code("""
        |macro_rules! double { ($x:expr) => { $x * 2 }; }
        |fn main() {
        | let single = double!(5);
        |}
        |""".stripMargin)

    "have no offsets" in {
      inside(cpg.call.nameExact(Operators.multiplication).l) { case multiplication :: Nil =>
        multiplication.offset shouldBe None
        multiplication.offsetEnd shouldBe None
      }
    }
  }

}
