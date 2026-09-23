package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.Config
import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class OffsetTests extends Rust2CpgSuite(noSysRoot = true) {

  private val contentEnabled = Config().withNoSysRoot(true).withDisableFileContent(false)

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
    val cpg = code("""fn f() { "🙂"; }""").withConfig(contentEnabled)

    "have correct offsets" in {
      inside(cpg.literal.l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "\"🙂\""
      }
    }
  }

  "2-byte UTF-8 char (é) shifts subsequent offsets" should {
    val cpg = code(
      """fn f() {
        | let café = 1;
        | let next = 2;
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after multi-byte char" in {
      inside(cpg.literal.code("2").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "2"
      }
    }
  }

  "3-byte UTF-8 chars (中文) shift subsequent offsets" should {
    val cpg = code(
      """fn f() {
        | let s = "中文";
        | let x = 42;
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after CJK chars" in {
      inside(cpg.literal.code("42").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "42"
      }
    }
  }

  "4-byte UTF-8 char (🎉) in comment shifts subsequent offsets" should {
    val cpg = code(
      """// 🎉
        |fn f() {
        | let x = 1;
        |}
        |""".stripMargin
    ).withConfig(contentEnabled)

    "have correct offset for literal after emoji" in {
      inside(cpg.literal.code("1").l) { case lit :: Nil =>
        val fileContent = cpg.file.content.head
        fileContent.substring(lit.offset.get, lit.offsetEnd.get) shouldBe "1"
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
