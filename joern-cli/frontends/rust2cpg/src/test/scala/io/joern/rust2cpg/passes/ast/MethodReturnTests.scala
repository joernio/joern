package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MethodReturnTests extends Rust2CpgSuite(noSysRoot = true) {

  "an implicit return of a literal" should {
    val cpg = code("""
        |fn main() -> i32 { 1 }
        |""".stripMargin)

    "type the METHOD_RETURN as i32" in {
      inside(cpg.method.name("main").methodReturn.l) { case methodRet :: Nil =>
        methodRet.typeFullName shouldBe "i32"
        methodRet.code shouldBe "RET"
      }
    }

    "lower the body to a single RETURN" in {
      cpg.method.name("main").block.astChildren.isReturn.code.l shouldBe List("1")
    }

    "wrap the literal in the RETURN" in {
      inside(cpg.method.name("main").block.astChildren.isReturn.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "1"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "an explicit return statement" should {
    val cpg = code("""
        |fn main() -> char {
        | return 'x';
        |}
        |""".stripMargin)

    "type the METHOD_RETURN as char" in {
      cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("char")
    }

    "lower the body to a single RETURN" in {
      cpg.method.name("main").block.astChildren.isReturn.code.l shouldBe List("return 'x'")
    }

    "wrap the char literal in the RETURN" in {
      inside(cpg.method.name("main").block.astChildren.isReturn.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "'x'"
        lit.typeFullName shouldBe "char"
      }
    }
  }

  "a parenthesised implicit return" should {
    val cpg = code("""
        |fn foo() -> i32 {
        | (24)
        |}
        |""".stripMargin)

    "type the METHOD_RETURN as i32" in {
      inside(cpg.method.name("foo").methodReturn.l) { case methodRet :: Nil =>
        methodRet.typeFullName shouldBe "i32"
        methodRet.code shouldBe "RET"
      }
    }

    "keep the parens in the RETURN code" in {
      cpg.method.name("foo").block.astChildren.isReturn.code.l shouldBe List("(24)")
    }

    "strip the parens around the literal child" in {
      inside(cpg.method.name("foo").block.astChildren.isReturn.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "24"
        lit.typeFullName shouldBe "i32"
      }
    }
  }
}
