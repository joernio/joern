package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MethodTests extends Rust2CpgSuite(noSysRoot = true) {

  "`main` lives under the crate namespace" in {
    val cpg = code("""
        |fn main() {}
        |""".stripMargin)

    inside(cpg.method.name("main").l) { case main :: Nil =>
      main.fullName shouldBe "rust2cpgtest::main"
      main.astParentFullName shouldBe "rust2cpgtest"
      main.astParentType shouldBe NodeTypes.NAMESPACE_BLOCK
      main.parameter shouldBe empty
      main.methodReturn.typeFullName shouldBe "()"
      main.block.typeFullName shouldBe Defines.Any
      main.block.astChildren shouldBe empty
    }
  }

  "tail expression lowers to a RETURN under the method block" in {
    val cpg = code("""
        |fn main() -> i32 { 1 }
        |""".stripMargin)

    inside(cpg.method.name("main").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("main").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "1"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "1"
        lit.typeFullName shouldBe "i32"
      }
    }
  }

  "explicit `return` statement lowers as a RETURN node" in {
    val cpg = code("""
        |fn main() -> char {
        | return 'x';
        |}
        |""".stripMargin)

    cpg.method.name("main").methodReturn.typeFullName.l shouldBe List("char")

    inside(cpg.method.name("main").block.astChildren.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "return 'x'"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "'x'"
        lit.typeFullName shouldBe "char"
      }
    }
  }

  "freestanding single parameter function has correct properties" in {
    val cpg = code("""
        |fn id(x: i32) -> i32 {
        | x
        |}
        |""".stripMargin)

    inside(cpg.method.name("id").l) { case (method: Method) :: Nil =>
      method.fullName shouldBe "rust2cpgtest::id"
    }

    inside(cpg.method.name("id").parameter.sortBy(_.order).l) { case (param: MethodParameterIn) :: Nil =>
      param.name shouldBe "x"
      param.index shouldBe 1
      param.typeFullName shouldBe "i32"
    }

    inside(cpg.method.name("id").block.astChildren.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "x"

      inside(ret.astChildren.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.code shouldBe "x"
        ident.typeFullName shouldBe "i32"
      }
    }
  }

  "freestanding function's parameter start at index 1" in {
    val cpg = code("""
        |fn foo(p1: i32, p2: i64, p3: f32) {}
        |""".stripMargin)

    inside(cpg.method.name("foo").l) { case (method: Method) :: Nil =>
      method.fullName shouldBe "rust2cpgtest::foo"
    }

    inside(cpg.method.name("foo").parameter.sortBy(_.order).l) { case p1 :: p2 :: p3 :: Nil =>
      p1.name shouldBe "p1"
      p1.index shouldBe 1
      p1.typeFullName shouldBe "i32"

      p2.name shouldBe "p2"
      p2.index shouldBe 2
      p2.typeFullName shouldBe "i64"

      p3.name shouldBe "p3"
      p3.index shouldBe 3
      p3.typeFullName shouldBe "f32"
    }

    cpg.method.name("foo").block.astChildren shouldBe empty
  }

  "a parenthesised tail expression is correctly lowered" in {
    val cpg = code("""
        |fn foo() -> i32 {
        | (24)
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").methodReturn.l) { case (methodRet: MethodReturn) :: Nil =>
      methodRet.typeFullName shouldBe "i32"
      methodRet.code shouldBe "RET"
    }

    inside(cpg.method.name("foo").block.astChildren.isReturn.l) { case (ret: Return) :: Nil =>
      ret.code shouldBe "(24)"

      inside(ret.astChildren.l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "24"
        lit.typeFullName shouldBe "i32"
      }
    }
  }
}
