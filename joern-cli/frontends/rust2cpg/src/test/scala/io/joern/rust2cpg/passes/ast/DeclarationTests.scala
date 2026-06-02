package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class DeclarationTests extends Rust2CpgSuite(noSysRoot = true) {

  "a top-level const" should {
    val cpg = code("const MAX_SIZE: usize = 1024;")

    "create a LOCAL with the annotated type" in {
      cpg.local.name("MAX_SIZE").typeFullName.l shouldBe List("usize")
    }

    "lower the initializer into an assignment" in {
      inside(cpg.assignment.l) { case assignment :: Nil =>
        assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        assignment.code shouldBe "const MAX_SIZE: usize = 1024;"
        assignment.typeFullName shouldBe Defines.Any
        assignment.methodFullName shouldBe Operators.assignment
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "MAX_SIZE"
        lhs.typeFullName shouldBe "usize"
        lhs.code shouldBe "MAX_SIZE"
      }
    }

    "have a Literal as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
        rhs.code shouldBe "1024"
        rhs.typeFullName shouldBe "usize"
      }
    }
  }

  "a const inside a function body" should {
    val cpg = code("""
        |fn main() {
        | const FOO: i32 = 0;
        |}
        |""".stripMargin)

    "create a LOCAL with the annotated type" in {
      inside(cpg.method.name("main").block.local.name("FOO").l) { case local :: Nil =>
        local.typeFullName shouldBe "i32"
        local.code shouldBe "FOO"
      }
    }

    "lower the initializer into an assignment" in {
      inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
        assignment.code shouldBe "const FOO: i32 = 0;"
        assignment.lineNumber shouldBe Some(3)
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "FOO"
        lhs.typeFullName shouldBe "i32"
        lhs.code shouldBe "FOO"
      }
    }

    "have a Literal as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
        rhs.code shouldBe "0"
        rhs.typeFullName shouldBe "i32"
      }
    }
  }

  "an untyped let bound to an integer literal" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        |}
        |""".stripMargin)

    "create an i32 LOCAL" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe "i32"
        local.code shouldBe "x"
      }
    }

    "lower the initializer into an assignment" in {
      inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
        assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        assignment.code shouldBe "let x = 1;"
        assignment.typeFullName shouldBe Defines.Any
        assignment.methodFullName shouldBe Operators.assignment
        assignment.lineNumber shouldBe Some(3)
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "x"
        lhs.typeFullName shouldBe "i32"
        lhs.code shouldBe "x"
      }
    }

    "have a Literal as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
        rhs.code shouldBe "1"
        rhs.typeFullName shouldBe "i32"
      }
    }
  }

  "a typed let bound to an integer literal" should {
    val cpg = code("""
        |fn foo() {
        | let x: usize = 10;
        |}
        |""".stripMargin)

    "create a LOCAL with the annotated type" in {
      inside(cpg.method.name("foo").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe "usize"
        local.code shouldBe "x"
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "x"
        lhs.typeFullName shouldBe "usize"
        lhs.code shouldBe "x"
      }
    }

    // rust-analyzer sets the RHS type according to the type annotation provided.
    // Otherwise, 10 alone would be i32.
    "have a Literal as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (lit: Literal) :: Nil =>
        lit.code shouldBe "10"
        lit.typeFullName shouldBe "usize"
      }
    }
  }

  "boolean literals" should {
    val cpg = code("const TT: bool = true; const FF: bool = false;")

    "have a bool typeFullName for true" in {
      cpg.literal.code("true").typeFullName.l shouldBe List("bool")
    }

    "have a bool typeFullName for false" in {
      cpg.literal.code("false").typeFullName.l shouldBe List("bool")
    }
  }

  "an untyped let bound to a string literal" should {
    val cpg = code("""
        |fn main() {
        | let s = "hello";
        |}
        |""".stripMargin)

    "create an &str LOCAL" in {
      inside(cpg.method.name("main").block.local.name("s").l) { case local :: Nil =>
        local.typeFullName shouldBe "&str"
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "s"
        lhs.typeFullName shouldBe "&str"
      }
    }
  }

  "`let x: i32;`" should {
    val cpg = code("""
        |fn main() {
        | let x: i32;
        |}
        |""".stripMargin)

    "have a LOCAL i32" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe "i32"
        local.code shouldBe "x"
      }
    }
  }

  "`let x;`" should {
    val cpg = code("""
        |fn main() {
        | let x;
        |}
        |""".stripMargin)

    "have a LOCAL Any" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe Defines.Any
        local.code shouldBe "x"
      }
    }
  }

  "`let x;` followed by `x = 5;`" should {
    val cpg = code("""
        |fn main() {
        | let x;
        | x = 5;
        |}
        |""".stripMargin)

    "have a LOCAL i32" in {
      inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
        local.typeFullName shouldBe "i32"
        local.code shouldBe "x"
      }
    }

    "have the LHS of the assignment reference the declaration" in {
      cpg.method("main").assignment.target.isIdentifier.name("x").refsTo.l shouldBe cpg.method("main").local.name("x").l
    }
  }
}
