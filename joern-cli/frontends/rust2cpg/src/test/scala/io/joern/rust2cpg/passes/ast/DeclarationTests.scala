package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal.globalNamespaceName

class DeclarationTests extends Rust2CpgSuite(noSysRoot = true) {

  "top-level `const` emits a LOCAL and an assignment in the file's `<global>` method" in {
    val cpg = code("const MAX_SIZE: usize = 1024;")

    inside(cpg.method.name(globalNamespaceName).block.local.name("MAX_SIZE").l) { case local :: Nil =>
      local.typeFullName shouldBe "usize"
      local.code shouldBe "MAX_SIZE"
    }

    inside(cpg.method.name(globalNamespaceName).block.assignment.l) { case assignment :: Nil =>
      assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignment.code shouldBe "const MAX_SIZE: usize = 1024;"
      assignment.typeFullName shouldBe Defines.Any
      assignment.methodFullName shouldBe Operators.assignment
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "MAX_SIZE"
      lhs.typeFullName shouldBe "usize"
      lhs.code shouldBe "MAX_SIZE"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "1024"
      rhs.typeFullName shouldBe "usize"
    }
  }

  "`const` in a function's body emits a LOCAL and an assignment" in {
    val cpg = code("""
        |fn main() {
        | const FOO: i32 = 0;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("FOO").l) { case local :: Nil =>
      local.typeFullName shouldBe "i32"
      local.code shouldBe "FOO"
    }

    inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
      assignment.code shouldBe "const FOO: i32 = 0;"
      assignment.lineNumber shouldBe Some(3)
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "FOO"
      lhs.typeFullName shouldBe "i32"
      lhs.code shouldBe "FOO"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "0"
      rhs.typeFullName shouldBe "i32"
    }
  }

  "untyped integer literal `let` emits an i32 LOCAL and an assignment" in {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe "i32"
      local.code shouldBe "x"
    }

    inside(cpg.method.name("main").block.assignment.l) { case assignment :: Nil =>
      assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      assignment.code shouldBe "let x = 1;"
      assignment.typeFullName shouldBe Defines.Any
      assignment.methodFullName shouldBe Operators.assignment
      assignment.lineNumber shouldBe Some(3)
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "x"
      lhs.typeFullName shouldBe "i32"
      lhs.code shouldBe "x"
    }

    inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
      rhs.code shouldBe "1"
      rhs.typeFullName shouldBe "i32"
    }
  }

  "typed `let` emits a LOCAL of that type and an assignment whose RHS' type is the same" in {
    val cpg = code("""
        |fn foo() {
        | let x: usize = 10;
        |}
        |""".stripMargin)

    inside(cpg.method.name("foo").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe "usize"
      local.code shouldBe "x"
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "x"
      lhs.typeFullName shouldBe "usize"
      lhs.code shouldBe "x"
    }

    // rust-analyzer sets the RHS type according to the type annotation provided.
    // Otherwise, 10 alone would be i32.
    inside(cpg.assignment.argument(2).l) { case (lit: Literal) :: Nil =>
      lit.code shouldBe "10"
      lit.typeFullName shouldBe "usize"
    }
  }

  "`true/false` literals have typeFullName `bool`" in {
    val cpg = code("const TT: bool = true; const FF: bool = false;")

    cpg.literal.code("true").typeFullName.l shouldBe List("bool")
    cpg.literal.code("false").typeFullName.l shouldBe List("bool")
  }

  "untyped uninitialized `let` emits LOCAL (with ANY type) but no assignment" in {
    val cpg = code("""
        |fn main() {
        | let x;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe Defines.Any
      local.code shouldBe "x"
    }

    cpg.method.name("main").block.assignment.l shouldBe Nil
  }

  "typed uninitialized `let` emits LOCAL (with corresponding type) but no assignment" in {
    val cpg = code("""
        |fn main() {
        | let x: i32;
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("x").l) { case local :: Nil =>
      local.typeFullName shouldBe "i32"
      local.code shouldBe "x"
    }

    cpg.method.name("main").block.assignment.l shouldBe Nil
  }

  "untyped `let` for a string literal has type `&str`" in {
    val cpg = code("""
        |fn main() {
        | let s = "hello";
        |}
        |""".stripMargin)

    inside(cpg.method.name("main").block.local.name("s").l) { case local :: Nil =>
      local.typeFullName shouldBe "&str"
    }

    inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
      lhs.name shouldBe "s"
      lhs.typeFullName shouldBe "&str"
    }
  }
}
