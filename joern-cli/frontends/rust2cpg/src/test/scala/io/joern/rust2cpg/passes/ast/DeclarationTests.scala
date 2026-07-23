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

  "a top-level static" should {
    val cpg = code("static BAR: usize = 42;")

    "create a LOCAL with the annotated type" in {
      cpg.local.name("BAR").typeFullName.l shouldBe List("usize")
    }

    "lower the initializer into an assignment" in {
      inside(cpg.assignment.l) { case assignment :: Nil =>
        assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        assignment.code shouldBe "static BAR: usize = 42;"
        assignment.typeFullName shouldBe Defines.Any
        assignment.methodFullName shouldBe Operators.assignment
      }
    }

    "have an Identifier as the assignment's first argument" in {
      inside(cpg.assignment.argument(1).l) { case (lhs: Identifier) :: Nil =>
        lhs.name shouldBe "BAR"
        lhs.typeFullName shouldBe "usize"
        lhs.code shouldBe "BAR"
      }
    }

    "have a Literal as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: Literal) :: Nil =>
        rhs.code shouldBe "42"
        rhs.typeFullName shouldBe "usize"
      }
    }
  }

  "a static inside a function body" should {
    val cpg = code("""
        |fn main() {
        | static FOO: i32 = 0;
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
        assignment.code shouldBe "static FOO: i32 = 0;"
        assignment.lineNumber shouldBe Some(3)
      }
    }
  }

  "a mutable static" should {
    val cpg = code("static mut BAZ: u32 = 0;")

    "create a LOCAL with the annotated type" in {
      inside(cpg.local.name("BAZ").l) { case local :: Nil =>
        local.typeFullName shouldBe "u32"
        local.code shouldBe "BAZ"
      }
    }

    "lower the initializer into an assignment" in {
      inside(cpg.assignment.l) { case assignment :: Nil =>
        assignment.code shouldBe "static mut BAZ: u32 = 0;"
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
  }

  "`let (a, b): (i32, bool);`" should {
    val cpg = code("""
        |fn main() {
        | let (a, b): (i32, bool);
        |}
        |""".stripMargin)

    "have a LOCAL for each binding" in {
      inside(cpg.method.name("main").block.local.l) { case aLocal :: bLocal :: Nil =>
        aLocal.name shouldBe "a"
        aLocal.typeFullName shouldBe "i32"
        aLocal.code shouldBe "a"

        bLocal.name shouldBe "b"
        bLocal.typeFullName shouldBe "bool"
        bLocal.code shouldBe "b"
      }
    }
  }

  "`let Point { x, y };`" should {
    val cpg = code("""
        |struct Point { x: i32, y: bool }
        |fn main() {
        | let Point { x, y };
        |}
        |""".stripMargin)

    "have a LOCAL for each binding" in {
      inside(cpg.method.name("main").block.local.l) { case xLocal :: yLocal :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
        xLocal.code shouldBe "x"

        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "bool"
        yLocal.code shouldBe "y"
      }
    }
  }

  "`let Point { x: a, y };`" should {
    val cpg = code("""
        |struct Point { x: i32, y: bool }
        |fn main() {
        | let Point { x: a, y };
        |}
        |""".stripMargin)

    "have a LOCAL for each binding, named after the binding rather than the field" in {
      inside(cpg.method.name("main").block.local.l) { case aLocal :: yLocal :: Nil =>
        aLocal.name shouldBe "a"
        aLocal.typeFullName shouldBe "i32"
        aLocal.code shouldBe "a"

        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "bool"
        yLocal.code shouldBe "y"
      }
    }
  }

  "`let Foo(a);`" should {
    val cpg = code("""
        |struct Foo(i32);
        |fn main() {
        | let Foo(a);
        |}
        |""".stripMargin)

    "have a LOCAL i32" in {
      inside(cpg.method.name("main").block.local.name("a").l) { case local :: Nil =>
        local.typeFullName shouldBe "i32"
        local.code shouldBe "a"
      }
    }
  }

  "`let [a, b]: [i32; 2];`" should {
    val cpg = code("""
        |fn main() {
        | let [a, b]: [i32; 2];
        |}
        |""".stripMargin)

    "have a LOCAL for each binding" in {
      inside(cpg.method.name("main").block.local.l) { case aLocal :: bLocal :: Nil =>
        aLocal.name shouldBe "a"
        aLocal.typeFullName shouldBe "i32"
        aLocal.code shouldBe "a"
        bLocal.name shouldBe "b"
        bLocal.typeFullName shouldBe "i32"
        bLocal.code shouldBe "b"
      }
    }
  }

  "untyped let with `@`" should {
    val cpg = code("""
        |fn main() {
        | let a @ b = 1;
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp and each binding" in {
      inside(cpg.method.name("main").block.astChildren.l) {
        case (tmpLocal: Local) :: (aLocal: Local) :: (bLocal: Local) :: (tmpAssign: Call) ::
            (aAssign: Call) :: (bAssign: Call) :: Nil =>
          tmpLocal.name shouldBe "tmp"
          tmpLocal.typeFullName shouldBe "i32"
          tmpAssign.code shouldBe "tmp = 1"

          aLocal.name shouldBe "a"
          aLocal.typeFullName shouldBe "i32"
          aAssign.code shouldBe "a = tmp"

          bLocal.name shouldBe "b"
          bLocal.typeFullName shouldBe "i32"
          bAssign.code shouldBe "b = tmp"
      }
    }

    "lower the initializer into an assignment to tmp" in {
      inside(cpg.assignment.codeExact("tmp = 1").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Literal) :: Nil =>
          lhs.name shouldBe "tmp"
          lhs.typeFullName shouldBe "i32"
          rhs.code shouldBe "1"
          rhs.typeFullName shouldBe "i32"
      }
    }

    "lower each binding as a tmp assignment" in {
      inside(cpg.assignment.codeExact("a = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "a"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "tmp"
      }
      inside(cpg.assignment.codeExact("b = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "b"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "tmp"
      }
    }
  }

  "typed let with `@`" should {
    val cpg = code("""
        |fn main() {
        | let a @ b: u8 = 1;
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp and each binding" in {
      inside(cpg.method.name("main").block.astChildren.l) {
        case (tmpLocal: Local) :: (aLocal: Local) :: (bLocal: Local) :: (tmpAssign: Call) ::
            (aAssign: Call) :: (bAssign: Call) :: Nil =>
          tmpLocal.name shouldBe "tmp"
          tmpLocal.typeFullName shouldBe "u8"
          tmpAssign.code shouldBe "tmp = 1"

          aLocal.name shouldBe "a"
          aLocal.typeFullName shouldBe "u8"
          aAssign.code shouldBe "a = tmp"

          bLocal.name shouldBe "b"
          bLocal.typeFullName shouldBe "u8"
          bAssign.code shouldBe "b = tmp"
      }
    }

    "lower the initializer into an assignment to tmp" in {
      inside(cpg.assignment.codeExact("tmp = 1").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Literal) :: Nil =>
          lhs.name shouldBe "tmp"
          lhs.typeFullName shouldBe "u8"
          rhs.code shouldBe "1"
          rhs.typeFullName shouldBe "u8"
      }
    }

    "lower each binding as a tmp assignment" in {
      inside(cpg.assignment.codeExact("a = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "a"
          lhs.typeFullName shouldBe "u8"
          rhs.name shouldBe "tmp"
      }
      inside(cpg.assignment.codeExact("b = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "b"
          lhs.typeFullName shouldBe "u8"
          rhs.name shouldBe "tmp"
      }
    }
  }

  "let with _" should {
    val cpg = code("""
        |fn main() {
        | let _ = 1;
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp" in {
      inside(cpg.method.name("main").block.astChildren.l) { case (tmpLocal: Local) :: (tmpAssign: Call) :: Nil =>
        tmpLocal.name shouldBe "tmp"
        tmpLocal.typeFullName shouldBe "i32"
        tmpAssign.code shouldBe "tmp = 1"
      }
    }

    "lower the initializer into an assignment to tmp" in {
      inside(cpg.assignment.codeExact("tmp = 1").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Literal) :: Nil =>
          lhs.name shouldBe "tmp"
          lhs.typeFullName shouldBe "i32"
          rhs.code shouldBe "1"
          rhs.typeFullName shouldBe "i32"
      }
    }
  }

  "let with nested `@`" should {
    val cpg = code("""
        |fn foo() -> i32 { 1 }
        |fn main() {
        | let x @ (z @ _) = foo();
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp and each binding" in {
      inside(cpg.method.name("main").block.astChildren.l) {
        case (tmpLocal: Local) :: (xLocal: Local) :: (zLocal: Local) :: (tmpAssign: Call) ::
            (xAssign: Call) :: (zAssign: Call) :: Nil =>
          tmpLocal.name shouldBe "tmp"
          tmpLocal.typeFullName shouldBe "i32"
          tmpAssign.code shouldBe "tmp = foo()"

          xLocal.name shouldBe "x"
          xLocal.typeFullName shouldBe "i32"
          xAssign.code shouldBe "x = tmp"

          zLocal.name shouldBe "z"
          zLocal.typeFullName shouldBe "i32"
          zAssign.code shouldBe "z = tmp"
      }
    }

    "lower the initializer into an assignment to tmp" in {
      inside(cpg.assignment.codeExact("tmp = foo()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "tmp"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "foo"
          rhs.methodFullName shouldBe "rust2cpgtest::foo"
          rhs.typeFullName shouldBe "i32"
      }
    }

    "lower each binding as a tmp assignment" in {
      inside(cpg.assignment.codeExact("x = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "tmp"
      }
      inside(cpg.assignment.codeExact("z = tmp").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Identifier) :: Nil =>
          lhs.name shouldBe "z"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "tmp"
      }
    }
  }

  "let with a tuple pattern" should {
    val cpg = code("""
        |fn main() {
        | let (x, y) = (1, "a");
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp and each binding" in {
      inside(cpg.method.name("main").block.astChildren.l) {
        case (tmpLocal: Local) :: (xLocal: Local) :: (yLocal: Local) :: (tmpAssign: Call) ::
            (xAssign: Call) :: (yAssign: Call) :: Nil =>
          tmpLocal.name shouldBe "tmp"
          tmpLocal.typeFullName shouldBe "(i32, &str)"
          tmpAssign.code shouldBe "tmp = (1, \"a\")"

          xLocal.name shouldBe "x"
          xLocal.typeFullName shouldBe "i32"
          xAssign.code shouldBe "x = tmp.0"

          yLocal.name shouldBe "y"
          yLocal.typeFullName shouldBe "&str"
          yAssign.code shouldBe "y = tmp.1"
      }
    }

    "lower the initializer into an assignment to tmp" in {
      inside(cpg.assignment.codeExact("tmp = (1, \"a\")").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "tmp"
          lhs.typeFullName shouldBe "(i32, &str)"
          rhs.name shouldBe "<operator>.tupleLiteral"
          rhs.typeFullName shouldBe "(i32, &str)"
      }
    }

    "lower each binding as an assignment of a tmp field access" in {
      inside(cpg.assignment.codeExact("x = tmp.0").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe Operators.fieldAccess
          rhs.typeFullName shouldBe "i32"
          inside(rhs.argument.sortBy(_.argumentIndex).l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
            base.name shouldBe "tmp"
            field.canonicalName shouldBe "0"
          }
      }
      inside(cpg.assignment.codeExact("y = tmp.1").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "y"
          lhs.typeFullName shouldBe "&str"
          rhs.name shouldBe Operators.fieldAccess
          rhs.typeFullName shouldBe "&str"
          inside(rhs.argument.sortBy(_.argumentIndex).l) { case (base: Identifier) :: (field: FieldIdentifier) :: Nil =>
            base.name shouldBe "tmp"
            field.canonicalName shouldBe "1"
          }
      }
    }
  }

  "let with a tuple pattern with `_`" should {
    val cpg = code("""
        |fn main() {
        | let (x, _) = (1, 2);
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for the single binding" in {
      inside(cpg.method.name("main").block.astChildren.l) { case (xLocal: Local) :: (xAssign: Call) :: Nil =>
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
        xAssign.code shouldBe "x = (1, 2).0"
      }
    }

    "lower the binding as an assignment of a field access on the initializer" in {
      inside(cpg.assignment.codeExact("x = (1, 2).0").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe Operators.fieldAccess
          rhs.typeFullName shouldBe "i32"
          inside(rhs.argument.sortBy(_.argumentIndex).l) { case (base: Call) :: (field: FieldIdentifier) :: Nil =>
            base.name shouldBe "<operator>.tupleLiteral"
            base.code shouldBe "(1, 2)"
            field.canonicalName shouldBe "0"
          }
      }
    }
  }

  "let with a nested tuple pattern" should {
    val cpg = code("""
        |fn main() {
        | let ((a, b), c) = ((1, 2), 3);
        |}
        |""".stripMargin)

    "lower into a LOCAL and an assignment for tmp and each binding" in {
      inside(cpg.method.name("main").block.astChildren.l) {
        case (tmpLocal: Local) :: (aLocal: Local) :: (bLocal: Local) :: (cLocal: Local) ::
            (tmpAssign: Call) :: (aAssign: Call) :: (bAssign: Call) :: (cAssign: Call) :: Nil =>
          tmpLocal.name shouldBe "tmp"
          tmpLocal.typeFullName shouldBe "((i32, i32), i32)"
          tmpAssign.code shouldBe "tmp = ((1, 2), 3)"

          aLocal.name shouldBe "a"
          aLocal.typeFullName shouldBe "i32"
          aAssign.code shouldBe "a = tmp.0.0"

          bLocal.name shouldBe "b"
          bLocal.typeFullName shouldBe "i32"
          bAssign.code shouldBe "b = tmp.0.1"

          cLocal.name shouldBe "c"
          cLocal.typeFullName shouldBe "i32"
          cAssign.code shouldBe "c = tmp.1"
      }
    }
  }

  "let to a function" should {
    val cpg = code("""
        |fn handler(x: i64) -> i64 { x }
        |fn main() {
        | let f = handler;
        |}
        |""".stripMargin)

    "have a MethodRef as the assignment's second argument" in {
      inside(cpg.assignment.argument(2).l) { case (rhs: MethodRef) :: Nil =>
        rhs.code shouldBe "handler"
        rhs.methodFullName shouldBe "rust2cpgtest::handler"
        rhs.typeFullName shouldBe "rust2cpgtest::handler"
      }
    }
  }
}
