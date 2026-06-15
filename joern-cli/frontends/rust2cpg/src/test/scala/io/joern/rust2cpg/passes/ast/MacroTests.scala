package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class MacroTests extends Rust2CpgSuite(noSysRoot = true) {

  "an expression macro" should {
    val cpg = code("""
        |macro_rules! double { ($x:expr) => { $x * 2 }; }
        |macro_rules! quad { ($x:expr) => { double!($x) + double!($x) }; }
        |fn main() {
        | let single = double!(5);
        | let nested = quad!(3);
        |}
        |""".stripMargin)

    "lower to the expanded multiplication call" in {
      inside(cpg.assignment.codeExact("let single = double!(5);").source.l) { case (mul: Call) :: Nil =>
        mul.name shouldBe Operators.multiplication
        mul.code shouldBe "5*2"
        mul.typeFullName shouldBe "i32"
        mul.argument.isLiteral.sortBy(_.argumentIndex).code.l shouldBe List("5", "2")
      }
    }

    "expand nested macro calls recursively" in {
      inside(cpg.assignment.codeExact("let nested = quad!(3);").source.l) { case (add: Call) :: Nil =>
        add.name shouldBe Operators.addition
        add.argument.isCall.nameExact(Operators.multiplication).code.l shouldBe List("3*2", "3*2")
      }
    }

    "have a line number for the let statement but none for the expansion" in {
      cpg.assignment.codeExact("let single = double!(5);").lineNumber.l shouldBe List(5)
      cpg.call.nameExact(Operators.multiplication).lineNumber.l shouldBe empty
    }
  }

  "a statement macro expanding to multiple statements" should {
    val cpg = code("""
        |macro_rules! two_lets { ($a:expr) => { let p = $a; let q = p + 1; }; }
        |fn main() {
        | two_lets!(4);
        |}
        |""".stripMargin)

    "create a local for each expanded let" in {
      cpg.method.name("main").block.local.name.l shouldBe List("p", "q")
    }

    "have the macro argument as the assignment source of `p`" in {
      inside(cpg.method.name("main").block.assignment.where(_.target.isIdentifier.nameExact("p")).source.l) {
        case (lit: Literal) :: Nil =>
          lit.code shouldBe "4"
          lit.typeFullName shouldBe "i32"
      }
    }

    "have the addition as the assignment source of `q`" in {
      inside(cpg.method.name("main").block.assignment.where(_.target.isIdentifier.nameExact("q")).source.l) {
        case (add: Call) :: Nil =>
          add.name shouldBe Operators.addition
          add.argument.sortBy(_.argumentIndex).code.l shouldBe List("p", "1")
      }
    }
  }

  "a type macro" should {
    val cpg = code("""
        |macro_rules! int_type { () => { i128 }; }
        |fn main() {
        | let x: int_type!() = 1;
        |}
        |""".stripMargin)

    "expand in type position for the local" in {
      cpg.local.name("x").typeFullName.l shouldBe List("i128")
    }

    "expand in type position for the assignment target" in {
      cpg.assignment.codeExact("let x: int_type!() = 1;").target.isIdentifier.typeFullName.l shouldBe List("i128")
    }
  }

  "a module-level item macro" should {
    val cpg = code("""
        |macro_rules! make_fn { () => { fn generated() -> i32 { 42 } }; }
        |macro_rules! unused { () => { fn never() {} }; }
        |make_fn!();
        |""".stripMargin)

    "expand to the generated method" in {
      inside(cpg.method.name("generated").l) { case generated :: Nil =>
        generated.fullName shouldBe "rust2cpgtest::generated"
        generated.methodReturn.typeFullName shouldBe "i32"
        generated.block.astChildren.isReturn.code.l shouldBe List("42")
      }
    }

    "create no method for the uninvoked macro" in {
      cpg.method.name("never") shouldBe empty
    }

    "create no Unknown nodes" in {
      cpg.all.collectAll[Unknown] shouldBe empty
    }
  }
}

class MacroTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "a `vec!` macro resolved against the sysroot" should {
    val cpg = code("""
        |fn main() {
        | let v = vec![1, 2, 3];
        |}
        |""".stripMargin)

    "type the local as alloc::vec::Vec" in {
      cpg.local.name("v").typeFullName.l shouldBe List("alloc::vec::Vec<i32, alloc::alloc::Global>")
    }

    "have the expanded constructor call as the assignment source" in {
      inside(cpg.assignment.codeExact("let v = vec![1, 2, 3];").source.l) { case (rhs: Call) :: Nil =>
        rhs.typeFullName shouldBe "alloc::vec::Vec<i32, alloc::alloc::Global>"
      }
    }

    "lower the elements to an arrayInitializer inside the expanded constructor" in {
      inside(cpg.assignment.codeExact("let v = vec![1, 2, 3];").source.l) { case (rhs: Call) :: Nil =>
        // The specific methods that vec! expands to are not "public", in the sense they can change
        // depending on the sysroot/platform. But the arrayInitializer should be safe to assert.
        inside(rhs.astChildren.isCall.astChildren.isCall.nameExact(Operators.arrayInitializer).l) { case array :: Nil =>
          array.code shouldBe "[1,2,3]"
          array.typeFullName shouldBe "[i32; 3]"
          array.argument.isLiteral.sortBy(_.argumentIndex).code.l shouldBe List("1", "2", "3")
        }
      }
    }
  }

  "a `format!` macro resolved against the sysroot" should {
    val cpg = code("""
        |fn main() {
        | let x = 7;
        | let s = format!("{} {}", x, 3);
        |}
        |""".stripMargin)

    "lower to a formatString call" in {
      inside(cpg.call.nameExact(Operators.formatString).l) { case format :: Nil =>
        format.methodFullName shouldBe Operators.formatString
        format.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        format.typeFullName shouldBe "core::fmt::Arguments<'a>"
      }
    }

    "have the template and the formattedValues as arguments" in {
      inside(cpg.call.nameExact(Operators.formatString).argument.sortBy(_.argumentIndex).l) {
        case (lit: Literal) :: (fmtX: Call) :: (fmt3: Call) :: Nil =>
          lit.code shouldBe "\"{} {}\""
          lit.typeFullName shouldBe "&str"

          fmtX.name shouldBe Operators.formattedValue
          fmtX.typeFullName shouldBe "i32"
          fmtX.argument.isIdentifier.name.l shouldBe List("x")

          fmt3.name shouldBe Operators.formattedValue
          fmt3.argument.isLiteral.code.l shouldBe List("3")
      }
    }
  }

  "a `println!` macro resolved against the sysroot" should {
    val cpg = code("""
        |fn main() {
        | let x = 7;
        | println!("{}", x);
        |}
        |""".stripMargin)

    "lower to a `_print` call on a formatString" in {
      inside(cpg.call.nameExact("_print").l) { case print :: Nil =>
        print.methodFullName shouldBe "std::io::stdio::_print"

        inside(print.argument.l) { case (format: Call) :: Nil =>
          format.name shouldBe Operators.formatString
          format.typeFullName shouldBe "core::fmt::Arguments<'a>"
          format.argument.isCall.nameExact(Operators.formattedValue).argument.isIdentifier.name.l shouldBe List("x")
        }
      }
    }
  }
}
