package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ForTests extends Rust2CpgSuite(noSysRoot = true) {

  "a for loop" should {
    val cpg = code("""
        |fn main(xs: Vec<i32>) {
        | for x in xs {
        |  foo(x);
        | };
        |}
        |""".stripMargin)

    "lower into a block with a local, the into_iter assignment and a WHILE" in {
      inside(cpg.method.name("main").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (intoIterAssign: Call) :: (loop: ControlStructure) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          intoIterAssign.code shouldBe "<tmp>0 = xs.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "lower the iterable as an into_iter assignment" in {
      inside(cpg.assignment.codeExact("<tmp>0 = xs.into_iter()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>0"
          lhs.typeFullName shouldBe Defines.Any
          rhs.name shouldBe "into_iter"
          rhs.code shouldBe "xs.into_iter()"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::into_iter"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          rhs.typeFullName shouldBe Defines.Any

          inside(rhs.argument(0)) { case xs: Identifier =>
            xs.name shouldBe "xs"
            xs.typeFullName shouldBe Defines.Any
          }
      }
    }

    "create a local for the loop variable" in {
      inside(cpg.whileBlock.astChildren.isBlock.astChildren.isLocal.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.typeFullName shouldBe Defines.Any
      }
    }

    "lower the loop variable as a next assignment" in {
      inside(cpg.assignment.codeExact("x = <tmp>0.next()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe Defines.Any
          rhs.name shouldBe "next"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          rhs.typeFullName shouldBe Defines.Any

          inside(rhs.argument(0)) { case tmp: Identifier =>
            tmp.name shouldBe "<tmp>0"
            tmp.typeFullName shouldBe Defines.Any
          }
      }
    }
  }
}

class ForTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "a for loop over a vector" should {
    val cpg = code("""
        |fn main() {
        | let xs = vec![1, 2, 3];
        | for x in xs {
        |  foo(x);
        | };
        |}
        |""".stripMargin)

    "lower into a block with a local, the into_iter assignment and a WHILE" in {
      inside(cpg.method.name("main").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (intoIterAssign: Call) :: (loop: ControlStructure) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          intoIterAssign.code shouldBe "<tmp>0 = xs.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "lower the iterable as an into_iter assignment" in {
      inside(cpg.assignment.codeExact("<tmp>0 = xs.into_iter()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>0"
          rhs.name shouldBe "into_iter"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::into_iter"
          rhs.typeFullName shouldBe Defines.Any

          inside(rhs.argument(0)) { case xs: Identifier =>
            xs.name shouldBe "xs"
            xs.typeFullName shouldBe "alloc::vec::Vec<i32, alloc::alloc::Global>"
          }
      }
    }

    "create a local for the loop variable" in {
      inside(cpg.whileBlock.astChildren.isBlock.astChildren.isLocal.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.typeFullName shouldBe "i32"
      }
    }

    "lower the loop variable as a next assignment" in {
      inside(cpg.assignment.codeExact("x = <tmp>0.next()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "next"
          // TODO(rust_ast_gen): export this `next`'s methodFullName.
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.typeFullName shouldBe "core::option::Option<i32>"

          inside(rhs.argument(0)) { case tmp: Identifier =>
            tmp.name shouldBe "<tmp>0"
            tmp.typeFullName shouldBe Defines.Any
          }
      }
    }

    "have x as argument to the foo call" in {
      inside(cpg.call.nameExact("foo").argument.l) { case (ident: Identifier) :: Nil =>
        ident.name shouldBe "x"
        ident.typeFullName shouldBe "i32"
      }
    }
  }

  "for loop over tuple pattern" should {
    val cpg = code("""
        |fn main(pairs: Vec<(i32, bool)>) {
        | for (x, y) in pairs {
        |  foo(x, y);
        | };
        |}
        |""".stripMargin)

    "have correct block children" in {
      inside(cpg.method.nameExact("main").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (intoIter: Call) :: (loop: ControlStructure) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          intoIter.code shouldBe "<tmp>0 = pairs.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "have correct into_iter assignment" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>0")).argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>0"
          rhs.name shouldBe "into_iter"
          rhs.code shouldBe "pairs.into_iter()"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::into_iter"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          // TODO(rust_ast_gen): typeFullName for into_iter()/tmp0.
          pendingUntilFixed(
            rhs.typeFullName shouldBe "alloc::vec::into_iter::IntoIter<(i32, bool), alloc::alloc::Global>"
          )

          inside(rhs.argument.l) { case (pairs: Identifier) :: Nil =>
            pairs.name shouldBe "pairs"
            pairs.typeFullName shouldBe "alloc::vec::Vec<(i32, bool), alloc::alloc::Global>"
          }
      }
    }

    "have correct locals" in {
      inside(cpg.whileBlock.astChildren.isBlock.astChildren.isLocal.l) { case tmp :: xLocal :: yLocal :: Nil =>
        tmp.name shouldBe "<tmp>1"
        tmp.typeFullName shouldBe "(i32, bool)"
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "bool"
      }
    }

    "have correct next assignment" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>1")).argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>1"
          lhs.typeFullName shouldBe "(i32, bool)"

          rhs.name shouldBe "next"
          rhs.code shouldBe "<tmp>0.next()"
          // TODO(rust_ast_gen): methodFullName for next().
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          rhs.typeFullName shouldBe "core::option::Option<(i32, bool)>"
      }
    }

    "have correct binding assignments" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "<tmp>1.0"
        fieldAccess.typeFullName shouldBe "i32"
      }

      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("y")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "<tmp>1.1"
        fieldAccess.typeFullName shouldBe "bool"
      }
    }
  }

  "for loop over record pattern" should {
    val cpg = code("""
        |struct Point { x: i32, y: bool }
        |fn main(points: Vec<Point>) {
        | for Point { x, y } in points {
        |  foo(x, y);
        | };
        |}
        |""".stripMargin)

    "have correct block children" in {
      inside(cpg.method.nameExact("main").block.astChildren.isBlock.astChildren.l) {
        case (tmp: Local) :: (intoIter: Call) :: (loop: ControlStructure) :: Nil =>
          tmp.name shouldBe "<tmp>0"
          intoIter.code shouldBe "<tmp>0 = points.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "have correct into_iter assignment" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>0")).argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>0"
          rhs.name shouldBe "into_iter"
          rhs.code shouldBe "points.into_iter()"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::into_iter"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          // TODO(rust_ast_gen): typeFullName for into_iter()/tmp0.
          pendingUntilFixed(
            rhs.typeFullName shouldBe "alloc::vec::into_iter::IntoIter<rust2cpgtest::Point, alloc::alloc::Global>"
          )

          inside(rhs.argument.l) { case (points: Identifier) :: Nil =>
            points.name shouldBe "points"
            points.typeFullName shouldBe "alloc::vec::Vec<rust2cpgtest::Point, alloc::alloc::Global>"
          }
      }
    }

    "have correct locals" in {
      inside(cpg.whileBlock.astChildren.isBlock.astChildren.isLocal.l) { case tmp :: xLocal :: yLocal :: Nil =>
        tmp.name shouldBe "<tmp>1"
        // TODO(rust_ast_gen): typeFullName for patterns.
        pendingUntilFixed(tmp.typeFullName shouldBe "rust2cpgtest::Point")
        xLocal.name shouldBe "x"
        xLocal.typeFullName shouldBe "i32"
        yLocal.name shouldBe "y"
        yLocal.typeFullName shouldBe "bool"
      }
    }

    "have correct next assignment" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("<tmp>1")).argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "<tmp>1"
          rhs.name shouldBe "next"
          rhs.code shouldBe "<tmp>0.next()"
          // TODO(rust_ast_gen): methodFullName for next().
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      }
    }

    "have correct binding assignments" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("x")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "<tmp>1.x"
        fieldAccess.typeFullName shouldBe "i32"
      }

      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("y")).source.l) { case (fieldAccess: Call) :: Nil =>
        fieldAccess.methodFullName shouldBe Operators.fieldAccess
        fieldAccess.code shouldBe "<tmp>1.y"
        fieldAccess.typeFullName shouldBe "bool"
      }
    }
  }
}
