package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes}
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
          tmp.name shouldBe "tmp"
          intoIterAssign.code shouldBe "tmp = xs.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "lower the iterable as an into_iter assignment" in {
      inside(cpg.assignment.codeExact("tmp = xs.into_iter()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "tmp"
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
      inside(cpg.assignment.codeExact("x = tmp.next()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe Defines.Any
          rhs.name shouldBe "next"
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
          rhs.typeFullName shouldBe Defines.Any

          inside(rhs.argument(0)) { case tmp: Identifier =>
            tmp.name shouldBe "tmp"
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
          tmp.name shouldBe "tmp"
          intoIterAssign.code shouldBe "tmp = xs.into_iter()"
          loop.controlStructureType shouldBe ControlStructureTypes.WHILE
      }
    }

    "lower the iterable as an into_iter assignment" in {
      inside(cpg.assignment.codeExact("tmp = xs.into_iter()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "tmp"
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
      inside(cpg.assignment.codeExact("x = tmp.next()").argument.sortBy(_.argumentIndex).l) {
        case (lhs: Identifier) :: (rhs: Call) :: Nil =>
          lhs.name shouldBe "x"
          lhs.typeFullName shouldBe "i32"
          rhs.name shouldBe "next"
          // TODO(rust_ast_gen): export this `next`'s methodFullName.
          rhs.methodFullName shouldBe s"${Defines.UnresolvedNamespace}::next"
          rhs.typeFullName shouldBe "core::option::Option<i32>"

          inside(rhs.argument(0)) { case tmp: Identifier =>
            tmp.name shouldBe "tmp"
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
}
