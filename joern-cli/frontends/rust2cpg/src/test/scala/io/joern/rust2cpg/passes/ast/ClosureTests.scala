package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ClosureTests extends Rust2CpgSuite(noSysRoot = true) {

  "capture-free closure" should {
    val cpg = code("""
        |fn main() {
        | let f = |x| {
        |  let y = x * 2;
        |  y
        | };
        | let z = f(3);
        |}
        |""".stripMargin)

    "have correct fullName" in {
      cpg.method.nameExact("<lambda>0").fullName.l shouldBe List("rust2cpgtest::main::<lambda>0")
    }

    "have correct modifiers" in {
      cpg.method.nameExact("<lambda>0").modifier.modifierType.l shouldBe List(
        ModifierTypes.VIRTUAL,
        ModifierTypes.LAMBDA
      )
    }

    "have correct MethodRef" in {
      inside(cpg.assignment.where(_.target.isIdentifier.nameExact("f")).argument(2).l) {
        case (metRef: MethodRef) :: Nil =>
          metRef.methodFullName shouldBe "rust2cpgtest::main::<lambda>0"
          metRef.typeFullName shouldBe "rust2cpgtest::main::<lambda>0"
      }
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("<lambda>0").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "x"
        param.index shouldBe 1
        param.typeFullName shouldBe "i32"
      }
    }

    // TODO(rust_ast_gen): check why typeFullNames are missing.
    "have correct body" in {
      inside(cpg.method.nameExact("<lambda>0").block.astChildren.l) {
        case (local: Local) :: (assign: Call) :: (ret: Return) :: Nil =>
          local.name shouldBe "y"
          pendingUntilFixed(local.typeFullName shouldBe "i32")

          inside(assign.argument.l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.name shouldBe "y"
            rhs.code shouldBe "x * 2"
          }

          inside(ret.astChildren.l) { case (yIdent: Identifier) :: Nil =>
            yIdent.name shouldBe "y"
            pendingUntilFixed(yIdent.typeFullName shouldBe "i32")
          }
      }
    }

    // TODO(rust_ast_gen): check why typeFullName is missing.
    "have correct typeFullName" in {
      pendingUntilFixed(cpg.method.nameExact("<lambda>0").methodReturn.typeFullName.l shouldBe List("i32"))
    }

  }

}

class ClosureTestsWithSysroot extends Rust2CpgSuite(noSysRoot = false) {

  "capture-free closure into `map`" should {
    val cpg = code("""
        |fn main() {
        | let xs = vec![1, 2, 3];
        | let ys: Vec<i32> = xs.iter().map(|x| x * 2).collect();
        |}
        |""".stripMargin)

    "have correct MethodRef" in {
      inside(cpg.call.nameExact("map").argument.sortBy(_.argumentIndex).l) {
        case (base: Call) :: (metRef: MethodRef) :: Nil =>
          base.code shouldBe "xs.iter()"
          metRef.code shouldBe "|x| x * 2"
          metRef.methodFullName shouldBe "rust2cpgtest::main::<lambda>0"
          metRef.typeFullName shouldBe "rust2cpgtest::main::<lambda>0"
      }
    }

    "have correct parameters" in {
      inside(cpg.method.nameExact("<lambda>0").parameter.l) { case (param: MethodParameterIn) :: Nil =>
        param.name shouldBe "x"
        param.index shouldBe 1
        param.typeFullName shouldBe "&i32"
      }
    }

    "have correct body" in {
      inside(cpg.method.nameExact("<lambda>0").block.astChildren.l) { case (ret: Return) :: Nil =>
        ret.code shouldBe "x * 2"

        inside(ret.astChildren.l) { case (mul: Call) :: Nil =>
          mul.code shouldBe "x * 2"
          mul.typeFullName shouldBe "i32"
        }
      }
    }

    "have correct typeFullName" in {
      cpg.method.nameExact("<lambda>0").methodReturn.typeFullName.l shouldBe List("i32")
    }
  }
}
