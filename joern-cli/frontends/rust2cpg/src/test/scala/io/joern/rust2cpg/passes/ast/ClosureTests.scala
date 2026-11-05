package io.joern.rust2cpg.passes.ast

import io.joern.rust2cpg.testfixtures.Rust2CpgSuite
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EvaluationStrategies, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ClosureTests extends Rust2CpgSuite(noSysRoot = true) {

  "capture-free closure" should {
    val cpg = code("""
        |fn double(x: i32) -> i32 { x * 2 }
        |fn main() {
        | let f = |x| {
        |  let y = double(x);
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

    "have correct body" in {
      inside(cpg.method.nameExact("<lambda>0").block.astChildren.l) {
        case (local: Local) :: (assign: Call) :: (ret: Return) :: Nil =>
          local.name shouldBe "y"
          local.typeFullName shouldBe "i32"

          inside(assign.argument.l) { case (lhs: Identifier) :: (rhs: Call) :: Nil =>
            lhs.name shouldBe "y"
            rhs.code shouldBe "double(x)"
          }

          inside(ret.astChildren.l) { case (yIdent: Identifier) :: Nil =>
            yIdent.name shouldBe "y"
            yIdent.typeFullName shouldBe "i32"
          }
      }
    }

    "have correct typeFullName" in {
      cpg.method.nameExact("<lambda>0").methodReturn.typeFullName.l shouldBe List("i32")
    }

    "have correct typeDecl fullName" in {
      cpg.typeDecl.nameExact("<lambda>0").fullName.l shouldBe List("rust2cpgtest::main::<lambda>0")
    }

    "have correct inheritsFrom" in {
      cpg.typeDecl.fullNameExact("rust2cpgtest::main::<lambda>0").inheritsFromTypeFullName.l shouldBe List(
        "core::ops::function::Fn",
        "core::ops::function::FnMut",
        "core::ops::function::FnOnce"
      )
    }

    "have correct bindings" in {
      inside(cpg.typeDecl.fullNameExact("rust2cpgtest::main::<lambda>0").methodBinding.sortBy(_.name).l) {
        case Binding(
              name = "call",
              signature = "core::ops::function::Fn",
              methodFullName = "rust2cpgtest::main::<lambda>0"
            ) ::
            Binding(
              name = "call_mut",
              signature = "core::ops::function::FnMut",
              methodFullName = "rust2cpgtest::main::<lambda>0"
            ) ::
            Binding(
              name = "call_once",
              signature = "core::ops::function::FnOnce",
              methodFullName = "rust2cpgtest::main::<lambda>0"
            ) :: Nil =>
          succeed
      }
    }
  }

  "closure capturing enclosing local" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | || x + x;
        |}
        |""".stripMargin)

    "have correct captured local" in {
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:x")
      }
    }

    "have correct closure binding" in {
      inside(cpg.method.nameExact("main").block.local.nameExact("x").closureBinding.l) { case binding :: Nil =>
        binding.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:x")
        binding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
        binding._methodRefViaCaptureIn.methodFullName.l shouldBe List("rust2cpgtest::main::<lambda>0")
      }
    }

    "have correct REF edges" in {
      cpg.method.nameExact("main").block.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3)
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.lineNumber shouldBe Some(4)
        local.referencingIdentifiers.lineNumber.l shouldBe List(4, 4)
      }
    }
  }

  "move closure capturing enclosing local" should {
    val cpg = code("""
        |fn main() {
        | let mut n = 0;
        | move || n += 1;
        |}
        |""".stripMargin)

    "have correct captured local" in {
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "n"
        local.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:n")
      }
    }

    "have correct closure binding" in {
      inside(cpg.method.nameExact("main").block.local.nameExact("n").closureBinding.l) { case binding :: Nil =>
        binding.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:n")
        binding.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
        binding._methodRefViaCaptureIn.methodFullName.l shouldBe List("rust2cpgtest::main::<lambda>0")
      }
    }

    "have correct REF edges" in {
      cpg.method.nameExact("main").block.local.nameExact("n").referencingIdentifiers.lineNumber.l shouldBe List(3)
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "n"
        local.lineNumber shouldBe Some(4)
        local.referencingIdentifiers.lineNumber.l shouldBe List(4)
      }
    }
  }

  "nested closure capturing enclosing local" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | || || x;
        |}
        |""".stripMargin)

    "have correct captured locals" in {
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:x")
      }
      inside(cpg.method.nameExact("<lambda>1").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0::<lambda>1:x")
      }
    }

    "have correct closure bindings" in {
      inside(cpg.method.nameExact("main").block.local.nameExact("x").closureBinding.l) { case binding :: Nil =>
        binding.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0:x")
        binding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
        binding._methodRefViaCaptureIn.methodFullName.l shouldBe List("rust2cpgtest::main::<lambda>0")
      }
      inside(cpg.method.nameExact("<lambda>0").block.local.nameExact("x").closureBinding.l) { case binding :: Nil =>
        binding.closureBindingId shouldBe Some("rust2cpgtest::main::<lambda>0::<lambda>1:x")
        binding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
        binding._methodRefViaCaptureIn.methodFullName.l shouldBe List("rust2cpgtest::main::<lambda>0::<lambda>1")
      }
    }

    "have correct REF edges" in {
      cpg.method.nameExact("main").block.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3)
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.lineNumber shouldBe Some(4)
        local.referencingIdentifiers shouldBe empty
      }
      inside(cpg.method.nameExact("<lambda>1").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.lineNumber shouldBe Some(4)
        local.referencingIdentifiers.lineNumber.l shouldBe List(4)
      }
    }
  }

  "closure capturing enclosing parameter" should {
    val cpg = code("""
        |fn f(x: i32) {
        | || x;
        |}
        |""".stripMargin)

    "have correct captured local" in {
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.closureBindingId shouldBe Some("rust2cpgtest::f::<lambda>0:x")
      }
    }

    "have correct closure binding" in {
      inside(cpg.method.nameExact("f").parameter.nameExact("x").closureBinding.l) { case binding :: Nil =>
        binding.closureBindingId shouldBe Some("rust2cpgtest::f::<lambda>0:x")
        binding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
        binding._methodRefViaCaptureIn.methodFullName.l shouldBe List("rust2cpgtest::f::<lambda>0")
      }
    }

    "have correct REF edges" in {
      cpg.method.nameExact("f").parameter.nameExact("x").referencingIdentifiers shouldBe empty
      inside(cpg.method.nameExact("<lambda>0").block.local.l) { case local :: Nil =>
        local.name shouldBe "x"
        local.lineNumber shouldBe Some(3)
        local.referencingIdentifiers.lineNumber.l shouldBe List(3)
      }
    }
  }

  "closure parameter shadowing enclosing local" should {
    val cpg = code("""
        |fn main() {
        | let x = 1;
        | |x| x;
        |}
        |""".stripMargin)

    "have no closure binding" in {
      cpg.closureBinding shouldBe empty
      cpg.method.nameExact("<lambda>0").block.local shouldBe empty
    }

    "have correct REF edges" in {
      cpg.method.nameExact("main").block.local.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(3)
      cpg.method.nameExact("<lambda>0").parameter.nameExact("x").referencingIdentifiers.lineNumber.l shouldBe List(4)
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
