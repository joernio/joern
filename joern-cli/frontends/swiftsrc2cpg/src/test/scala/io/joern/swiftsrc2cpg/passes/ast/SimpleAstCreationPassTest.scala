package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class SimpleAstCreationPassTest extends AbstractPassTest {

  "AST generation for simple fragments" should {

    "have module modifier at the top level module method node" in AstFixture("") { cpg =>
      val List(method)                = cpg.method.nameExact("<global>").l
      val List(modVirtual, modModule) = method.modifier.l
      modVirtual.modifierType shouldBe ModifierTypes.VIRTUAL
      modVirtual.order shouldBe 0
      modModule.modifierType shouldBe ModifierTypes.MODULE
      modModule.order shouldBe 1
    }

    "have correct modifier for a function" in AstFixture("private static func foo() -> {}") { cpg =>
      val List(method)                            = cpg.method.nameExact("foo").l
      val List(modVirtual, modPrivate, modStatic) = method.modifier.l
      modVirtual.modifierType shouldBe ModifierTypes.VIRTUAL
      modVirtual.order shouldBe 0
      modPrivate.modifierType shouldBe ModifierTypes.PRIVATE
      modPrivate.order shouldBe 1
      modStatic.modifierType shouldBe ModifierTypes.STATIC
      modStatic.order shouldBe 2
    }

    "have correct structure for simple variable declarations" in AstFixture("""
        |let x = 1
        |var y: String = "2"
        |""".stripMargin) { cpg =>
      val List(method)           = cpg.method.nameExact("<global>").l
      val List(assignX, assignY) = method.assignment.l
      assignX.code shouldBe "let x = 1"
      assignY.code shouldBe """var y: String = "2""""
    }

    "have corresponding type decl with correct bindings for function" in AstFixture("func method() -> {}") { cpg =>
      val List(typeDecl) = cpg.typeDecl.nameExact("method").l
      typeDecl.fullName should endWith(".swift:<global>:method")

      val List(binding) = typeDecl.bindsOut.l
      binding.name shouldBe ""
      binding.signature shouldBe ""

      val List(boundMethod) = binding.refOut.l
      boundMethod shouldBe cpg.method.nameExact("method").head
    }

    "have correct closure bindings" in AstFixture("""
        |func foo() -> {
        |  let x = 1
        |  func bar() -> {
        |    x = 2
        |  }
        |}
        |""".stripMargin) { cpg =>
      val List(fooMethod)      = cpg.method.nameExact("foo").l
      val List(fooBlock)       = fooMethod.astChildren.isBlock.l
      val List(fooLocalX)      = fooBlock.astChildren.isLocal.nameExact("x").l
      val List(barRef)         = fooBlock.astChildren.isCall.astChildren.isMethodRef.l
      val List(closureBinding) = barRef.captureOut.l
      closureBinding.closureBindingId shouldBe Option("code.swift:<global>:foo:bar:x")
      closureBinding.closureOriginalName shouldBe Option("x")
      closureBinding.evaluationStrategy shouldBe EvaluationStrategies.BY_REFERENCE
      closureBinding.refOut.head shouldBe fooLocalX

      val List(barMethod)      = cpg.method.nameExact("bar").l
      val List(barMethodBlock) = barMethod.astChildren.isBlock.l
      val List(barLocals)      = barMethodBlock.astChildren.isLocal.l
      barLocals.closureBindingId shouldBe Option("code.swift:<global>:foo:bar:x")

      val List(identifierX) = barMethodBlock.astChildren.isCall.astChildren.isIdentifier.nameExact("x").l
      identifierX.refOut.head shouldBe barLocals
    }

    "have correct structure for annotated function" in AstFixture("""
        |@bar(x: "y")
        |func foo() -> {
        |  let x = 1
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.nameExact("foo").annotation.l) { case List(bar) =>
        bar.code shouldBe """@bar(x: "y")"""
        bar.name shouldBe "bar"
        bar.fullName shouldBe "bar"
        val List(paramAssignFoo) = bar.parameterAssign.l
        paramAssignFoo.code shouldBe """x: "y""""
        paramAssignFoo.order shouldBe 1
        val List(paramFoo) = paramAssignFoo.parameter.l
        paramFoo.code shouldBe "argument"
        paramFoo.order shouldBe 1
        val List(paramValueFoo) = paramAssignFoo.value.l
        paramValueFoo.code shouldBe """x: "y""""
        paramValueFoo.order shouldBe 2
        paramValueFoo.argumentIndex shouldBe 2
      }
    }
  }

}
