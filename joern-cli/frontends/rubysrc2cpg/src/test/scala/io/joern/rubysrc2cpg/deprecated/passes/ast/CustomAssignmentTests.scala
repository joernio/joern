package io.joern.rubysrc2cpg.deprecated.passes.ast

import io.joern.rubysrc2cpg.testfixtures.{DifferentInNewFrontend, RubyCode2CpgFixture}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, MethodRef, TypeRef}
import io.shiftleft.semanticcpg.language.*

class CustomAssignmentTests extends RubyCode2CpgFixture(withPostProcessing = true, useDeprecatedFrontend = true) {

  "custom assignment for builtIn" should {
    val cpg = code("""
        |puts "This is ruby"
        |""".stripMargin)
    "be created for builtin presence" taggedAs DifferentInNewFrontend in {
      val List(putsAssignmentCall, _) = cpg.call.l
      putsAssignmentCall.name shouldBe "<operator>.assignment"

      val List(putsIdentifier: Identifier, putsBuiltInTypeRef: TypeRef) = putsAssignmentCall.argument.l: @unchecked

      putsIdentifier.name shouldBe "puts"
      putsBuiltInTypeRef.code shouldBe "__builtin.puts"
      putsBuiltInTypeRef.typeFullName shouldBe "__builtin.puts"
    }

    "resolve type for `puts`" in {
      val List(_, putsCall) = cpg.call.l
      putsCall.name shouldBe "puts"
      putsCall.methodFullName shouldBe "__builtin.puts"
    }
  }

  "custom assignment for user defined function" should {
    val cpg = code("""
        |def foo()
        |   return "This is my foo"
        |end
        |
        |foo()
        |""".stripMargin)
    "be created" in {
      val List(fooAssignmentCall, _) = cpg.call.l
      fooAssignmentCall.name shouldBe "<operator>.assignment"

      val List(fooIdentifier: Identifier, fooMethodRef: MethodRef) = fooAssignmentCall.argument.l: @unchecked

      fooIdentifier.name shouldBe "foo"
      fooMethodRef.methodFullName shouldBe "Test0.rb::program.foo"
      fooMethodRef.referencedMethod.name shouldBe "foo"
    }

    "resolve type for `foo`" in {
      val List(_, fooCall) = cpg.call.l
      fooCall.name shouldBe "foo"
      fooCall.methodFullName shouldBe "Test0.rb::program.foo"
    }
  }

}
