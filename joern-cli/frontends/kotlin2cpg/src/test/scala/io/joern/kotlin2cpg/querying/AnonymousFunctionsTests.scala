package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, ModifierTypes}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.Ignore

import scala.annotation.unused

class AnonymousFunctionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with anonymous function as argument" should {
    val cpg = code("""
        |package mypkg
        |fun foo(x: Int): Int {
        |    val l = listOf(1, x)
        |    val out = l.filter(fun(item: Int): Boolean { return item > 0 }).last()
        |    return out
        |}
        |""".stripMargin)

    "should contain a METHOD node for the anonymous fn with the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*0.*").l
      m.fullName shouldBe s"mypkg.foo.${Defines.ClosurePrefix}0:boolean(int)"
      m.signature shouldBe "boolean(int)"
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "java.lang.Object"
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod1, mod2) = cpg.method.fullName(".*lambda.*").modifier.l
      mod1.modifierType shouldBe ModifierTypes.VIRTUAL
      mod2.modifierType shouldBe ModifierTypes.LAMBDA
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "item"
      p.typeFullName shouldBe "int"
    }
  }

  "CPG for code with single-expression-body anonymous function as argument" should {
    val cpg = code("""
        |package mypkg
        |fun foo(x: Int): Int {
        |    val l = listOf(1, x)
        |    val out = l.filter(fun(item) = item > 0).last()
        |    return out
        |}
        |""".stripMargin)

    "should contain a METHOD node for the anonymous fn with the correct props set" in {
      val List(m) = cpg.method.fullName(".*lambda.*0.*").l
      m.fullName shouldBe s"mypkg.foo.${Defines.ClosurePrefix}0:boolean(int)"
      m.signature shouldBe "boolean(int)"
    }

    "should contain a METHOD node for the lambda with a corresponding METHOD_RETURN which has the correct props set" in {
      val List(mr) = cpg.method.fullName(".*lambda.*").methodReturn.l
      mr.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      mr.typeFullName shouldBe "java.lang.Object"
    }

    "should contain a METHOD node for the lambda with a corresponding MODIFIER which has the correct props set" in {
      val List(mod1, mod2) = cpg.method.fullName(".*lambda.*").modifier.l
      mod1.modifierType shouldBe ModifierTypes.VIRTUAL
      mod2.modifierType shouldBe ModifierTypes.LAMBDA
    }

    "should contain a METHOD_PARAMETER_IN for the lambda with the correct properties set" in {
      val List(p) = cpg.method.fullName(".*lambda.*").parameter.l
      p.code shouldBe "item"
      p.typeFullName shouldBe "int"
    }
  }
}
