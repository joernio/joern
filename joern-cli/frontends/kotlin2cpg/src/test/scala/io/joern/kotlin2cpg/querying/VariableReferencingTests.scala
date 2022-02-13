package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class VariableReferencingTests extends AnyFreeSpec with Matchers {
  "should find references for simple case" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun foo(x: Int): Int {
        |  val y = x
        |  return y
        |}
        |""".stripMargin)

    "the local variable exists" in {
      val localNode = cpg.method.name("foo").local.name("y").head
      localNode.closureBindingId shouldBe None
    }

    "identifiers to the parameters exist" in {
      val param = cpg.method.name("foo").parameter.name("x").head
      param.referencingIdentifiers.toSet should not be Set()
    }

    "identifiers to the locals exist" in {
      val localNode = cpg.method.name("foo").local.name("y").head
      localNode.referencingIdentifiers.toSet should not be Set()
    }
  }

  "should find references inside expressions" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun foo(x: Int): Int {
        |  val y = 1 + x
        |  return y
        |}
        |""".stripMargin)

    "the local variable exists" in {
      val localNode = cpg.method.name("foo").local.name("y").head
      localNode.closureBindingId shouldBe None
    }

    "identifiers to the parameters exist" in {
      val param = cpg.method.name("foo").parameter.name("x").head
      param.referencingIdentifiers.toSet should not be Set()
    }
  }
}
