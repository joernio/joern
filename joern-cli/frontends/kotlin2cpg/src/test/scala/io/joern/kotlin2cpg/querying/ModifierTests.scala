package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ModifierTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with various modifiers applied to various functions" should {
    val cpg = code("""
       |package mypkg
       |
       |fun g1() = println(1)
       |public fun g2() = println(2)
       |private fun g3() = println(3)
       |internal fun g4() = println(4)
       |""".stripMargin)

    "should contain MODIFIER nodes attached to the METHOD nodes" in {
      val List(mod1) = cpg.method.nameExact("g1").modifier.l
      mod1.modifierType shouldBe "PUBLIC"

      val List(mod2) = cpg.method.nameExact("g2").modifier.l
      mod2.modifierType shouldBe "PUBLIC"

      val List(mod3) = cpg.method.nameExact("g3").modifier.l
      mod3.modifierType shouldBe "PRIVATE"

      val List(mod4) = cpg.method.nameExact("g4").modifier.l
      mod4.modifierType shouldBe "INTERNAL"
    }
  }

  "CPG for code with various modifiers applied to various methods of a class" should {
    val cpg = code("""
        |package mypkg
        |
        |class SomeClass {
        |    fun g1() = println(1)
        |    public fun g2() = println(2)
        |    private fun g3() = println(3)
        |    internal fun g4() = println(4)
        |    protected fun g5() = println(4)
        |}
        |""".stripMargin)

    "should contain MODIFIER nodes attached to the METHOD nodes" in {
      val List(mod1, modv1) = cpg.method.nameExact("g1").modifier.l
      mod1.modifierType shouldBe "PUBLIC"
      modv1.modifierType shouldBe "VIRTUAL"

      val List(mod2, modv2) = cpg.method.nameExact("g2").modifier.l
      mod2.modifierType shouldBe "PUBLIC"
      modv2.modifierType shouldBe "VIRTUAL"

      val List(mod3, modv3) = cpg.method.nameExact("g3").modifier.l
      mod3.modifierType shouldBe "PRIVATE"
      modv3.modifierType shouldBe "VIRTUAL"

      val List(mod4, modv4) = cpg.method.nameExact("g4").modifier.l
      mod4.modifierType shouldBe "INTERNAL"
      modv4.modifierType shouldBe "VIRTUAL"

      val List(mod5, modv5) = cpg.method.nameExact("g5").modifier.l
      mod5.modifierType shouldBe "PROTECTED"
      modv5.modifierType shouldBe "VIRTUAL"
    }
  }
}
