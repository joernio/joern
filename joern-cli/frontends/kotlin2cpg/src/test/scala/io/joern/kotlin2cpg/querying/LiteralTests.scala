package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language._

class LiteralTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code simple literal declarations with explicit types" should {
    val cpg = code("""
        |fun main(args : Array<String>) {
        |  val a: Int = 1
        |  val b: Boolean = true
        |  val bt: Byte = 127
        |  val c: Float = 1.0
        |  val d: Char = 'A'
        |  val e: String = "ABC"
        |  val p = 1_000_000
        |  val q = 9999L
        |  val r = 0xB4DF00D
        |  val s = 0b010101
        |}
        |""".stripMargin)

    "should contain LITERAL nodes with the correct TYPE_FULL_NAME set" in {
      cpg.literal("1").typeFullName.next() shouldBe "int"
      cpg.literal("true").typeFullName.next() shouldBe "boolean"
      cpg.literal("127").typeFullName.next() shouldBe "byte"
      cpg.literal("1.0").typeFullName.next() shouldBe "double"
      cpg.literal("\'A\'").typeFullName.next() shouldBe "char"
      cpg.literal("\"ABC\"").typeFullName.next() shouldBe "java.lang.String"
      cpg.literal("1_000_000").typeFullName.next() shouldBe "int"
      cpg.literal("9999L").typeFullName.next() shouldBe "long"
      cpg.literal("0xB4DF00D").typeFullName.next() shouldBe "int"
      cpg.literal("0b010101").typeFullName.next() shouldBe "int"
    }
  }

  "CPG for code simple literal declarations without explicit types" should {
    val cpg = code("""
        |fun main(args : Array<String>) {
        |  val a = 1
        |  val b = true
        |  val c = 1.0
        |  val d = 'A'
        |  val e = "ABC"
        |  val p = 1_000_000
        |  val q = 9999L
        |  val r = 0xB4DF00D
        |  val s = 0b010101
        |}
        |""".stripMargin)

    "should contain LITERAL nodes with the correct TYPE_FULL_NAME set" in {
      cpg.literal("1").typeFullName.next() shouldBe "int"
      cpg.literal("1.0").typeFullName.next() shouldBe "double"
      cpg.literal("\'A\'").typeFullName.next() shouldBe "char"
      cpg.literal("\"ABC\"").typeFullName.next() shouldBe "java.lang.String"
      cpg.literal("1_000_000").typeFullName.next() shouldBe "int"
      cpg.literal("9999L").typeFullName.next() shouldBe "long"
      cpg.literal("0xB4DF00D").typeFullName.next() shouldBe "int"
      cpg.literal("0b010101").typeFullName.next() shouldBe "int"
    }
  }
}
