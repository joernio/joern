package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

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
      cpg.literal("1").typeFullName.head shouldBe "int"
      cpg.literal("true").typeFullName.head shouldBe "boolean"
      cpg.literal("127").typeFullName.head shouldBe "byte"
      cpg.literal("1.0").typeFullName.head shouldBe "double"
      cpg.literal("\'A\'").typeFullName.head shouldBe "char"
      cpg.literal("\"ABC\"").typeFullName.head shouldBe "java.lang.String"
      cpg.literal("1_000_000").typeFullName.head shouldBe "int"
      cpg.literal("9999L").typeFullName.head shouldBe "long"
      cpg.literal("0xB4DF00D").typeFullName.head shouldBe "int"
      cpg.literal("0b010101").typeFullName.head shouldBe "int"
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
      cpg.literal("1").typeFullName.head shouldBe "int"
      cpg.literal("1.0").typeFullName.head shouldBe "double"
      cpg.literal("\'A\'").typeFullName.head shouldBe "char"
      cpg.literal("\"ABC\"").typeFullName.head shouldBe "java.lang.String"
      cpg.literal("1_000_000").typeFullName.head shouldBe "int"
      cpg.literal("9999L").typeFullName.head shouldBe "long"
      cpg.literal("0xB4DF00D").typeFullName.head shouldBe "int"
      cpg.literal("0b010101").typeFullName.head shouldBe "int"
    }
  }
}
