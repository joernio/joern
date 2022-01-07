package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LiteralTests extends AnyFreeSpec with Matchers {

  "CPG for code simple literal declarations" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |fun main(args : Array<String>) {
        |  val a: Int = 1
        |  val b: Boolean = true
        |  val c: Float = 1.0
        |  val d: Char = 'A'
        |  val e: String = "ABC"
        |  val p = 1_000_000
        |  val q = 9999L
        |  val r = 0xB4DF00D
        |  val s = 0b010101
        |}
        |""".stripMargin)

    "should contain the literals" in {
      val List(a: Literal) = cpg.literal("1").l
      a.typeFullName shouldBe "kotlin.Int"

      val List(b: Literal) = cpg.literal("true").l
      b.typeFullName shouldBe "kotlin.Boolean"

      val List(c: Literal) = cpg.literal("1.0").l
      c.typeFullName shouldBe "kotlin.Double"

      val List(d: Literal) = cpg.literal("\'A\'").l
      d.typeFullName shouldBe "kotlin.Char"

      val List(e: Literal) = cpg.literal("\"ABC\"").l
      e.typeFullName shouldBe "kotlin.String"

      val List(p: Literal) = cpg.literal("1_000_000").l
      p.typeFullName shouldBe "kotlin.Int"

      val List(q: Literal) = cpg.literal("9999L").l
      q.typeFullName shouldBe "kotlin.Long"

      val List(r: Literal) = cpg.literal("0xB4DF00D").l
      r.typeFullName shouldBe "kotlin.Int"

      val List(s: Literal) = cpg.literal("0b010101").l
      s.typeFullName shouldBe "kotlin.Int"
    }
  }

  "CPG for code simple literal declarations without explicit types" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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

    "should literals with the correct TYPE_FULL_NAME" in {
      val List(a: Literal) = cpg.literal("1").l
      a.typeFullName shouldBe "kotlin.Int"

      val List(b: Literal) = cpg.literal("true").l
      b.typeFullName shouldBe "kotlin.Boolean"

      val List(c: Literal) = cpg.literal("1.0").l
      c.typeFullName shouldBe "kotlin.Double"

      val List(d: Literal) = cpg.literal("\'A\'").l
      d.typeFullName shouldBe "kotlin.Char"

      val List(e: Literal) = cpg.literal("\"ABC\"").l
      e.typeFullName shouldBe "kotlin.String"

      val List(p: Literal) = cpg.literal("1_000_000").l
      p.typeFullName shouldBe "kotlin.Int"

      val List(q: Literal) = cpg.literal("9999L").l
      q.typeFullName shouldBe "kotlin.Long"

      val List(r: Literal) = cpg.literal("0xB4DF00D").l
      r.typeFullName shouldBe "kotlin.Int"

      val List(s: Literal) = cpg.literal("0b010101").l
      s.typeFullName shouldBe "kotlin.Int"
    }

  }
}
