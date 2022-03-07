package io.joern.kotlin2cpg.validation

import io.joern.kotlin2cpg.Kt2CpgTestContext
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.edges.Ast
import io.shiftleft.codepropertygraph.generated.nodes.{ClosureBinding, FieldIdentifier, Identifier}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.jIteratortoTraversal

class PrimitiveArrayTypeMappingTests extends AnyFreeSpec with Matchers {
  "CPG for code with usage of `kotlin.BooleanArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = BooleanArray(2)
        |    nums[0] = true
        |    nums[1] = false
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "boolean[]"
    }
  }

  "CPG for code with usage of `kotlin.ByteArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = ByteArray(2)
        |    nums[0] = 0x41
        |    nums[1] = 0x42
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "byte[]"
    }
  }

  "CPG for code with extension fn called on instance of `kotlin.ByteArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val charset = Charsets.UTF_8
        |    val byteArray = "Hello".toByteArray(charset)
        |    println(byteArray.toString(charset))
        |}
        |""".stripMargin)

    "should contain a CALL node with a METHOD_FULL_NAME starting with `kotlin.ByteArray`" in {
      val List(c) = cpg.call.code("byte.*toString.*").l
      c.methodFullName shouldBe "kotlin.ByteArray.toString:java.lang.String(java.nio.charset.Charset)"
    }
  }

  "CPG for code with usage of `kotlin.CharArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = CharArray(2)
        |    nums[0] = 'A'
        |    nums[1] = 'B'
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "char[]"
    }
  }

  "CPG for code with usage of `kotlin.DoubleArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = DoubleArray(2)
        |    nums[0] = 41.0
        |    nums[1] = 42.0
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "double[]"
    }
  }

  "CPG for code with usage of `kotlin.FloatArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = FloatArray(2)
        |    nums[0] = 41.0f
        |    nums[1] = 42.0f
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "float[]"
    }
  }

  "CPG for code with usage of `kotlin.IntArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = IntArray(2)
        |    nums[0] = 41
        |    nums[1] = 42
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "int[]"
    }
  }

  "CPG for code with usage of `kotlin.LongArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = LongArray(2)
        |    nums[0] = 41L
        |    nums[1] = 42L
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "long[]"
    }
  }

  "CPG for code with usage of `kotlin.ShortArray`" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val nums = ShortArray(2)
        |    nums[0] = 41
        |    nums[1] = 42
        |    val x = nums[0]
        |    println(x)
        |}
        |""".stripMargin)

    "should contain an IDENTIFIER node with a TYPE_FULL_NAME of its mapped type" in {
      val List(i) = cpg.identifier.nameExact("nums").head.l
      i.typeFullName shouldBe "short[]"
    }
  }
}
