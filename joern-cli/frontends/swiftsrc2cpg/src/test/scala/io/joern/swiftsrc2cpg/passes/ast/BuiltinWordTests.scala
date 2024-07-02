// This test file has been translated from swift/test/Parse/builtin_word.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite
import io.shiftleft.semanticcpg.language.*

class BuiltinWordTests extends AstSwiftSrc2CpgSuite {

  "BuiltinWordTests" should {

    "testBuiltinWord1" in {
      val cpg = code("precedencegroup AssignmentPrecedence { assignment: true }")
      // precedencegroups cannot be expressed within the CPG and are of no value for us
      cpg.literal.codeExact("AssignmentPrecedence") shouldBe empty
    }

    "testBuiltinWord2" in {
      val cpg = code("""
      |var word: Builtin.Word
      |var i16: Builtin.Int16
      |var i32: Builtin.Int32
      |var i128: Builtin.Int128
      |""".stripMargin)
      val List(word, i16, i32, i128) = cpg.local.l
      word.typeFullName shouldBe "Builtin.Word"
      i16.typeFullName shouldBe "Builtin.Int16"
      i32.typeFullName shouldBe "Builtin.Int32"
      i128.typeFullName shouldBe "Builtin.Int128"
    }

    "testBuiltinWord4" in {
      val cpg = code("""
        |word128 = Builtin.truncOrBitCast_Int128_Word(i128)
        |word64 = Builtin.truncOrBitCast_Int64_Word(i64)
        |word32 = Builtin.truncOrBitCast_Int32_Word(i32)
        |word16 = Builtin.truncOrBitCast_Int16_Word(i16)
        |""".stripMargin)
      val List(word128, word64, word32, word16) = cpg.identifier.name("word.*").l
      cpg.local.nameExact(word128.name).size shouldBe 1
      cpg.local.nameExact(word64.name).size shouldBe 1
      cpg.local.nameExact(word32.name).size shouldBe 1
      cpg.local.nameExact(word16.name).size shouldBe 1
      cpg.call.codeExact("Builtin.truncOrBitCast_Int128_Word(i128)").size shouldBe 1
      cpg.call.codeExact("Builtin.truncOrBitCast_Int64_Word(i64)").size shouldBe 1
      cpg.call.codeExact("Builtin.truncOrBitCast_Int32_Word(i32)").size shouldBe 1
      cpg.call.codeExact("Builtin.truncOrBitCast_Int16_Word(i16)").size shouldBe 1
    }

  }

}
