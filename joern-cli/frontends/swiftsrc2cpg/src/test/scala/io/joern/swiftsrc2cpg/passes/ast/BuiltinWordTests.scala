// This test file has been translated from swift/test/Parse/builtin_word.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class BuiltinWordTests extends AbstractPassTest {

  "BuiltinWordTests" should {

    "testBuiltinWord1" ignore AstFixture("precedencegroup AssignmentPrecedence { assignment: true }") { cpg => ??? }

    "testBuiltinWord2" ignore AstFixture("""
      |var word: Builtin.Word
      |var i16: Builtin.Int16
      |var i32: Builtin.Int32
      |var i128: Builtin.Int128
      |""".stripMargin) { cpg => ??? }

    "testBuiltinWord4" ignore AstFixture("""
        |word = Builtin.truncOrBitCast_Int128_Word(i128)
        |word = Builtin.truncOrBitCast_Int64_Word(i64)
        |word = Builtin.truncOrBitCast_Int32_Word(i32)
        |word = Builtin.truncOrBitCast_Int16_Word(i16)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord5" ignore AstFixture("""
        |i16 = Builtin.truncOrBitCast_Word_Int16(word)
        |i32 = Builtin.truncOrBitCast_Word_Int32(word)
        |i64 = Builtin.truncOrBitCast_Word_Int64(word)
        |i128 = Builtin.truncOrBitCast_Word_Int128(word)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord6" ignore AstFixture("""
        |word = Builtin.zextOrBitCast_Int128_Word(i128)
        |word = Builtin.zextOrBitCast_Int64_Word(i64)
        |word = Builtin.zextOrBitCast_Int32_Word(i32)
        |word = Builtin.zextOrBitCast_Int16_Word(i16)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord7" ignore AstFixture("""
        |i16 = Builtin.zextOrBitCast_Word_Int16(word)
        |i32 = Builtin.zextOrBitCast_Word_Int32(word)
        |i64 = Builtin.zextOrBitCast_Word_Int64(word)
        |i128 = Builtin.zextOrBitCast_Word_Int128(word)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord8" ignore AstFixture("""
        |word = Builtin.trunc_Int128_Word(i128)
        |word = Builtin.trunc_Int64_Word(i64)
        |word = Builtin.trunc_Int32_Word(i32)
        |word = Builtin.trunc_Int16_Word(i16)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord9" ignore AstFixture("""
        |i16 = Builtin.trunc_Word_Int16(word)
        |i32 = Builtin.trunc_Word_Int32(word)
        |i64 = Builtin.trunc_Word_Int64(word)
        |i128 = Builtin.trunc_Word_Int128(word)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord10" ignore AstFixture("""
        |word = Builtin.zext_Int128_Word(i128)
        |word = Builtin.zext_Int64_Word(i64)
        |word = Builtin.zext_Int32_Word(i32)
        |word = Builtin.zext_Int16_Word(i16)
        |""".stripMargin) { cpg => ??? }

    "testBuiltinWord11" ignore AstFixture(""",
        |i16 = Builtin.zext_Word_Int16(word)
        |i32 = Builtin.zext_Word_Int32(word)
        |i64 = Builtin.zext_Word_Int64(word)
        |i128 = Builtin.zext_Word_Int128(word)
        |""".stripMargin) { cpg => ??? }
  }

}
