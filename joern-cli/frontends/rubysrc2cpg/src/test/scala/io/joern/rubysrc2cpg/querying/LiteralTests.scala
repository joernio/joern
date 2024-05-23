package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends RubyCode2CpgFixture {

  "`123` is represented by a LITERAL node" in {
    val cpg = code("""
        |123
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "123"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Integer"
  }

  "`3.14` is represented by a LITERAL node" in {
    val cpg = code("""
        |3.14
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3.14"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Float"
  }

  "`3e10` is represented by a LITERAL node" in {
    val cpg = code("""
        |3e10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3e10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Float"
  }

  "`12e-10` is represented by a LITERAL node" in {
    val cpg = code("""
        |12e-10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "12e-10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Float"
  }

  "`0b01` is represented by a LITERAL node" in {
    val cpg = code("""
        |0b01
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0b01"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Integer"
  }

  "`0xabc` is represented by a LITERAL node" in {
    val cpg = code("""
        |0xabc
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0xabc"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Integer"
  }

  "`true` is represented by a LITERAL node" in {
    val cpg = code("""
        |true
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "true"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.TrueClass"
  }

  "`false` is represented by a LITERAL node" in {
    val cpg = code("""
        |false
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "false"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.FalseClass"
  }

  "`nil` is represented by a LITERAL node" in {
    val cpg = code("""
        |nil
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "nil"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.NilClass"
  }

  "`'hello'` is represented by a LITERAL node" in {
    val cpg = code("""
        |'hello'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "'hello'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  "`'x' 'y' 'z'` is represented by a LITERAL node" in {
    val cpg = code("""
        |'x' 'y' 'z'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "'x' 'y' 'z'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  "`\"hello\"` is represented by a LITERAL node" in {
    val cpg = code("""
        |"hello"
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "\"hello\""
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  "`%q(hello)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%q(hello)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%q(hello)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  "`%Q(hello world)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%Q(hello world)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%Q(hello world)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  "`%(foo \"bar\" baz)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%(foo "bar" baz)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%(foo \"bar\" baz)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"
  }

  """`%q<\n...\n>` is represented by a LITERAL node""" in {
    val cpg = code("""
        |%q<
        |xyz
        |123
        |>
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe
      """%q<
        |xyz
        |123
        |>""".stripMargin
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.String"

  }

  "`:symbol` is represented by a LITERAL node" in {
    val cpg = code("""
        |:symbol
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":symbol"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Symbol"
  }

  "`:'symbol'` is represented by a LITERAL node" in {
    val cpg = code("""
        |:'symbol'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":'symbol'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Symbol"
  }

  "`/(eu|us)/` is represented by a LITERAL node" in {
    val cpg = code("""
        |/(eu|us)/
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "/(eu|us)/"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe "__builtin.Regexp"
  }

  "`/#{os_version_regex}/` is represented by a CALL node with a string format method full name" in {
    val cpg = code("""
        |os_version_regex = "1.2.0"
        |/#{os_version_regex}/
        |""".stripMargin)

    val List(formatValueCall) = cpg.call.code("/#.*").l
    formatValueCall.code shouldBe "/#{os_version_regex}/"
    formatValueCall.lineNumber shouldBe Some(3)
    formatValueCall.typeFullName shouldBe "__builtin.Regexp"
    formatValueCall.methodFullName shouldBe Operators.formatString
  }

  "regex values in a hash literal" ignore {
    val cpg = code("""
        |PLATFORM_PATTERNS = {
        | :redhat        => /fedora|el-|centos/
        |}
        |""".stripMargin)

    cpg.method(":program").dotAst.foreach(println)
  }

}
