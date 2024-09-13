package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends RubyCode2CpgFixture {

  "`123` is represented by a LITERAL node" in {
    val cpg = code("""
        |123
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "123"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Integer"
  }

  "`3.14` is represented by a LITERAL node" in {
    val cpg = code("""
        |3.14
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3.14"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Float"
  }

  "`3e10` is represented by a LITERAL node" in {
    val cpg = code("""
        |3e10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3e10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Float"
  }

  "`12e-10` is represented by a LITERAL node" in {
    val cpg = code("""
        |12e-10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "12e-10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Float"
  }

  "`0b01` is represented by a LITERAL node" in {
    val cpg = code("""
        |0b01
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0b01"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Integer"
  }

  "`0xabc` is represented by a LITERAL node" in {
    val cpg = code("""
        |0xabc
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0xabc"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Integer"
  }

  "`true` is represented by a LITERAL node" in {
    val cpg = code("""
        |true
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "true"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.TrueClass"
  }

  "`false` is represented by a LITERAL node" in {
    val cpg = code("""
        |false
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "false"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.FalseClass"
  }

  "`nil` is represented by a LITERAL node" in {
    val cpg = code("""
        |nil
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "nil"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.NilClass"
  }

  "`'hello'` is represented by a LITERAL node" in {
    val cpg = code("""
        |'hello'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "'hello'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`'x' 'y' 'z'` is represented by a LITERAL node" in {
    val cpg = code("""
        |'x' 'y' 'z'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "'x' 'y' 'z'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`\"hello\"` is represented by a LITERAL node" in {
    val cpg = code("""
        |"hello"
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "\"hello\""
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`%q(hello)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%q(hello)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%q(hello)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`%Q(hello world)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%Q(hello world)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%Q(hello world)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`%(foo \"bar\" baz)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%(foo "bar" baz)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%(foo \"bar\" baz)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.String"
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
    literal.typeFullName shouldBe s"$kernelPrefix.String"

  }

  "`:symbol` is represented by a LITERAL node" in {
    val cpg = code("""
        |:symbol
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":symbol"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Symbol"
  }

  "`:'symbol'` is represented by a LITERAL node" in {
    val cpg = code("""
        |:'symbol'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":'symbol'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Symbol"
  }

  "`/(eu|us)/` is represented by a LITERAL node" in {
    val cpg = code("""
        |/(eu|us)/
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "/(eu|us)/"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Regexp"
  }

  "`/fedora|el-|centos/` is represented by a LITERAL node" in {
    val cpg = code("""
        |/fedora|el-|centos/
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "/fedora|el-|centos/"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe s"$kernelPrefix.Regexp"
  }

  "`/#{os_version_regex}/` is represented by a CALL node with a string format method full name" in {
    val cpg = code("""
        |os_version_regex = "1.2.0"
        |/#{os_version_regex}/
        |""".stripMargin)

    val List(formatValueCall) = cpg.call.code("/#.*").l
    formatValueCall.code shouldBe "/#{os_version_regex}/"
    formatValueCall.lineNumber shouldBe Some(3)
    formatValueCall.typeFullName shouldBe s"$kernelPrefix.Regexp"
    formatValueCall.methodFullName shouldBe Operators.formatString
  }

  "-> Lambda literal" in {
    val cpg = code("""
        |-> (a, *b, &c) {}
        |""".stripMargin)

    inside(cpg.method.isLambda.l) {
      case lambdaLiteral :: Nil =>
        inside(lambdaLiteral.parameter.l) {
          case _ :: aParam :: bParam :: cParam :: Nil =>
            aParam.code shouldBe "a"
            bParam.code shouldBe "*b"
            cParam.code shouldBe "&c"
          case xs => fail(s"Expected four parameters, got [${xs.code.mkString(",")}]")
        }
      case xs => fail(s"Expected one lambda, got [${xs.name.mkString(",")}]")
    }
  }

}
