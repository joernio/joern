package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RubyDefines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class LiteralTests extends RubyCode2CpgFixture {

  "`123` is represented by a LITERAL node" in {
    val cpg = code("""
        |123
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "123"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")
  }

  "`3.14` is represented by a LITERAL node" in {
    val cpg = code("""
        |3.14
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3.14"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Float")
  }

  "`3e10` is represented by a LITERAL node" in {
    val cpg = code("""
        |3e10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "3e10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Float")
  }

  "`12e-10` is represented by a LITERAL node" in {
    val cpg = code("""
        |12e-10
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "12e-10"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Float")
  }

  "`0b01` is represented by a LITERAL node" in {
    val cpg = code("""
        |0b01
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0b01"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")
  }

  "`0xabc` is represented by a LITERAL node" in {
    val cpg = code("""
        |0xabc
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "0xabc"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Integer")
  }

  "`true` is represented by a LITERAL node" in {
    val cpg = code("""
        |true
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "true"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("TrueClass")
  }

  "`false` is represented by a LITERAL node" in {
    val cpg = code("""
        |false
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "false"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("FalseClass")
  }

  "`nil` is represented by a LITERAL node" in {
    val cpg = code("""
        |nil
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "nil"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("NilClass")
  }

  "`'hello'` is represented by a LITERAL node" in {
    val cpg = code("""
        |'hello'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "'hello'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  "`'x' 'y' 'z'` is represented by a dynamic literal node call" in {
    val cpg = code("""
        |'x' 'y' 'z'
        |""".stripMargin)

    val List(dynamicLitCall) = cpg.call.methodFullNameExact(Operators.formatString).l: @unchecked
    dynamicLitCall.code shouldBe "'x' 'y' 'z'"
    dynamicLitCall.methodFullName shouldBe Operators.formatString

    inside(dynamicLitCall.argument.astChildren.l) { case (x: Literal) :: (y: Literal) :: (z: Literal) :: Nil =>
      x.code shouldBe "'x'"
      x.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
      y.code shouldBe "'y'"
      y.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
      z.code shouldBe "'z'"
      z.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
    }
  }

  "`\"hello\"` is represented by a LITERAL node" in {
    val cpg = code("""
        |"hello"
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "\"hello\""
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  "`%q(hello)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%q(hello)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%q(hello)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  "`%Q(hello world)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%Q(hello world)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%Q(hello world)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  "`%(foo \"bar\" baz)` is represented by a LITERAL node" in {
    val cpg = code("""
        |%(foo "bar" baz)
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "%(foo \"bar\" baz)"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  """`%q<\n...\n>` is represented by a LITERAL node""" in {
    val cpg = code("""
        |%q<
        |xyz
        |123
        |>
        |""".stripMargin)

    val List(firstLine, xyz, one23) = cpg.literal.l
    firstLine.code.trim shouldBe ""
    firstLine.lineNumber shouldBe Some(2)
    xyz.code.trim shouldBe "xyz"
    xyz.lineNumber shouldBe Some(3)
    xyz.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
    one23.code.trim shouldBe "123"
    one23.lineNumber shouldBe Some(4)
    one23.typeFullName shouldBe RubyDefines.prefixAsCoreType("String")
  }

  "`:symbol` is represented by a LITERAL node" in {
    val cpg = code("""
        |:symbol
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":symbol"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Symbol")
  }

  "`:'symbol'` is represented by a LITERAL node" in {
    val cpg = code("""
        |:'symbol'
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe ":'symbol'"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Symbol")
  }

  "`/(eu|us)/` is represented by a LITERAL node" in {
    val cpg = code("""
        |/(eu|us)/
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "/(eu|us)/"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Regexp")
  }

  "`/fedora|el-|centos/` is represented by a LITERAL node" in {
    val cpg = code("""
        |/fedora|el-|centos/
        |""".stripMargin)

    val List(literal) = cpg.literal.l
    literal.code shouldBe "/fedora|el-|centos/"
    literal.lineNumber shouldBe Some(2)
    literal.typeFullName shouldBe RubyDefines.prefixAsCoreType("Regexp")
  }

  "`/#{os_version_regex}/` is represented by a CALL node with a string format method full name" in {
    val cpg = code("""
        |os_version_regex = "1.2.0"
        |/#{os_version_regex}/
        |""".stripMargin)

    val List(formatValueCall) = cpg.call.code("/#.*").l
    formatValueCall.code shouldBe "/#{os_version_regex}/"
    formatValueCall.lineNumber shouldBe Some(3)
    formatValueCall.typeFullName shouldBe RubyDefines.prefixAsCoreType("Regexp")
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
