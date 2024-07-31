package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.GlobalTypes.{builtinPrefix, kernelPrefix}
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language.*

class ArrayTests extends RubyCode2CpgFixture {

  "`[]` is represented by an `arrayInitializer` operator call" in {
    val cpg = code("""
                     |[]
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.l

    arrayCall.methodFullName shouldBe Operators.arrayInitializer
    arrayCall.code shouldBe "[]"
    arrayCall.lineNumber shouldBe Some(2)
  }

  "`[1]` is represented by an `arrayInitializer` operator call with arguments `1`" in {
    val cpg = code("""
                     |[1]
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.l

    arrayCall.methodFullName shouldBe Operators.arrayInitializer
    arrayCall.code shouldBe "[1]"
    arrayCall.lineNumber shouldBe Some(2)

    val List(one) = arrayCall.argument.l

    one.code shouldBe "1"
  }

  "`[1,2,]` is represented by an `arrayInitializer` operator call with arguments `1`, `2`" in {
    val cpg = code("""
                     |[1,2,]
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.l

    arrayCall.methodFullName shouldBe Operators.arrayInitializer
    arrayCall.code shouldBe "[1,2,]"
    arrayCall.lineNumber shouldBe Some(2)

    val List(one, two) = arrayCall.argument.l

    one.code shouldBe "1"
    two.code shouldBe "2"
  }

  "`%w{}` is represented by an `arrayInitializer` operator call" in {
    val cpg = code("""
                     |%w{}
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.l

    arrayCall.methodFullName shouldBe Operators.arrayInitializer
    arrayCall.code shouldBe "%w{}"
    arrayCall.lineNumber shouldBe Some(2)
    arrayCall.argument.isEmpty shouldBe true
  }

  "`%w?foo?` is represented by an `arrayInitializer` operator call with arguments 'foo'" in {
    val cpg = code("""
                     |%w?foo?
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.name(Operators.arrayInitializer).l

    arrayCall.code shouldBe "%w?foo?"
    arrayCall.lineNumber shouldBe Some(2)

    val List(foo) = arrayCall.argument.isLiteral.l
    foo.code shouldBe "foo"
    foo.typeFullName shouldBe s"$kernelPrefix.String"
  }

  "`%i(x y)` is represented by an `arrayInitializer` operator call with arguments `:x`, `:y`" in {
    val cpg = code("""
                     |%i(x y)
                     |""".stripMargin)

    val List(arrayCall) = cpg.call.name(Operators.arrayInitializer).l

    arrayCall.code shouldBe "%i(x y)"
    arrayCall.lineNumber shouldBe Some(2)

    val List(x, y) = arrayCall.argument.isLiteral.l
    x.code shouldBe "x"
    x.typeFullName shouldBe y.typeFullName

    y.code shouldBe "y"
    y.typeFullName shouldBe s"$kernelPrefix.Symbol"
  }

  "%W is represented an `arrayInitializer` operator call" in {
    val cpg = code("""%W(x#{1 + 3} y#{23} z)
        |""".stripMargin)

    val List(arrayCall) = cpg.call.name(Operators.arrayInitializer).l

    arrayCall.code shouldBe "%W(x#{1 + 3} y#{23} z)"
    arrayCall.lineNumber shouldBe Some(1)

    val List(xFmt, yFmt) = arrayCall.argument.isCall.l
    xFmt.name shouldBe Operators.formatString
    yFmt.name shouldBe Operators.formatString

    val List(xFmtStr) = xFmt.astChildren.isCall.l
    xFmtStr.name shouldBe Operators.formattedValue

    val List(xFmtStrAdd) = xFmtStr.astChildren.isCall.l
    xFmtStrAdd.name shouldBe Operators.addition

    val List(lhs, rhs) = xFmtStrAdd.argument.l
    lhs.code shouldBe "1"
    rhs.code shouldBe "3"

    val List(yFmtStr) = yFmt.astChildren.isCall.l
    yFmtStr.name shouldBe Operators.formattedValue

    val List(yFmtStrLit) = yFmtStr.argument.l
    yFmtStrLit.code shouldBe "23"

    val List(zLit) = arrayCall.argument.isLiteral.l
    zLit.code shouldBe "z"
  }

  "an implicit array constructor (Array::[]) should be lowered to an array initializer" in {
    val cpg = code("""
        |x = Array [1, 2, 3]
        |""".stripMargin)

    inside(cpg.call.nameExact("[]").l) {
      case bracketCall :: Nil =>
        bracketCall.name shouldBe "[]"
        bracketCall.methodFullName shouldBe s"$builtinPrefix.Array.[]"
        bracketCall.typeFullName shouldBe s"$builtinPrefix.Array"

        inside(bracketCall.argument.l) {
          case _ :: one :: two :: three :: Nil =>
            one.code shouldBe "1"
            two.code shouldBe "2"
            three.code shouldBe "3"
          case xs => fail(s"Expected 3 literals under the array initializer, instead got [${xs.code.mkString(", ")}]")
        }
      case xs => fail(s"Expected a single array initializer call ([]), instead got [${xs.code.mkString(", ")}]")
    }

  }

}
