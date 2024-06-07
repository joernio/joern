package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
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

  "an implicit array constructor (Array::[]) should be lowered to an array initializer" in {
    val cpg = code("""
        |x = Array [1, 2, 3]
        |""".stripMargin)

    inside(cpg.call.nameExact("[]").l) {
      case bracketCall :: Nil =>
        bracketCall.name shouldBe "[]"
        bracketCall.methodFullName shouldBe s"$kernelPrefix.Array:[]"
        bracketCall.typeFullName shouldBe s"$kernelPrefix.Array"

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
