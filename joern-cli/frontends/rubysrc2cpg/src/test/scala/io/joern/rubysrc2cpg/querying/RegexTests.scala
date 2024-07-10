package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.Literal
import io.shiftleft.semanticcpg.language.*

class RegexTests extends RubyCode2CpgFixture(withPostProcessing = true) {
  "`'x' =~ y` is a member call `'x'.=~ /y/" in {
    val cpg = code("""|'x' =~ /y/
       |0
       |""".stripMargin)
    cpg.call(RubyOperators.regexpMatch).methodFullName.l shouldBe List(
      s"$kernelPrefix.String.${RubyOperators.regexpMatch}"
    )
  }
  "`/x/ =~ 'y'` is a member call `/x/.=~ 'y'" in {
    val cpg = code("""|/x/ =~ 'y'
       |0
       |""".stripMargin)
    cpg.call(RubyOperators.regexpMatch).methodFullName.l shouldBe List(
      s"$kernelPrefix.Regexp.${RubyOperators.regexpMatch}"
    )
  }

  "Regex expression in if statements" in {
    val cpg = code("""
        |
        |if /mswin|mingw|cygwin/ =~ "mswin"
        |end
        |""".stripMargin)

    inside(cpg.controlStructure.isIf.l) {
      case regexIf :: Nil =>
        regexIf.condition.isCall.methodFullName.l shouldBe List(s"$kernelPrefix.Regexp.${RubyOperators.regexpMatch}")

        inside(regexIf.condition.isCall.argument.l) {
          case (lhs: Literal) :: (rhs: Literal) :: Nil =>
            lhs.code shouldBe "/mswin|mingw|cygwin/"
            lhs.typeFullName shouldBe s"$kernelPrefix.Regexp"

            rhs.code shouldBe "\"mswin\""
            rhs.typeFullName shouldBe s"$kernelPrefix.String"
          case xs => fail(s"Expected two arguments, got [${xs.code.mkString(",")}]")
        }

      case xs => fail(s"One if statement expected, got [${xs.code.mkString(",")}]")
    }
  }
}
