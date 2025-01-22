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

  "Global regex related variables" should {

    "be assigned to the match by the `~=` operator" in {

      val cpg = code("""
          |'hello' =~ /h(el)lo/
          |""".stripMargin)

      cpg.method.isModule.dotAst.foreach(println)
    }

    "be assigned to the match in a case equality" in {
      val cpg = code("""
          |case "hello"
          |when /h(el)lo/
          | puts $1
          |end
          |""".stripMargin)

      cpg.method.dotAst.foreach(println)
    }

    "be assigned to the match in a match call (regex lhs)" in {
      val cpg = code("""
          |/h(el)lo/.match("hello")
          |""".stripMargin)
    }

    "be assigned to the match in a match call (regex rhs)" in {
      val cpg = code("""
          |"hello".match(/h(el)lo/)
          |""".stripMargin)
    }

    "be assigned to the match of the default global string in a match call (no rhs)" in {
      val cpg = code("""
          |$_ = "hello"
          |/h(el)lo/ =~ # Match, updates $~, $1 = "el"
          |""".stripMargin)
    }

    // We can only approximate this if the regex is directly in the index, otherwise it becomes expensive to
    // perform constant propagation in order to determine all such cases
    "be assigned to the match using string indexing" in {
      val cpg = code("""
          |"hello"[/h(el)lo]
          |""".stripMargin)
    }

    "be assigned to the match using `sub` or `gsub` calls" in {
      val cpg = code("""
          |"hello".sub(/h(el)lo/)
          |""".stripMargin)
    }

  }

}
