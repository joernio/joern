package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
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

    /** Checks for the presence of the lowered regex match which assigns the match results to the respective global
      * variables.
      *
      * TODO: Check for matching of match group ($1, $2, etc.) variables.
      */
    def assertLoweredStructure(cpg: Cpg, tmpNo: String = "0"): Unit = {
      // We lower =~ to the `match` equivalent
      val tmpInit = cpg.assignment.code(s"<tmp-$tmpNo> =.*").head

      val tmpTarget = tmpInit.target.asInstanceOf[Identifier]
      tmpTarget.name shouldBe s"<tmp-$tmpNo>"
      val tmpSource = tmpInit.source.asInstanceOf[Call]
      tmpSource.code shouldBe "\"hello\".match(/h(el)lo/)"
      tmpSource.name shouldBe "match"

      // Now test for the lowered global variable assignments
      val ifStmt = cpg.controlStructure.head
      inside(ifStmt.whenTrue.assignment.l) { case tildeAsgn :: amperAsgn :: Nil =>
        tildeAsgn.code shouldBe s"$$~ = <tmp-$tmpNo>"
        val taSource = tildeAsgn.source.asInstanceOf[Identifier]
        taSource.name shouldBe s"<tmp-$tmpNo>"
        val taTarget = tildeAsgn.target.asInstanceOf[Call]
        taTarget.methodFullName shouldBe Operators.fieldAccess
        taTarget.code shouldBe "self.$~"

        amperAsgn.code shouldBe s"$$& = <tmp-$tmpNo>[0]"
        val aaSource = amperAsgn.source.asInstanceOf[Call]
        aaSource.methodFullName shouldBe Operators.indexAccess
        aaSource.code shouldBe s"<tmp-$tmpNo>[0]"
        aaSource.argument(1).asInstanceOf[Identifier].name shouldBe s"<tmp-$tmpNo>"
        aaSource.argument(2).asInstanceOf[Literal].code shouldBe "0"

        val aaTarget = amperAsgn.target.asInstanceOf[Call]
        aaTarget.methodFullName shouldBe Operators.fieldAccess
        aaTarget.code shouldBe "self.$&"
      }
      inside(ifStmt.whenFalse.assignment.l) { case tildeAsgn :: amperAsgn :: Nil =>
        tildeAsgn.code shouldBe "$~ = nil"
        val taSource = tildeAsgn.source.asInstanceOf[Literal]
        taSource.code shouldBe "nil"
        val taTarget = tildeAsgn.target.asInstanceOf[Call]
        taTarget.methodFullName shouldBe Operators.fieldAccess
        taTarget.code shouldBe "self.$~"

        amperAsgn.code shouldBe "$& = nil"
        val aaSource = amperAsgn.source.asInstanceOf[Literal]
        aaSource.code shouldBe "nil"

        val aaTarget = amperAsgn.target.asInstanceOf[Call]
        aaTarget.methodFullName shouldBe Operators.fieldAccess
        aaTarget.code shouldBe "self.$&"
      }
    }

    "be assigned to the match by the `~=` operator" in {

      val cpg = code("""
          |"hello" =~ /h(el)lo/
          |""".stripMargin)

      assertLoweredStructure(cpg)
    }

    "be assigned to the match in a case equality" in {
      val cpg = code("""
          |case "hello"
          |when /h(el)lo/
          | puts $1
          |end
          |""".stripMargin)

      assertLoweredStructure(cpg, "1")
    }

    "be assigned to the match in a match call (regex lhs)" in {
      val cpg = code("""
          |/h(el)lo/.match("hello")
          |""".stripMargin)

      assertLoweredStructure(cpg)
    }

    "be assigned to the match in a match call (regex rhs)" in {
      val cpg = code("""
          |"hello".match(/h(el)lo/)
          |""".stripMargin)

      assertLoweredStructure(cpg)
    }

    "be assigned to the match using string indexing" in {
      val cpg = code("""
          |"hello"[/h(el)lo/]
          |""".stripMargin)

      assertLoweredStructure(cpg)
    }

    "be assigned to the match using `sub` (or `gsub`) calls" in {
      val cpg = code("""
          |"hello".sub(/h(el)lo/)
          |""".stripMargin)

      assertLoweredStructure(cpg)
    }

  }

}
