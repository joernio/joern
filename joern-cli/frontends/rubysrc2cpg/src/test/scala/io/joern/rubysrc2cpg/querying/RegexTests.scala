package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{Cpg, Operators}
import io.shiftleft.semanticcpg.language.*

class RegexTests extends RubyCode2CpgFixture(withPostProcessing = false) {

  "Global regex related variables" should {

    /** Checks for the presence of the lowered regex match which assigns the match results to the respective global
      * variables.
      *
      * TODO: Check for matching of match group ($1, $2, etc.) variables.
      */
    def assertLoweredStructure(cpg: Cpg, tmpNo: String = "0", expectedSubject: String = "\"hello\""): Unit = {
      // We lower =~ to the `match` equivalent
      val tmpInit = cpg.assignment.code(s"<tmp-$tmpNo> =.*").head

      val tmpTarget = tmpInit.target.asInstanceOf[Identifier]
      tmpTarget.name shouldBe s"<tmp-$tmpNo>"
      val tmpSource = tmpInit.source.asInstanceOf[Call]
      tmpSource.code shouldBe s"/h(el)lo/.match($expectedSubject)"
      tmpSource.name shouldBe "match"
      tmpSource.methodFullName shouldBe "__core.Regexp.match"

      // Now test for the lowered global variable assignments
      val ifStmt = cpg.controlStructure.last
      inside(ifStmt.whenTrue.assignment.l) { case tildeAsgn :: amperAsgn :: match1Asgn :: Nil =>
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

        match1Asgn.code shouldBe s"$$1 = <tmp-$tmpNo>[1]"
        val match1AsgnSource = match1Asgn.source.asInstanceOf[Call]
        match1AsgnSource.methodFullName shouldBe Operators.indexAccess
        match1AsgnSource.code shouldBe s"<tmp-$tmpNo>[1]"

        val match1AsgnTarget = match1Asgn.target.asInstanceOf[Call]
        match1AsgnTarget.methodFullName shouldBe Operators.indexAccess
        match1AsgnTarget.code shouldBe "$[1]"
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

      assertLoweredStructure(cpg, "1", "<tmp-0>")
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
