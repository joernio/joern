package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Literal, Identifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class DestructuredAssignmentsTests extends RubyCode2CpgFixture {

  "destructuring of a paired multi-assignment" should {

    val cpg = code("""
                    |a, b, c = 1, 2, 3
                    |""".stripMargin)

    "separate the assigments into three separate assignment nodes" in {
      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "a, b, c = 1, 2, 3"
          bAssignment.code shouldBe "a, b, c = 1, 2, 3"
          cAssignment.code shouldBe "a, b, c = 1, 2, 3"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          two.code shouldBe "2"

          val List(c: Identifier, three: Literal) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          three.code shouldBe "3"
        case _ => fail("Unexpected number of assignments found")
      }

    }

  }

  "destructuring of an unpaired multi-assignment biased to LHS" should {

    val cpg = code("""
                    |a, b, c, d = 1, 2, 3
                    |""".stripMargin)

    "separate the assigments into 3 and leave `d` undefined" in {
      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "a, b, c, d = 1, 2, 3"
          bAssignment.code shouldBe "a, b, c, d = 1, 2, 3"
          cAssignment.code shouldBe "a, b, c, d = 1, 2, 3"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          two.code shouldBe "2"

          val List(c: Identifier, three: Literal) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          three.code shouldBe "3"
        case _ => fail("Unexpected number of assignments found")
      }

    }

  }

}
