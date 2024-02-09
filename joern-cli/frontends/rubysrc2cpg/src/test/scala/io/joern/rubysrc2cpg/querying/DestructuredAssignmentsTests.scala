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

    "separate the assignments into three separate assignment nodes" in {
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

    "separate the assignments into 3 and leave `d` undefined" in {
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

  "destructuring of an paired multi-assignment with a splat operator on ths LHS (slurping)" should {

    "create an array for the LHS variable slurping on the right of the group" in {
      val cpg = code("""
                |a, b, *c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "a, b, *c = 1, 2, 3, 4"
          bAssignment.code shouldBe "a, b, *c = 1, 2, 3, 4"
          cAssignment.code shouldBe "a, b, *c = 1, 2, 3, 4"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          two.code shouldBe "2"

          val List(c: Identifier, arr: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          arr.name shouldBe Operators.arrayInitializer
          inside(arr.argumentOut.l) {
            case (three: Literal) :: (four: Literal) :: Nil =>
              three.code shouldBe "3"
              four.code shouldBe "4"
            case _ => fail("Unexpected number of array elements in `c`'s assignment")
          }

        case _ => fail("Unexpected number of assignments found")
      }

    }

    "create an array for the LHS variable slurping in the middle of the group" in {
      val cpg = code("""
                |a, *b, c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4"
          bAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4"
          cAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, arr: Call) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          arr.name shouldBe Operators.arrayInitializer
          inside(arr.argumentOut.l) {
            case (two: Literal) :: (three: Literal) :: Nil =>
              two.code shouldBe "2"
              three.code shouldBe "3"
            case _ => fail("Unexpected number of array elements in `b`'s assignment")
          }

          val List(c: Identifier, four: Literal) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          four.code shouldBe "4"

        case _ => fail("Unexpected number of assignments found")
      }

    }

    "create an array for the LHS variable slurping on the left of the group" in {
      val cpg = code("""
                |*a, b, c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "*a, b, c = 1, 2, 3, 4"
          bAssignment.code shouldBe "*a, b, c = 1, 2, 3, 4"
          cAssignment.code shouldBe "*a, b, c = 1, 2, 3, 4"

          val List(a: Identifier, arr: Call) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          arr.name shouldBe Operators.arrayInitializer
          inside(arr.argumentOut.l) {
            case (one: Literal) :: (two: Literal) :: Nil =>
              one.code shouldBe "1"
              two.code shouldBe "2"
            case _ => fail("Unexpected number of array elements in `a`'s assignment")
          }

          val List(b: Identifier, three: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          three.code shouldBe "3"

          val List(c: Identifier, four: Literal) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          four.code shouldBe "4"

        case _ => fail("Unexpected number of assignments found")
      }

    }

    "create two arrays for the LHS variable slurping on the left and right of the group" in {
      val cpg = code("""
                |*a, b, *c = 1, 2, 3, 4, 5
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case aAssignment :: bAssignment :: cAssignment :: Nil =>
          aAssignment.code shouldBe "*a, b, *c = 1, 2, 3, 4, 5"
          bAssignment.code shouldBe "*a, b, *c = 1, 2, 3, 4, 5"
          cAssignment.code shouldBe "*a, b, *c = 1, 2, 3, 4, 5"

          val List(a: Identifier, arrA: Call) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          arrA.name shouldBe Operators.arrayInitializer
          inside(arrA.argumentOut.l) {
            case (one: Literal) :: (two: Literal) :: Nil =>
              one.code shouldBe "1"
              two.code shouldBe "2"
            case _ => fail("Unexpected number of array elements in `a`'s assignment")
          }

          val List(b: Identifier, three: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          three.code shouldBe "3"

          val List(c: Identifier, arrC: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          arrC.name shouldBe Operators.arrayInitializer
          inside(arrC.argumentOut.l) {
            case (four: Literal) :: (five: Literal) :: Nil =>
              four.code shouldBe "4"
              five.code shouldBe "5"
            case _ => fail("Unexpected number of array elements in `c`'s assignment")
          }

        case _ => fail("Unexpected number of assignments found")
      }

    }

    "create an array for the LHS variable implicitly slurping the RHS" in {
      val cpg = code("""
                |a = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case aAssignment :: Nil =>
          aAssignment.code shouldBe "a = 1, 2, 3, 4"

          val List(a: Identifier, arr: Call) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          arr.name shouldBe Operators.arrayInitializer
          inside(arr.argumentOut.l) {
            case (one: Literal) :: (two: Literal) :: (three: Literal) :: (four: Literal) :: Nil =>
              one.code shouldBe "1"
              two.code shouldBe "2"
              three.code shouldBe "3"
              four.code shouldBe "4"
            case _ => fail("Unexpected number of array elements in `a`'s assignment")
          }

        case _ => fail("Unexpected number of assignments found")
      }

    }

  }

}
