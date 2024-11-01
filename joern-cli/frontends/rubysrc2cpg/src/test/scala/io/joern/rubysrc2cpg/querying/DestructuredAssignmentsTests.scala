package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal}
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

  "destructuring of an unpaired multi-assignment with a splat operator on the LHS (slurping)" should {

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

  "destructuring of an unpaired multi-assignment with a splat operator on the RHS (splitting)" should {

    "assign c to `list` via a splat operator call (as we cannot always unpack `list`)" in {
      val cpg = code("""
                |list = [3, 4]
                |a, b, c = 1, 2, *list
                |""".stripMargin)

      inside(cpg.assignment.l) {
        case listAssignment :: aAssignment :: bAssignment :: cAssignment :: Nil =>
          listAssignment.code shouldBe "list = [3, 4]"
          aAssignment.code shouldBe "a, b, c = 1, 2, *list"
          bAssignment.code shouldBe "a, b, c = 1, 2, *list"
          cAssignment.code shouldBe "a, b, c = 1, 2, *list"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          two.code shouldBe "2"

          val List(c: Identifier, splat: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          splat.name shouldBe RubyOperators.splat
          inside(splat.argumentOut.l) {
            case (list: Identifier) :: Nil =>
              list.name shouldBe "list"
            case _ => fail("Unexpected number of array elements in `c`'s assignment")
          }

        case _ => fail("Unexpected number of assignments found")
      }

    }

    "assign both b & c to `list` via a splat operator call (as we cannot always unpack `list`)" in {
      val cpg = code("""
          |list = [3, 4]
          |a, b, c = 1, *list
          |""".stripMargin)

      inside(cpg.assignment.l) {
        case listAssignment :: aAssignment :: bAssignment :: cAssignment :: Nil =>
          listAssignment.code shouldBe "list = [3, 4]"
          aAssignment.code shouldBe "a, b, c = 1, *list"
          bAssignment.code shouldBe "a, b, c = 1, *list"
          cAssignment.code shouldBe "a, b, c = 1, *list"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, splatB: Call) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          splatB.name shouldBe RubyOperators.splat
          inside(splatB.argumentOut.l) {
            case (list: Identifier) :: Nil =>
              list.name shouldBe "list"
            case _ => fail("Unexpected number of array elements in `b`'s assignment")
          }

          val List(c: Identifier, splatC: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          splatC.name shouldBe RubyOperators.splat
          inside(splatC.argumentOut.l) {
            case (list: Identifier) :: Nil =>
              list.name shouldBe "list"
            case _ => fail("Unexpected number of array elements in `c`'s assignment")
          }

        case _ => fail("Unexpected number of assignments found")
      }

    }

  }

  "Destructered Assignment with splat in the middle" in {
    val cpg = code("""
        |a, *b, c = 1, 2, 3, 4, 5, 6
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignment).l) {
      case aAssignment :: bAssignment :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4, 5, 6"
        bAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4, 5, 6"
        cAssignment.code shouldBe "a, *b, c = 1, 2, 3, 4, 5, 6"

        val List(a: Identifier, lit: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        lit.code shouldBe "1"

        val List(splat: Identifier, arr: Call) = bAssignment.argumentOut.toList: @unchecked
        splat.name shouldBe "b"
        arr.name shouldBe Operators.arrayInitializer

        inside(arr.argumentOut.l) {
          case (two: Literal) :: (three: Literal) :: (four: Literal) :: (five: Literal) :: Nil =>
            two.code shouldBe "2"
            three.code shouldBe "3"
            four.code shouldBe "4"
            five.code shouldBe "5"
          case _ => fail("Unexpected number of array elements in `*`'s assignment")
        }

        val List(c: Identifier, cLiteral: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        cLiteral.code shouldBe "6"
      case xs => fail(s"Expected three assignments, got ${xs.code.mkString(",")}")
    }
  }

  "Destructured assignment with naked splat" in {
    val cpg = code("""
        |*, a = 1, 2, 3
        |""".stripMargin)

    inside(cpg.assignment.l) {
      case splatAssignment :: aAssignment :: Nil =>
        aAssignment.code shouldBe "*, a = 1, 2, 3"
        splatAssignment.code shouldBe "*, a = 1, 2, 3"

        val List(a: Identifier, lit: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        lit.code shouldBe "3"

        val List(splat: Identifier, arr: Call) = splatAssignment.argumentOut.toList: @unchecked
        splat.name shouldBe "_"
        arr.name shouldBe Operators.arrayInitializer
        inside(arr.argumentOut.l) {
          case (one: Literal) :: (two: Literal) :: Nil =>
            one.code shouldBe "1"
            two.code shouldBe "2"
          case _ => fail("Unexpected number of array elements in `*`'s assignment")
        }
      case _ => fail("Unexpected number of assignments found")
    }
  }

  "Destructured Assignment RHS" in {
    val cpg = code("""
        |a, *b, c = 1, 2, *d, *f, 4
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignment).l) {
      case aAssignment :: bAssignment :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a, *b, c = 1, 2, *d, *f, 4"
        bAssignment.code shouldBe "a, *b, c = 1, 2, *d, *f, 4"
        cAssignment.code shouldBe "a, *b, c = 1, 2, *d, *f, 4"

        val List(a: Identifier, aLiteral: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        aLiteral.code shouldBe "1"

        val List(splat: Identifier, arr: Call) = bAssignment.argumentOut.toList: @unchecked
        splat.name shouldBe "b"
        arr.name shouldBe Operators.arrayInitializer

        inside(arr.argumentOut.l) {
          case (two: Literal) :: (d: Call) :: (f: Call) :: Nil =>
            two.code shouldBe "2"

            d.code shouldBe "*d"
            d.methodFullName shouldBe RubyOperators.splat

            f.code shouldBe "*f"
            f.methodFullName shouldBe RubyOperators.splat

          case xs => fail(s"Unexpected number of array elements in `*`'s assignment, got ${xs.code.mkString(",")}")
        }

        val List(c: Identifier, cLiteral: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        cLiteral.code shouldBe "4"

      case xs => fail(s"Expected 3 assignments, got ${xs.code.mkString(",")}")
    }
  }

  "multi-assignments as a return value" should {

    val cpg = code("""
        |def f
        | a, b = 1, 2 # => return [1, 2]
        |end
        |""".stripMargin)

    "create an explicit return of the LHS values as an array" in {
      val arrayLiteral = cpg.method.name("f").methodReturn.toReturn.astChildren.isCall.head

      arrayLiteral.name shouldBe Operators.arrayInitializer
      arrayLiteral.methodFullName shouldBe Operators.arrayInitializer
      arrayLiteral.code shouldBe "a, b = 1, 2"

      arrayLiteral.astChildren.isIdentifier.code.l shouldBe List("a", "b")
    }

  }

}
