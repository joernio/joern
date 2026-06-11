package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal}
import io.shiftleft.semanticcpg.language.*

class DestructuredAssignmentsTests extends RubyCode2CpgFixture {

  "destructuring of a paired multi-assignment" should {

    val cpg = code("""
                    |a, b, c = 1, 2, 3
                    |""".stripMargin)

    "separate the assignments into three separate assignment nodes" in {
      inside(cpg.assignment.l) { case aAssignment :: bAssignment :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a = 1"
        bAssignment.code shouldBe "b = 2"
        cAssignment.code shouldBe "c = 3"

        val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        one.code shouldBe "1"

        val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
        b.name shouldBe "b"
        two.code shouldBe "2"

        val List(c: Identifier, three: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        three.code shouldBe "3"
      }

    }

  }

  "destructuring of an unpaired multi-assignment biased to LHS" should {

    val cpg = code("""
                    |a, b, c, d = 1, 2, 3
                    |""".stripMargin)

    "separate the assignments into 3 and leave `d` undefined" in {
      inside(cpg.assignment.l) { case aAssignment :: bAssignment :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a = 1"
        bAssignment.code shouldBe "b = 2"
        cAssignment.code shouldBe "c = 3"

        val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        one.code shouldBe "1"

        val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
        b.name shouldBe "b"
        two.code shouldBe "2"

        val List(c: Identifier, three: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        three.code shouldBe "3"
      }

    }

  }

  "destructuring of an unpaired multi-assignment with a splat operator on the LHS (slurping)" should {

    "create an array for the LHS variable slurping on the right of the group" in {
      val cpg = code("""
                |a, b, *c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) { case aAssignment :: bAssignment :: cAssignment :: _ :: _ :: _ :: _ :: Nil =>
        aAssignment.code shouldBe "a = 1"
        bAssignment.code shouldBe "b = 2"
        cAssignment.code shouldBe "c = [3, 4]"

        val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        one.code shouldBe "1"

        val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
        b.name shouldBe "b"
        two.code shouldBe "2"

        val List(c: Identifier, arr: Block) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        arr.code shouldBe "[3, 4]"

        val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
        inside(asgns.map(_.source)) { case (three: Literal) :: (four: Literal) :: Nil =>
          three.code shouldBe "3"
          four.code shouldBe "4"
        }

      }

    }

    "create an array for the LHS variable slurping in the middle of the group" in {
      val cpg = code("""
                |a, *b, c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) { case aAssignment :: bAssignment :: _ :: _ :: _ :: _ :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a = 1"
        bAssignment.code shouldBe "b = [2, 3]"
        cAssignment.code shouldBe "c = 4"

        val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        one.code shouldBe "1"

        val List(b: Identifier, arr: Block) = bAssignment.argumentOut.toList: @unchecked
        b.name shouldBe "b"
        arr.code shouldBe "[2, 3]"

        val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
        inside(asgns.map(_.source)) { case (two: Literal) :: (three: Literal) :: Nil =>
          two.code shouldBe "2"
          three.code shouldBe "3"
        }

        val List(c: Identifier, four: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        four.code shouldBe "4"

      }

    }

    "create an array for the LHS variable slurping on the left of the group" in {
      val cpg = code("""
                |*a, b, c = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.l) { case aAssignment :: _ :: _ :: _ :: _ :: bAssignment :: cAssignment :: Nil =>
        aAssignment.code shouldBe "a = [1, 2]"
        bAssignment.code shouldBe "b = 3"
        cAssignment.code shouldBe "c = 4"

        val List(a: Identifier, arr: Block) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        arr.code shouldBe "[1, 2]"

        val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
        inside(asgns.map(_.source)) { case (one: Literal) :: (two: Literal) :: Nil =>
          one.code shouldBe "1"
          two.code shouldBe "2"
        }

        val List(b: Identifier, three: Literal) = bAssignment.argumentOut.toList: @unchecked
        b.name shouldBe "b"
        three.code shouldBe "3"

        val List(c: Identifier, four: Literal) = cAssignment.argumentOut.toList: @unchecked
        c.name shouldBe "c"
        four.code shouldBe "4"

      }

    }

    "create an array for the LHS variable implicitly slurping the RHS" in {
      val cpg = code("""
                |a = 1, 2, 3, 4
                |""".stripMargin)

      inside(cpg.assignment.codeExact("a = 1, 2, 3, 4").l) { case aAssignment :: Nil =>
        aAssignment.code shouldBe "a = 1, 2, 3, 4"

        val List(a: Identifier, arr: Block) = aAssignment.argumentOut.toList: @unchecked
        a.name shouldBe "a"
        arr.code shouldBe "1, 2, 3, 4"
        val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
        inside(asgns.map(_.source)) {
          case (one: Literal) :: (two: Literal) :: (three: Literal) :: (four: Literal) :: Nil =>
            one.code shouldBe "1"
            two.code shouldBe "2"
            three.code shouldBe "3"
            four.code shouldBe "4"
        }

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
        case listAssignment :: _ :: _ :: _ :: _ :: aAssignment :: bAssignment :: cAssignment :: Nil =>
          listAssignment.code shouldBe "list = [3, 4]"
          aAssignment.code shouldBe "a = 1"
          bAssignment.code shouldBe "b = 2"
          cAssignment.code shouldBe "c = *list"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, two: Literal) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          two.code shouldBe "2"

          val List(c: Identifier, splat: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          splat.name shouldBe RubyOperators.splat
          inside(splat.argumentOut.l) { case (list: Identifier) :: Nil =>
            list.name shouldBe "list"
          }

      }

    }

    "assign both b & c to `list` via a splat operator call (as we cannot always unpack `list`)" in {
      val cpg = code("""
          |list = [3, 4]
          |a, b, c = 1, *list
          |""".stripMargin)

      inside(cpg.assignment.l) {
        case listAssignment :: _ :: _ :: _ :: _ :: aAssignment :: bAssignment :: cAssignment :: Nil =>
          listAssignment.code shouldBe "list = [3, 4]"
          aAssignment.code shouldBe "a = 1"
          bAssignment.code shouldBe "b = list"
          cAssignment.code shouldBe "c = list"

          val List(a: Identifier, one: Literal) = aAssignment.argumentOut.toList: @unchecked
          a.name shouldBe "a"
          one.code shouldBe "1"

          val List(b: Identifier, splatB: Call) = bAssignment.argumentOut.toList: @unchecked
          b.name shouldBe "b"
          splatB.name shouldBe RubyOperators.splat
          inside(splatB.argumentOut.l) { case (list: Identifier) :: Nil =>
            list.name shouldBe "list"
          }

          val List(c: Identifier, splatC: Call) = cAssignment.argumentOut.toList: @unchecked
          c.name shouldBe "c"
          splatC.name shouldBe RubyOperators.splat
          inside(splatC.argumentOut.l) { case (list: Identifier) :: Nil =>
            list.name shouldBe "list"
          }

      }

    }

  }

  "Destructured Assignment with splat in the middle" in {
    val cpg = code("""
        |a, *b, c = 1, 2, 3, 4, 5, 6
        |""".stripMargin)

    inside(cpg.assignment.l) { case aAssignment :: bAssignment :: _ :: _ :: _ :: _ :: _ :: _ :: cAssignment :: Nil =>
      aAssignment.code shouldBe "a = 1"
      bAssignment.code shouldBe "b = [2, 3, 4, 5]"
      cAssignment.code shouldBe "c = 6"

      val List(a: Identifier, lit: Literal) = aAssignment.argumentOut.toList: @unchecked
      a.name shouldBe "a"
      lit.code shouldBe "1"

      val List(splat: Identifier, arr: Block) = bAssignment.argumentOut.toList: @unchecked
      splat.name shouldBe "b"
      arr.code shouldBe "[2, 3, 4, 5]"
      val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
      inside(asgns.map(_.source)) {
        case (two: Literal) :: (three: Literal) :: (four: Literal) :: (five: Literal) :: Nil =>
          two.code shouldBe "2"
          three.code shouldBe "3"
          four.code shouldBe "4"
          five.code shouldBe "5"
      }

      val List(c: Identifier, cLiteral: Literal) = cAssignment.argumentOut.toList: @unchecked
      c.name shouldBe "c"
      cLiteral.code shouldBe "6"
    }
  }

  "Destructured assignment with naked splat" in {
    val cpg = code("""
        |*, a = 1, 2, 3
        |""".stripMargin)

    inside(cpg.assignment.l) { case splatAssignment :: _ :: _ :: _ :: _ :: aAssignment :: Nil =>
      splatAssignment.code shouldBe "_ = [1, 2]"
      aAssignment.code shouldBe "a = 3"

      val List(a: Identifier, lit: Literal) = aAssignment.argumentOut.toList: @unchecked
      a.name shouldBe "a"
      lit.code shouldBe "3"

      val List(splat: Identifier, arr: Block) = splatAssignment.argumentOut.toList: @unchecked
      splat.name shouldBe "_"
      arr.code shouldBe "[1, 2]"
      val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
      inside(asgns.map(_.source)) { case (one: Literal) :: (two: Literal) :: Nil =>
        one.code shouldBe "1"
        two.code shouldBe "2"
      }
    }
  }

  "Destructured Assignment RHS" in {
    val cpg = code("""
        |a, *b, c = 1, 2, *d, *f, 4
        |""".stripMargin)

    inside(cpg.assignment.l) { case aAssignment :: bAssignment :: _ :: _ :: _ :: _ :: _ :: cAssignment :: Nil =>
      aAssignment.code shouldBe "a = 1"
      bAssignment.code shouldBe "b = [2, *d, *f]"
      cAssignment.code shouldBe "c = 4"

      val List(a: Identifier, aLiteral: Literal) = aAssignment.argumentOut.toList: @unchecked
      a.name shouldBe "a"
      aLiteral.code shouldBe "1"

      val List(splat: Identifier, arr: Block) = bAssignment.argumentOut.toList: @unchecked
      splat.name shouldBe "b"
      arr.code shouldBe "[2, *d, *f]"

      val asgns = arr.astChildren.assignment.where(_.target.isCall.nameExact(Operators.indexAccess)).l
      inside(asgns.map(_.source)) { case (two: Literal) :: (d: Call) :: (f: Call) :: Nil =>
        two.code shouldBe "2"

        d.code shouldBe "*d"
        d.methodFullName shouldBe RubyOperators.splat

        f.code shouldBe "*f"
        f.methodFullName shouldBe RubyOperators.splat

      }

      val List(c: Identifier, cLiteral: Literal) = cAssignment.argumentOut.toList: @unchecked
      c.name shouldBe "c"
      cLiteral.code shouldBe "4"

    }
  }

  "multi-assignments as a return value" should {

    val cpg = code("""
        |def f
        | a, b = 1, 2 # => return [1, 2]
        |end
        |""".stripMargin)

    "create an explicit return of the LHS values as an array" in {
      inside(cpg.method.name("f").methodReturn.toReturn.astChildren.isBlock.l) { case retBlock :: Nil =>
        retBlock.code shouldBe "a, b = 1, 2"

        inside(retBlock.astChildren.isCall.nameExact(Operators.assignment).l) {
          case aAssign :: bAssign :: arrayAlloc :: arrayElem0 :: arrayElem1 :: Nil =>
            aAssign.code shouldBe "a = 1"
            bAssign.code shouldBe "b = 2"

            arrayAlloc.code shouldBe s"<tmp-0> = ${Operators.alloc}"
            arrayAlloc.argument(1).code shouldBe "<tmp-0>"
            arrayAlloc.argument(2).code shouldBe Operators.alloc

            arrayElem0.code shouldBe "<tmp-0>[0] = a"
            arrayElem0.argument(1).code shouldBe "<tmp-0>[0]"
            arrayElem0.argument(2).code shouldBe "a"

            arrayElem1.code shouldBe "<tmp-0>[1] = b"
            arrayElem1.argument(1).code shouldBe "<tmp-0>[1]"
            arrayElem1.argument(2).code shouldBe "b"
        }

        inside(retBlock.astChildren.l.last) { case returnedArray: Identifier =>
          returnedArray.code shouldBe "<tmp-0>"
        }
      }
    }

  }

  "multi-assignments with complex LHS as a return value" should {
    val cpg = code("""
        |def f(arr)
        | arr[0], arr[1] = 1, 2
        |end
        |""".stripMargin)

    "use temporary variables for the complex LHS expressions" in {
      inside(cpg.method.name("f").methodReturn.toReturn.astChildren.isBlock.l) { case retBlock :: Nil =>
        retBlock.code shouldBe "arr[0], arr[1] = 1, 2"

        inside(retBlock.astChildren.isCall.nameExact(Operators.assignment).l) {
          case rhsToTmp0 :: lhsFromTmp0 :: rhsToTmp1 :: lhsFromTmp1 :: arrayAlloc :: arrayElem0 :: arrayElem1 :: Nil =>
            rhsToTmp0.code shouldBe "<tmp-0> = 1"
            lhsFromTmp0.code shouldBe "arr[0] = <tmp-0>"

            rhsToTmp1.code shouldBe "<tmp-1> = 2"
            lhsFromTmp1.code shouldBe "arr[1] = <tmp-1>"

            arrayAlloc.code shouldBe s"<tmp-2> = ${Operators.alloc}"
            arrayAlloc.argument(1).code shouldBe "<tmp-2>"
            arrayAlloc.argument(2).code shouldBe Operators.alloc

            arrayElem0.code shouldBe "<tmp-2>[0] = <tmp-0>"
            arrayElem0.argument(1).code shouldBe "<tmp-2>[0]"
            arrayElem0.argument(2).code shouldBe "<tmp-0>"

            arrayElem1.code shouldBe "<tmp-2>[1] = <tmp-1>"
            arrayElem1.argument(1).code shouldBe "<tmp-2>[1]"
            arrayElem1.argument(2).code shouldBe "<tmp-1>"
        }

        inside(retBlock.astChildren.l.last) { case returnedArray: Identifier =>
          returnedArray.code shouldBe "<tmp-2>"
        }
      }
    }
  }
}
