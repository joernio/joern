package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class SingleAssignmentTests extends RubyCode2CpgFixture {

  "`=` is represented by an `assignment` operator call" in {
    val cpg = code("""
                     |x = 1
                     |""".stripMargin)

    val List(assignment) = cpg.assignment.l
    assignment.code shouldBe "x = 1"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    lhs.order shouldBe 1
    lhs.argumentIndex shouldBe 1

    rhs.code shouldBe "1"
    rhs.order shouldBe 2
    rhs.argumentIndex shouldBe 2
  }

  "`+=` is represented by an `assignmentPlus` operator call" in {
    val cpg = code("""
                     |x += 1
                     |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentPlus).l
    assignment.code shouldBe "x += 1"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "1"
  }

  "`||=` is represented by an `assignmentOr` operator call" in {
    val cpg = code("""
        |x ||= false
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentOr).l
    assignment.code shouldBe "x ||= false"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "false"
  }

  "`&&=` is represented by an `assignmentAnd` operator call" in {
    val cpg = code("""
        |x &&= true
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentAnd).l
    assignment.code shouldBe "x &&= true"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "true"
  }

  "`/=` is represented by an `assignmentDivision` operator call" in {
    val cpg = code("""
        |x /= 10
        |""".stripMargin)

    val List(assignment) = cpg.call(Operators.assignmentDivision).l
    assignment.code shouldBe "x /= 10"
    assignment.lineNumber shouldBe Some(2)
    assignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val List(lhs, rhs) = assignment.argument.l
    lhs.code shouldBe "x"
    rhs.code shouldBe "10"
  }

  "`=` is right-associative" in {
    val cpg = code("""
                     |x = y = 1
                     |""".stripMargin)

    val List(xAssignment, yAssignment) = cpg.assignment.l
    xAssignment.code shouldBe "x = y = 1"
    yAssignment.code shouldBe "y = 1"

    val List(x, yAssignment_) = xAssignment.argument.l
    yAssignment_ shouldBe yAssignment

    val List(y, one) = yAssignment.argument.l
    x.code shouldBe "x"
    y.code shouldBe "y"
    one.code shouldBe "1"
  }

  "`=` binds tighter than `or`" in {
    val cpg = code("""
                     |x = 1 or 2
                     |""".stripMargin)

    val List(or)                = cpg.call(Operators.logicalOr).l
    val List(xAssignment)       = cpg.call(Operators.assignment).l
    val List(xAssignment_, two) = or.argument.l

    xAssignment shouldBe xAssignment_
    xAssignment_.code shouldBe "x = 1"
    or.code shouldBe "x = 1 or 2"
    two.code shouldBe "2"
  }

  "`if-else-end` on the RHS of an assignment" in {
    val cpg = code("""
        |x = if true then 20 else 40 end
        |""".stripMargin)

    val List(assignmentIfBranch, assignmentElseBranch) = cpg.assignment.l

    val rhsIfBranchIdentifier = assignmentIfBranch.argument(1).asInstanceOf[Identifier]
    val rhsIfBranchValue      = assignmentIfBranch.argument(2).asInstanceOf[Literal]

    val rhsElseBranchIdentifier = assignmentElseBranch.argument(1).asInstanceOf[Identifier]
    val rhsElseBranchValue      = assignmentElseBranch.argument(2).asInstanceOf[Literal]

    assignmentIfBranch.methodFullName shouldBe Operators.assignment
    rhsIfBranchIdentifier.code shouldBe "x"
    rhsIfBranchValue.code shouldBe "20"

    assignmentElseBranch.methodFullName shouldBe Operators.assignment
    rhsElseBranchIdentifier.code shouldBe "x"
    rhsElseBranchValue.code shouldBe "40"
  }

  "nested if-else-end on the RHS of an assignment" in {
    val cpg = code("""
      |x = if true
      |  if true
      |    1
      |  else
      |    2
      |  end
      |else
      |  if true
      |    3
      |  else
      |    4
      |  end
      |end
      |
      |""".stripMargin)

    inside(cpg.assignment.l) {
      case assign1 :: assign2 :: assign3 :: assign4 :: Nil =>
        assign1.lineNumber shouldBe Some(4)
        assign1.argument(1).code shouldBe "x"
        assign1.argument(2).code shouldBe "1"
        assign1.argument(2).lineNumber shouldBe Some(4)

        assign2.lineNumber shouldBe Some(5)
        assign2.argument(1).code shouldBe "x"
        assign2.argument(2).code shouldBe "2"
        assign2.argument(2).lineNumber shouldBe Some(6)

        assign3.lineNumber shouldBe Some(10)
        assign3.argument(1).code shouldBe "x"
        assign3.argument(2).code shouldBe "3"
        assign3.argument(2).lineNumber shouldBe Some(10)

        assign4.lineNumber shouldBe Some(11)
        assign4.argument(1).code shouldBe "x"
        assign4.argument(2).code shouldBe "4"
        assign4.argument(2).lineNumber shouldBe Some(12)
      case xs => fail(s"Expected 4 assignments, instead got [${xs.code.mkString(",")}]")
    }

  }

  "nested if-end should have implicit elses" in {
    val cpg = code("""
      |x = if true
      | if true
      |  1
      | end
      |end
      |""".stripMargin)

    val assigns = cpg.assignment.l
    inside(cpg.assignment.l) {
      case assign1 :: assignNil1 :: assignNil2 :: Nil =>
        assign1.argument(1).code shouldBe "x"
        assign1.argument(2).code shouldBe "1"
        assign1.lineNumber shouldBe Some(4)

        assignNil1.argument(1).code shouldBe "x"
        assignNil1.argument(2).code shouldBe "nil"
        assignNil1.lineNumber shouldBe Some(3)

        assignNil2.argument(1).code shouldBe "x"
        assignNil2.argument(2).code shouldBe "nil"
        assignNil2.lineNumber shouldBe Some(2)
      case xs => fail(s"Expected 3 assignments, instead got [${xs.code.mkString(",")}]")
    }
  }

  "in a method body" in {
    val cpg = code("""
        |def f(p)
        |  y = p
        |  y
        |end
        |""".stripMargin)

    inside(cpg.assignment.code("y = p").l) {
      case assign :: Nil =>
        inside(assign.argument.l) {
          case (y: Identifier) :: (p: Identifier) :: Nil =>
            y.name shouldBe "y"
            p.name shouldBe "p"
          case _ => fail(s"Expected two assigment identifiers arguments")
        }
      case _ => fail("Unable to find assignment `y = p`")
    }
  }

}
