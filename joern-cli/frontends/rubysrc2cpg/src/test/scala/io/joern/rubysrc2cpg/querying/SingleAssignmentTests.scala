package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines as RubyDefines
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
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

  "`||=` is represented by a lowered if call to .nil?" in {
    val cpg = code("""
        |def foo(x)
        |  x ||= false
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("!x")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "x = false"
            val List(lhs, rhs) = assignmentCall.argument.l
            lhs.code shouldBe "x"
            rhs.code shouldBe "false"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one control structure, got ${xs.code.mkString(",")}")
    }
  }

  "`&&=` is represented by lowered if call to .nil?" in {
    val cpg = code("""
        |def foo(x)
        |  x &&= true
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("x")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "x = true"
            val List(lhs, rhs) = assignmentCall.argument.l
            lhs.code shouldBe "x"
            rhs.code shouldBe "true"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one control structure, got ${xs.code.mkString(",")}")
    }
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

        assign2.lineNumber shouldBe Some(6)
        assign2.argument(1).code shouldBe "x"
        assign2.argument(2).code shouldBe "2"
        assign2.argument(2).lineNumber shouldBe Some(6)

        assign3.lineNumber shouldBe Some(10)
        assign3.argument(1).code shouldBe "x"
        assign3.argument(2).code shouldBe "3"
        assign3.argument(2).lineNumber shouldBe Some(10)

        assign4.lineNumber shouldBe Some(12)
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

  "Bracket Assignments" in {
    val cpg = code("""
                     | def get_pto_schedule
                     |    begin
                     |       schedules = current_user.paid_time_off.schedule
                     |       jfs = []
                     |       schedules.each do |s|
                     |          hash = Hash.new
                     |          hash[:id] = s[:id]
                     |          hash[:title] = s[:event_name]
                     |          hash[:start] = s[:date_begin]
                     |          hash[:end] = s[:date_end]
                     |          jfs << hash
                     |       end
                     |    rescue
                     |    end
                     |    respond_to do |format|
                     |       format.json { render json: jfs.to_json }
                     |    end
                     |  end
                     |""".stripMargin)

    inside(cpg.method.isLambda.l) {
      case scheduleLambda :: _ :: _ :: Nil =>
        inside(scheduleLambda.call.name(Operators.assignment).l) {
          case _ :: id :: title :: start :: end :: _ :: Nil =>
            id.code shouldBe "hash[:id] = s[:id]"

            inside(id.argument.l) {
              case (lhs: Call) :: (rhs: Call) :: Nil =>
                lhs.methodFullName shouldBe Operators.indexAccess
                lhs.code shouldBe "hash[:id]"

                rhs.methodFullName shouldBe Operators.indexAccess
                rhs.code shouldBe "s[:id]"

                inside(lhs.argument.l) {
                  case base :: (index: Literal) :: Nil =>
                    index.typeFullName shouldBe RubyDefines.prefixAsCoreType(RubyDefines.Symbol)
                  case xs => fail(s"Expected base and index, got [${xs.code.mkString(",")}]")
                }

                inside(rhs.argument.l) {
                  case base :: (index: Literal) :: Nil =>
                    index.typeFullName shouldBe RubyDefines.prefixAsCoreType(RubyDefines.Symbol)
                  case xs => fail(s"Expected base and index, got [${xs.code.mkString(",")}]")
                }

              case xs => fail(s"Expected lhs and rhs, got ${xs.code.mkString(";")}]")
            }
          case xs => fail(s"Expected six assignments, got [${xs.code.mkString(";")}]")
        }
      case xs => fail(s"Expected three lambdas, got ${xs.size} lambdas instead")
    }
  }

  "Bracketed ||= is represented by a lowered if call to .nil?" in {
    val cpg = code("""
        |def foo
        |  hash[:id] ||= s[:id]
        |end
        |""".stripMargin)
    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("!hash[:id]")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "hash[:id] = s[:id]"
            val List(lhs, rhs) = assignmentCall.argument.l
            lhs.code shouldBe "hash[:id]"
            rhs.code shouldBe "s[:id]"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one control structure, got ${xs.code.mkString(",")}")
    }
  }

  "Bracketed +=" in {
    val cpg = code("""
        |hash[:id] += s[:id]
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignmentPlus).l) {
      case assignmentCall :: Nil =>
        assignmentCall.code shouldBe "hash[:id] += s[:id]"
        assignmentCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        inside(assignmentCall.argument.l) {
          case lhs :: rhs :: Nil =>
            lhs.code shouldBe "hash[:id]"
            rhs.code shouldBe "s[:id]"
          case xs => fail(s"Expected lhs and rhs arguments, got ${xs.code.mkString(",")}")
        }
      case xs => fail(s"Expected on assignmentOr call, got ${xs.code.mkString(",")}")
    }
  }

  "Bracketed &&= is represented by a lowere if call to .nil?" in {
    val cpg = code("""
                     |def foo
                     |  hash[:id] &&= s[:id]
                     |end
                     |""".stripMargin)
    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("hash[:id]")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "hash[:id] = s[:id]"
            val List(lhs, rhs) = assignmentCall.argument.l
            lhs.code shouldBe "hash[:id]"
            rhs.code shouldBe "s[:id]"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one control structure, got ${xs.code.mkString(",")}")
    }
  }

  "Bracketed /=" in {
    val cpg = code("""
        |hash[:id] /= s[:id]
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignmentDivision).l) {
      case assignmentCall :: Nil =>
        assignmentCall.code shouldBe "hash[:id] /= s[:id]"
        assignmentCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

        inside(assignmentCall.argument.l) {
          case lhs :: rhs :: Nil =>
            lhs.code shouldBe "hash[:id]"
            rhs.code shouldBe "s[:id]"
          case xs => fail(s"Expected lhs and rhs arguments, got ${xs.code.mkString(",")}")
        }
      case xs => fail(s"Expected on assignmentOr call, got ${xs.code.mkString(",")}")
    }
  }

  "Single ||= Assignment" in {
    val cpg = code("""
        |def foo
        |  A.B ||= c 1
        |end
        |""".stripMargin)

    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("!A.B")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "A.B = c 1"
            val List(lhs, rhs: Call) = assignmentCall.argument.l: @unchecked
            lhs.code shouldBe "A.B"

            rhs.code shouldBe "c 1"
            val List(_, litArg) = rhs.argument.l
            litArg.code shouldBe "1"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one if statement, got ${xs.code.mkString(",")}")
    }
  }

  "Single &&= Assignment" in {
    val cpg = code("""
                     |def foo
                     |  A.B &&= c 1
                     |end
                     |""".stripMargin)

    inside(cpg.method.name("foo").controlStructure.l) {
      case ifStruct :: Nil =>
        ifStruct.controlStructureType shouldBe ControlStructureTypes.IF
        ifStruct.condition.code.l shouldBe List("A.B")

        inside(ifStruct.whenTrue.ast.isCall.name(Operators.assignment).l) {
          case assignmentCall :: Nil =>
            assignmentCall.code shouldBe "A.B = c 1"
            val List(lhs: Call, rhs: Call) = assignmentCall.argument.l: @unchecked
            lhs.code shouldBe "A.B"
            lhs.methodFullName shouldBe Operators.fieldAccess

            rhs.code shouldBe "c 1"
            val List(_, litArg) = rhs.argument.l
            litArg.code shouldBe "1"
          case xs => fail(s"Expected assignment call in true branch, got ${xs.code.mkString}")
        }

      case xs => fail(s"Expected one if statement, got ${xs.code.mkString(",")}")
    }
  }

  "+= assignment operator" in {
    val cpg = code("""
        |A::b += 1
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignmentPlus).l) {
      case assignmentCall :: Nil =>
        val List(lhs: Call, rhs) = assignmentCall.argument.l: @unchecked

        lhs.code shouldBe "A::b"
        lhs.methodFullName shouldBe Operators.fieldAccess

        rhs.code shouldBe "1"
      case xs => fail(s"Expected one call for assignment, got ${xs.code.mkString(",")}")
    }
  }

  "*= assignment operator" in {
    val cpg = code("""
        |A::b *= 1
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignmentMultiplication).l) {
      case assignmentCall :: Nil =>
        assignmentCall.code shouldBe "A::b *= 1"
        val List(lhs: Call, rhs) = assignmentCall.argument.l: @unchecked

        lhs.code shouldBe "A::b"
        lhs.methodFullName shouldBe Operators.fieldAccess

        rhs.code shouldBe "1"
      case xs => fail(s"Expected one call for assignment, got ${xs.code.mkString(",")}")
    }
  }

  "MethodInvocationWithoutParentheses multiple call args" in {
    val cpg = code("""
        |def gl_badge_tag(*args, &block)
        |   render :some_symbol, &block
        |end
        |""".stripMargin)

    inside(cpg.call.name("render").argument.l) {
      case _ :: (symbolArg: Literal) :: (blockArg: Identifier) :: Nil =>
        symbolArg.code shouldBe ":some_symbol"
        blockArg.code shouldBe "block"

      case xs => fail(s"Expected two args, found [${xs.code.mkString(",")}]")
    }
  }

  "bitwise AND/OR assignments should parse correctly" in {
    val cpg = code("""
        |x = 1
        |x &= 0
        |x |= 1
        |""".stripMargin)

    inside(cpg.assignment.l) { case _ :: and :: or :: Nil =>
      and.name shouldBe Operators.assignmentAnd
      and.code shouldBe "x &= 0"

      or.name shouldBe Operators.assignmentOr
      or.code shouldBe "x |= 1"
    }
  }

  "shift left/right assignments should parse correctly" in {
    val cpg = code("""
        |x = 1
        |x >>= 1
        |x <<= 2
        |""".stripMargin)

    inside(cpg.assignment.l) { case _ :: sr :: sl :: Nil =>
      sr.name shouldBe Operators.assignmentArithmeticShiftRight
      sr.code shouldBe "x >>= 1"

      sl.name shouldBe Operators.assignmentShiftLeft
      sl.code shouldBe "x <<= 2"
    }
  }

  "global variable assignment" in {
    val cpg = code("""
        |$alfred = "123"
        |""".stripMargin)

    inside(cpg.call.name(Operators.assignment).l) {
      case alfredAssign :: Nil =>
        alfredAssign.code shouldBe "$alfred = \"123\""

        val List(lhs: Call, rhs: Literal) = alfredAssign.argument.l: @unchecked

        lhs.methodFullName shouldBe Operators.fieldAccess
        lhs.code shouldBe "self.$alfred"

        rhs.code shouldBe "\"123\""
      case xs => fail(s"Expected one assignment call, got [${xs.code.mkString(",")}]")
    }
  }
}
