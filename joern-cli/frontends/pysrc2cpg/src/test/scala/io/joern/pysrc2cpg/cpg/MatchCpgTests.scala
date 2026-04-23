package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class MatchCpgTests extends PySrc2CpgFixture() {
  "match statement with guards" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case [a, b] if 3 == 4:
        |      print(1)
        |    case _ if 5 == 6:
        |      print(2)
        |    case _:
        |      print(3)
        |""".stripMargin)

    "have correct AST" in {
      val matchStmt = cpg.controlStructure.head
      val condition = matchStmt.astChildren.order(1).head
      condition.label shouldBe NodeTypes.IDENTIFIER
      condition.code shouldBe "command"

      val matchBodyBlock = matchStmt.astChildren.order(2).head
      matchBodyBlock.label shouldBe NodeTypes.BLOCK

      matchBodyBlock.astChildren.l match {
        case List(
              jumpTargetFirstCase,
              blockFirstCase,
              jumpTargetSecondCase,
              blockSecondCase,
              jumpTargetThirdCase,
              blockThirdCase
            ) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b] if 3 == 4"

          blockFirstCase.label shouldBe NodeTypes.BLOCK

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "case _ if 5 == 6"

          blockSecondCase.label shouldBe NodeTypes.BLOCK

          jumpTargetThirdCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetThirdCase.code shouldBe "default"

          blockThirdCase.label shouldBe NodeTypes.BLOCK
          blockThirdCase.code shouldBe "print(3)"
        case other => fail(s"Expected 6 AST children, but got ${other.size}: $other")
      }
    }
  }

  "match statement" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case [a, b]:
        |      print(1)
        |    case _:
        |      print(2)
        |""".stripMargin)

    "have correct AST" in {
      val matchStmt = cpg.controlStructure.head
      val condition = matchStmt.astChildren.order(1).head
      condition.label shouldBe NodeTypes.IDENTIFIER
      condition.code shouldBe "command"

      val matchBodyBlock = matchStmt.astChildren.order(2).head
      matchBodyBlock.label shouldBe NodeTypes.BLOCK

      matchBodyBlock.astChildren.l match {
        case List(jumpTargetFirstCase, blockFirstCase, jumpTargetSecondCase, blockSecondCase) =>
          jumpTargetFirstCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetFirstCase.code shouldBe "case [a, b]"

          blockFirstCase.label shouldBe NodeTypes.BLOCK

          jumpTargetSecondCase.label shouldBe NodeTypes.JUMP_TARGET
          jumpTargetSecondCase.code shouldBe "default"

          blockSecondCase.label shouldBe NodeTypes.BLOCK
          blockSecondCase.code shouldBe "print(2)"
        case other => fail(s"Expected 4 AST children, but got ${other.size}: $other")
      }
    }

    "have correct CFG" in {
      // The condition identifier should have CFG edges to the jump targets
      val conditionId = cpg.identifier.nameExact("command").lineNumber(3).head
      conditionId.cfgOut.l match {
        case List(jumpTarget1, jumpTarget2) =>
          Set(jumpTarget1.code, jumpTarget2.code) shouldBe Set("case [a, b]", "default")
        case other => fail(s"Expected 2 CFG successors, but got ${other.size}: $other")
      }
    }
  }

  "match statement with sequence pattern bindings" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case [a, b]:
        |      print(a, b)
        |""".stripMargin)

    "create assignment nodes for pattern variables" in {
      val matchStmt = cpg.controlStructure.head
      val caseBlock = matchStmt.astChildren.order(2).head.astChildren.l(1)
      caseBlock.label shouldBe NodeTypes.BLOCK

      // Pattern assignments should be in the case block
      val assignments = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.matches("a = command\\[0\\]|b = command\\[1\\]"))
      assignments.size shouldBe 2
    }

    "use index access for sequence element extraction" in {
      val indexAccesses = cpg.call.methodFullName(Operators.indexAccess).l
        .filter(_.code.matches("command\\[0\\]|command\\[1\\]"))
      indexAccesses.size shouldBe 2
    }
  }

  "match statement with named binding (catch-all)" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case x:
        |      print(x)
        |""".stripMargin)

    "create assignment for catch-all binding" in {
      val assignments = cpg.call.methodFullName(Operators.assignment).codeExact("x = command").l
      assignments.size shouldBe 1
    }
  }

  "match statement with wildcard" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case _:
        |      print("default")
        |""".stripMargin)

    "not create pattern assignments for wildcard" in {
      // The only assignment-like thing should be the method body, not pattern bindings
      val patternAssignments = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.contains("command"))
      patternAssignments.size shouldBe 0
    }
  }

  "match statement with literal pattern" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case 42:
        |      print("found")
        |""".stripMargin)

    "not create pattern assignments for literal" in {
      val patternAssignments = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.contains("command"))
      patternAssignments.size shouldBe 0
    }
  }

  "match statement with complex subject" should {
    val cpg = code("""
        |def someFunc():
        |  match get_data():
        |    case [a, b]:
        |      print(a, b)
        |""".stripMargin)

    "create temp variable for complex subject" in {
      // Complex expression subjects get a temp variable
      val tmpAssignments = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.matches("tmp\\d+ = get_data\\(\\)"))
      tmpAssignments.size shouldBe 1
    }

    "create pattern assignments referencing temp" in {
      val assignments = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.matches("[ab] = tmp\\d+\\[\\d+\\]"))
      assignments.size shouldBe 2
    }
  }

  "match statement with alias pattern" should {
    val cpg = code("""
        |def someFunc(command):
        |  match command:
        |    case [a, b] as whole:
        |      print(whole)
        |""".stripMargin)

    "create assignments for both inner bindings and alias" in {
      val aAssign = cpg.call.methodFullName(Operators.assignment).l
        .filter(_.code.matches("a = command\\[0\\]"))
      aAssign.size shouldBe 1

      val wholeAssign = cpg.call.methodFullName(Operators.assignment).codeExact("whole = command").l
      wholeAssign.size shouldBe 1
    }
  }
}
