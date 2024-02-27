package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier}
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends RubyCode2CpgFixture {

  "`while-end` statement is represented by a `WHILE` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |x = 1
        |while x > 0 do
        | x = x - 1
        |end
        |""".stripMargin)

    val List(whileNode)  = cpg.whileBlock.l
    val List(whileCond)  = whileNode.condition.isCall.l
    val List(assignment) = whileNode.whenTrue.assignment.l

    whileCond.methodFullName shouldBe Operators.greaterThan
    whileCond.code shouldBe "x > 0"
    whileCond.lineNumber shouldBe Some(3)

    assignment.code shouldBe "x = x - 1"
    assignment.lineNumber shouldBe Some(4)
  }

  "`until-end` statement is represented by a negated `WHILE` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |x = 1
        |until x <= 0 do
        | x = x - 1
        |end
        |""".stripMargin)

    val List(untilNode)    = cpg.whileBlock.l
    val List(untilNegCond) = untilNode.condition.isCall.l
    val List(assignment)   = untilNode.whenTrue.assignment.l

    untilNegCond.methodFullName shouldBe Operators.logicalNot
    untilNegCond.code shouldBe "x <= 0"
    untilNegCond.lineNumber shouldBe Some(3)

    val List(untilOriginalCond: Call) = untilNegCond.astChildren.l: @unchecked
    untilOriginalCond.methodFullName shouldBe Operators.lessEqualsThan
    untilOriginalCond.code shouldBe "x <= 0"

    assignment.code shouldBe "x = x - 1"
    assignment.lineNumber shouldBe Some(4)
  }

  "`if-end` statement is represented by an `IF` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |if __LINE__ > 1 then
        | "> 1"
        |end
        |""".stripMargin)

    val List(ifNode) = cpg.ifBlock.l
    val List(ifCond) = ifNode.condition.isCall.l
    val List(str)    = ifNode.whenTrue.isBlock.astChildren.isLiteral.l

    ifCond.code shouldBe "__LINE__ > 1"
    ifCond.methodFullName shouldBe Operators.greaterThan
    ifCond.lineNumber shouldBe Some(2)

    str.code shouldBe """"> 1""""
    str.lineNumber shouldBe Some(3)
  }

  "`if-else-end` statement is represented by `IF`-`ELSE` CONTROL_STRUCTURE nodes" in {
    val cpg = code("""
        |if __LINE__ > 1 then
        | "> 1"
        |else
        | "<= 1"
        |end
        |""".stripMargin)

    val List(ifNode)  = cpg.ifBlock.l
    val List(ifCond)  = ifNode.condition.isCall.l
    val List(thenStr) = ifNode.whenTrue.isBlock.astChildren.isLiteral.l
    val List(elseStr) = ifNode.whenFalse.isBlock.astChildren.isLiteral.l

    ifCond.code shouldBe "__LINE__ > 1"
    ifCond.methodFullName shouldBe Operators.greaterThan
    ifCond.lineNumber shouldBe Some(2)

    thenStr.code shouldBe """"> 1""""
    thenStr.lineNumber shouldBe Some(3)

    elseStr.code shouldBe """"<= 1""""
    elseStr.lineNumber shouldBe Some(5)
  }

  "`if-elsif-end` statement is represented by `IF`-`ELSE`-`IF` CONTROL_STRUCTURE nodes" in {
    val cpg = code("""
        |if __LINE__ == 0 then
        | '= 0'
        |elsif __LINE__ > 0 then
        | '> 0'
        |end
        |""".stripMargin)

    val List(ifNode)  = cpg.ifBlock.where(_.lineNumber(2)).l
    val List(ifCond)  = ifNode.condition.isCall.l
    val List(thenStr) = ifNode.whenTrue.isBlock.astChildren.isLiteral.l

    ifCond.code shouldBe "__LINE__ == 0"
    ifCond.methodFullName shouldBe Operators.equals
    ifCond.lineNumber shouldBe Some(2)

    thenStr.code shouldBe "'= 0'"
    thenStr.lineNumber shouldBe Some(3)

    val List(elsIfNode) = ifNode.whenFalse.isBlock.astChildren.isControlStructure.l
    val List(elsIfCond) = elsIfNode.condition.isCall.l
    val List(elsIfStr)  = elsIfNode.whenTrue.isBlock.astChildren.isLiteral.l

    elsIfCond.code shouldBe "__LINE__ > 0"
    elsIfCond.methodFullName shouldBe Operators.greaterThan
    elsIfCond.lineNumber shouldBe Some(4)

    elsIfStr.code shouldBe "'> 0'"
    elsIfStr.lineNumber shouldBe Some(5)
  }

  "`unless-end` statement is represented by a negated `IF` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |unless __LINE__ == 0 then
        |  x = '!= 0'
        |end
        |""".stripMargin)

    val List(unlessNode)    = cpg.ifBlock.l
    val List(unlessNegCond) = unlessNode.condition.isCall.l
    val List(assignment)    = unlessNode.whenTrue.assignment.l

    unlessNode.whenFalse.isEmpty shouldBe true

    unlessNegCond.methodFullName shouldBe Operators.logicalNot
    unlessNegCond.code shouldBe "__LINE__ == 0"
    unlessNegCond.lineNumber shouldBe Some(2)

    val List(unlessOriginalCond) = unlessNegCond.argument.isCall.l
    unlessOriginalCond.methodFullName shouldBe Operators.equals
    unlessOriginalCond.code shouldBe "__LINE__ == 0"

    assignment.code shouldBe "x = '!= 0'"
    assignment.lineNumber shouldBe Some(3)
  }

  "`unless-else-end` statement is represented by a negated `IF` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |unless __LINE__ == 0 then
        | x = '!= 0'
        |else
        | x = '= 0'
        |end
        |""".stripMargin)

    val List(unlessNode)     = cpg.ifBlock.l
    val List(unlessNegCond)  = unlessNode.condition.isCall.l
    val List(thenAssignment) = unlessNode.whenTrue.assignment.l
    val List(elseAssignment) = unlessNode.whenFalse.assignment.l

    unlessNegCond.methodFullName shouldBe Operators.logicalNot
    unlessNegCond.code shouldBe "__LINE__ == 0"
    unlessNegCond.lineNumber shouldBe Some(2)

    val List(unlessOriginalCond) = unlessNegCond.argument.isCall.l
    unlessOriginalCond.methodFullName shouldBe Operators.equals
    unlessOriginalCond.code shouldBe "__LINE__ == 0"

    thenAssignment.code shouldBe "x = '!= 0'"
    thenAssignment.lineNumber shouldBe Some(3)

    elseAssignment.code shouldBe "x = '= 0'"
    elseAssignment.lineNumber shouldBe Some(5)
  }

  "`... unless ...` statement is represented by a negated `IF` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |42 unless false
        |""".stripMargin)

    val List(unlessNode)    = cpg.ifBlock.l
    val List(unlessNegCond) = unlessNode.condition.isCall.l
    val List(thenLiteral)   = unlessNode.whenTrue.isBlock.astChildren.isLiteral.l

    unlessNegCond.methodFullName shouldBe Operators.logicalNot
    unlessNegCond.code shouldBe "false"
    unlessNegCond.lineNumber shouldBe Some(2)

    val List(unlessOriginalCond) = unlessNegCond.argument.isLiteral.l
    unlessOriginalCond.code shouldBe "false"
    unlessOriginalCond.lineNumber shouldBe Some(2)

    thenLiteral.code shouldBe "42"
    thenLiteral.lineNumber shouldBe Some(2)
  }

  "`unless` binds tighter than `=`" in {
    val cpg = code("""
        |x = 1 unless false
        |""".stripMargin)

    val List(unlessNode)    = cpg.ifBlock.l
    val List(unlessNegCond) = unlessNode.condition.isCall.l
    val List(assignment)    = unlessNode.whenTrue.assignment.l

    unlessNode.whenFalse.isEmpty shouldBe true

    unlessNegCond.methodFullName shouldBe Operators.logicalNot
    unlessNegCond.code shouldBe "false"

    assignment.code shouldBe "x = 1"
    assignment.lineNumber shouldBe Some(2)
  }

  "`... if ...` statement is represented by an `IF` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |"> 1" if __LINE__ > 1
        |""".stripMargin)

    val List(ifNode) = cpg.ifBlock.l
    val List(ifCond) = ifNode.condition.isCall.l
    val List(str)    = ifNode.whenTrue.isBlock.astChildren.isLiteral.l

    ifCond.code shouldBe "__LINE__ > 1"
    ifCond.methodFullName shouldBe Operators.greaterThan
    ifCond.lineNumber shouldBe Some(2)

    str.code shouldBe """"> 1""""
    str.lineNumber shouldBe Some(2)
  }

  "`... while ...` statement is represented by a `WHILE` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |puts 'hi' while (true)
        |""".stripMargin)

    val List(whileNode) = cpg.whileBlock.l
    val List(whileCond) = whileNode.condition.isLiteral.l
    val List(putsHi)    = whileNode.whenTrue.isCall.l

    whileCond.code shouldBe "true"
    whileCond.lineNumber shouldBe Some(2)

    putsHi.methodFullName shouldBe "__builtin:puts"
    putsHi.code shouldBe "puts 'hi'"
    putsHi.lineNumber shouldBe Some(2)
  }

  "`begin ... rescue ... end is represented by a `TRY` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |def test1
        |  begin 
        |    puts
        |    1 
        |  rescue E1 => e
        |    puts
        |    2 
        |  rescue E2
        |    puts
        |    3
        |  rescue
        |    puts
        |    4
        |  else
        |    puts
        |    5
        |  ensure
        |    puts
        |    6
        |  end
        |end
        |""".stripMargin)

    val List(rescueNode) = cpg.method("test1").tryBlock.l
    rescueNode.controlStructureType shouldBe ControlStructureTypes.TRY
    val List(body, rescueBody1, rescueBody2, rescueBody3, elseBody, ensureBody) = rescueNode.astChildren.l
    body.ast.isLiteral.code.l shouldBe List("1")
    body.order shouldBe 1

    rescueBody1.ast.isLiteral.code.l shouldBe List("2")
    rescueBody1.order shouldBe 2

    rescueBody2.ast.isLiteral.code.l shouldBe List("3")
    rescueBody2.order shouldBe 2

    rescueBody3.ast.isLiteral.code.l shouldBe List("4")
    rescueBody3.order shouldBe 2

    elseBody.ast.isLiteral.code.l shouldBe List("5")
    elseBody.order shouldBe 2

    ensureBody.ast.isLiteral.code.l shouldBe List("6")
    ensureBody.order shouldBe 3

  }

  "`begin ... ensure ... end is represented by a `TRY` CONTROL_STRUCTURE node" in {
    val cpg = code("""
        |def test2
        |  begin
        |   1   
        |  ensure
        |   2
        |  end
        |end
        |""".stripMargin)
    val List(rescueNode) = cpg.method("test2").tryBlock.l
    rescueNode.controlStructureType shouldBe ControlStructureTypes.TRY
    val List(body, defaultElseBody, ensureBody) = rescueNode.astChildren.l

    body.ast.isLiteral.code.l shouldBe List("1")
    body.order shouldBe 1

    defaultElseBody.ast.isLiteral.code.l shouldBe List("nil")
    ensureBody.order shouldBe 3

    ensureBody.ast.isLiteral.code.l shouldBe List("2")
    ensureBody.order shouldBe 3
  }

  "`for .. in` control structure" should {
    val cpg = code("""
        |def foo1
        | x = [1, 2, 3]
        | for i in x do
        |   puts x - i
        | end
        |end
        |
        |def foo2
        | x = 3
        | for i in 1..x do
        |   puts x + i
        | end
        |end
        |""".stripMargin)

    "create a FOR control structure node with body with an array iterable" in {
      inside(cpg.method("foo1").controlStructure.l) {
        case forEachNode :: Nil =>
          forEachNode.controlStructureType shouldBe ControlStructureTypes.FOR

          inside(forEachNode.astChildren.l) {
            case (iteratorNode: Identifier) :: (iterableNode: Identifier) :: (doBody: Block) :: Nil =>
              iteratorNode.code shouldBe "i"
              iterableNode.code shouldBe "x"
              // We use .ast as there will be an implicit return node here
              doBody.ast.isCall.code.headOption shouldBe Option("puts x - i")
            case _ => fail("No node for iterable found in `for-in` statement")
          }

          inside(forEachNode.astChildren.isBlock.l) {
            case blockNode :: Nil =>
              val List(puts) = blockNode.ast.isCall.nameExact("puts").l
              puts.parentBlock.head shouldBe blockNode
            case _ => fail("Correct blockNode as child not found for `for-in` statement")
          }

        case _ => fail("No control structure node found for `for-in`.")
      }
    }

    "create a FOR control structure node with body with a 'range' iterable" in {
      inside(cpg.method("foo2").controlStructure.l) {
        case forEachNode :: Nil =>
          forEachNode.controlStructureType shouldBe ControlStructureTypes.FOR

          inside(forEachNode.astChildren.l) {
            case (iteratorNode: Identifier) :: (iterableNode: Call) :: (doBody: Block) :: Nil =>
              iteratorNode.code shouldBe "i"
              iterableNode.code shouldBe "1..x"
              iterableNode.name shouldBe Operators.range
              // We use .ast as there will be an implicit return node here
              doBody.ast.isCall.code.headOption shouldBe Option("puts x + i")
            case _ => fail("Invalid `for-in` children nodes")
          }

        case _ => fail("No control structure node found for `for-in`.")
      }
    }
  }
}
