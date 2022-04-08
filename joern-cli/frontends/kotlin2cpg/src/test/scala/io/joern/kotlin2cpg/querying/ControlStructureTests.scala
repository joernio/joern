package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, ControlStructure, Identifier, Local}
import io.shiftleft.semanticcpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ControlStructureTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple if-else" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo(x: Int): Int {
        |  if(x > 0) {
        |    return 1
        |  }
        |}
        | """.stripMargin)

    "should contain CODE node for the expression inside the `if`" in {
      val List(c) = cpg.call.code("x > 0").l
      c.code shouldBe "x > 0"
    }
  }

  "CPG for code with `when` statement with assignment in its conditional" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.random.Random
        |
        |fun main() {
        |  when (val result = Random.nextInt(0, 100)) {
        |    in 1..49   -> println("res < 50: `" + result + "`")
        |    in 50..100 -> println("res >= 50: `" + result + "`")
        |  }
        |}
        | """.stripMargin)

    "should contain CONTROL_STRUCTURE node" in {
      cpg.controlStructure.controlStructureType("SWITCH").size shouldBe 1
    }

    "should contain CONTROL_STRUCTURE node with a CALL node to assignment as its condition" in {
      val List(cs) = cpg.controlStructure.controlStructureType("SWITCH").condition.isCall.l
      cs.methodFullName shouldBe Operators.assignment
    }
  }

  "CPG for code with multiple control structures" - {
    lazy val cpg = TestContext.buildCpg("""
        |import kotlin.random.Random
        |
        |class ClassFoo {
        |  fun methodFoo(x: Int, y: Double): Int {
        |    if (x > 1) {
        |      println("> than 1")
        |    } else {
        |      println("<= than 1")
        |    }
        |
        |    when (x) {
        |      1 -> print("x == 1")
        |      2 -> print("x == 2")
        |      else -> { // Note the block
        |          print("x is neither 1 nor 2")
        |      }
        |    }
        |
        |    var z = x
        |    while (z > 0) {
        |      z--
        |    }
        |
        |    do {
        |       val q =  Random.nextInt(0, 100)
        |       print(q)
        |    } while (q < 50)
        |  }
        |}
        |""".stripMargin)

    "should identify `if` block" in {
      cpg.method.name("methodFoo").ifBlock.condition.code.l shouldBe List("x > 1")
    }

    "complex `if` statement contains all required properties" in {
      val List(i) = cpg.controlStructure.where(_.condition.code("x > 1")).l
      i.controlStructureType shouldBe ControlStructureTypes.IF
      i.lineNumber shouldBe Some(5)

      val List(ic) = cpg.controlStructure.where(_.condition.code("x > 1")).condition.l
      ic.code shouldBe "x > 1"

      val List(iwt) = cpg.controlStructure.where(_.condition.code("x > 1")).whenTrue.isExpression.l
      iwt.code shouldBe "println(\"> than 1\")"

      val List(iwf) = cpg.controlStructure.where(_.condition.code("x > 1")).whenFalse.isExpression.l
      iwf.code shouldBe "println(\"<= than 1\")"
    }

    "should identify `when` block" in {
      cpg.method.name("methodFoo").switchBlock.code.l shouldBe List("when(x)")
    }

    "should identify `while` block" in {
      cpg.method.name("methodFoo").whileBlock.code.size shouldBe 1
    }

    "should identify `do` block" in {
      cpg.method.name("methodFoo").doBlock.code.size shouldBe 1
    }
  }

  "CPG for code with simple `for`-statements" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  for (i in 1..8 step 2) print(i)
        |
        |  for (i in 8 downTo 1 step 2) print(i)
        |
        |  for (i in 1 until 10) {
        |    print(i)
        |  }
        |}
        | """.stripMargin)

    "should contain CONTROL_STRUCTURE nodes for the `for` statements with the CODE property set" in {
      cpg.controlStructure.code.dedup.l should not be Seq("")
      cpg.controlStructure.code.dedup.l should not be Seq("<empty>")
    }
  }

  "CPG for code with simple `if`-statement" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |  val aList = listOf("a", "b", "c")
        |  val msg = "b"
        |  if(aList.contains(msg)) {
        |    println("HELLO")
        |  }
        |}
        |""".stripMargin)

    "should contain a CALL node for the condition inside the `if`-statement" in {
      val List(c) = cpg.controlStructure.condition.isCall.l
      c.code shouldBe "aList.contains(msg)"
      c.methodFullName shouldBe "kotlin.collections.List.contains:java.lang.Boolean(java.lang.Object)"
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(5)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.signature shouldBe "java.lang.Boolean(java.lang.Object)"
    }
  }

  "CPG for code with try-catch-finally statement" - {
    lazy val cpg = TestContext.buildCpg("""
      |package mypkg
      |
      |fun main() {
      |   try {
      |      println("INSIDE_TRY")
      |    } catch (e: Exception) {
      |      print("Exception caught.")
      |    } finally {
      |      print("reached `finally`-block.")
      |    }
      |}
      |""".stripMargin)

    "should contain a CONTROL_STRUCTURE node for the try statement with the correct props set" in {
      def matchTryQ = cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY)
      val List(cs)  = matchTryQ.l
      cs.lineNumber shouldBe Some(4)
      cs.columnNumber shouldBe Some(3)

      val List(c1, c2, c3) = matchTryQ.astChildren.l
      c1.order shouldBe 1
      c2.order shouldBe 2
      c3.order shouldBe 3
    }
  }

  "CPG for code with try-catch statement" - {
    lazy val cpg = TestContext.buildCpg("""
      |package mypkg
      |
      |fun main() {
      |   try {
      |     println("INSIDE_TRY")
      |   } catch (e: Exception) {
      |     print("Exception caught.")
      |   }
      |}
      |""".stripMargin)

    "should contain a CONTROL_STRUCTURE node for the try statement with the correct props set" in {
      def matchTryQ = cpg.controlStructure.controlStructureType(ControlStructureTypes.TRY)
      val List(cs)  = matchTryQ.l
      cs.lineNumber shouldBe Some(4)
      cs.columnNumber shouldBe Some(3)

      val List(c1, c2) = matchTryQ.astChildren.l
      c1.order shouldBe 1
      c2.order shouldBe 2
    }
  }

  "CPG for code with code with `for-in` loop" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |    val l = listOf("one", "two", "three")
        |    for (entry in l) {
        |        println(entry)
        |    }
        |//prints:
        |//```
        |//one
        |//two
        |//three
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly lowered representation" in {
      val expectedIteratorLocalName = "iterator_1"
      val List(iteratorLocal)       = cpg.local.nameExact(expectedIteratorLocalName).l
      iteratorLocal.name shouldBe expectedIteratorLocalName
      iteratorLocal.code shouldBe expectedIteratorLocalName
      iteratorLocal.typeFullName shouldBe "" // TODO: maybe try to add a type here if possible to get it

      // TODO: add test for the block in here
      val List(iteratorAssignment: Call) = cpg.call.code("iterator.*itera.*").head.l
      iteratorAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      iteratorAssignment.name shouldBe Operators.assignment
      iteratorAssignment.methodFullName shouldBe Operators.assignment
      iteratorAssignment.code shouldBe expectedIteratorLocalName + " = " + "l.iterator()"
      iteratorAssignment.typeFullName shouldBe ""

      val List(iteratorAssignmentLhs: Identifier, iteratorAssignmentRhs: Call) = iteratorAssignment.argument.l
      iteratorAssignmentLhs.argumentIndex shouldBe 1
      iteratorAssignmentLhs.code shouldBe expectedIteratorLocalName
      iteratorAssignmentLhs.typeFullName shouldBe ""
      iteratorAssignmentRhs.argumentIndex shouldBe 2
      iteratorAssignmentRhs.code shouldBe "l.iterator()"
      iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      iteratorAssignmentRhs.typeFullName shouldBe "java.util.Iterator"
      iteratorAssignmentRhs.methodFullName shouldBe "java.util.List.iterator:java.util.Iterator()"
      iteratorAssignmentRhs.signature shouldBe "java.util.Iterator()"
      iteratorLocal.referencingIdentifiers.id.l.contains(iteratorAssignmentLhs.id) shouldBe true

      val List(iteratorAssignmentRhsArg: Identifier) = iteratorAssignmentRhs.argument.l
      iteratorAssignmentRhsArg.code shouldBe "l"
      iteratorAssignmentRhsArg.name shouldBe "l"
      iteratorAssignmentRhsArg.argumentIndex shouldBe 0
      iteratorAssignmentRhsArg.typeFullName shouldBe "java.util.List"

      val List(controlStructure: ControlStructure) = cpg.controlStructure.head.l
      controlStructure.controlStructureType shouldBe ControlStructureTypes.WHILE
      controlStructure.order shouldBe 3
      controlStructure.condition.size shouldBe 1

      val List(controlStructureFirstChild: Call, controlStructureSecondChild: Block) = controlStructure.astChildren.l
      controlStructureFirstChild.order shouldBe 1
      controlStructureFirstChild.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      controlStructureFirstChild.methodFullName shouldBe "kotlin.collections.Iterator.hasNext:java.lang.Boolean()"
      controlStructureFirstChild.receiver.size shouldBe 1
      controlStructureFirstChild.name shouldBe "hasNext"
      controlStructureFirstChild.signature shouldBe "java.lang.Boolean()"
      controlStructureSecondChild.order shouldBe 2

      val List(loopParameter: Local, getNext: Call, blockInsideBody: Block) = controlStructureSecondChild.astChildren.l
      loopParameter.order shouldBe 1
      loopParameter.code shouldBe "entry"
      getNext.order shouldBe 2
      getNext.methodFullName shouldBe Operators.assignment
      getNext.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      getNext.code shouldBe "entry = iterator_1.next()"

      val List(getNextFirstArg: Identifier, getNextSecondArg: Call) = getNext.argument.l
      getNextFirstArg.order shouldBe 1
      getNextFirstArg.argumentIndex shouldBe 1
      getNextFirstArg.code shouldBe "entry"
      loopParameter.referencingIdentifiers.id.l.contains(getNextFirstArg.id) shouldBe true

      getNextSecondArg.order shouldBe 2
      getNextSecondArg.argumentIndex shouldBe 2
      getNextSecondArg.code shouldBe "iterator_1.next()"
      getNextSecondArg.methodFullName shouldBe "kotlin.collections.Iterator.next:java.lang.Object()"
      getNextSecondArg.name shouldBe "next"
      getNextSecondArg.signature shouldBe "java.lang.Object()"
      getNextSecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      getNextSecondArg.receiver.size shouldBe 1

      val List(getNextSecondArgFirstArg: Identifier) = getNextSecondArg.argument.l
      getNextSecondArgFirstArg.order shouldBe 1
      getNextSecondArgFirstArg.argumentIndex shouldBe 0
      getNextSecondArgFirstArg.code shouldBe "iterator_1"
      getNextSecondArgFirstArg.typeFullName shouldBe ""
      iteratorLocal.referencingIdentifiers.id.l.contains(getNextSecondArgFirstArg.id) shouldBe true

      blockInsideBody.order shouldBe 3 // ????
      val List(statementInsideBody: Call) = blockInsideBody.astChildren.l
      statementInsideBody.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      statementInsideBody.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      statementInsideBody.code shouldBe "println(entry)"
      statementInsideBody.argument.size shouldBe 1

      val List(statementInsideBodyFirstArg: Identifier) = statementInsideBody.argument.l
      statementInsideBodyFirstArg.order shouldBe 1
      statementInsideBodyFirstArg.argumentIndex shouldBe 1
      statementInsideBodyFirstArg.code shouldBe "entry"
      loopParameter.referencingIdentifiers.id.l.contains(statementInsideBodyFirstArg.id) shouldBe true
    }
  }

  // TODO: add a test case also for a `for-in` statement with a destructuring declaration inside the condition
  // TODO: also add test for the loop range, when it is with downTo or whatever
}
