package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, ControlStructure, Identifier, Local}
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ControlStructureTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  "CPG for code with simple if-else" should {
    val cpg = code("""
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

  "CPG for code with `when` statement with assignment in its conditional" should {
    val cpg = code("""
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

    "should contain CONTROL_STRUCTURE node with a BLOCK as its condition" in {
      val List(_: Block) = cpg.controlStructure.controlStructureType("SWITCH").condition.l: @unchecked
    }
  }

  "CPG for code with multiple control structures" should {
    val cpg = code("""
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
      i.lineNumber shouldBe Some(6)

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

  "CPG for code with simple `for`-statements" should {
    val cpg = code("""
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

  "CPG for code with simple `if`-statement" should {
    val cpg = code("""
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
      c.methodFullName shouldBe "kotlin.collections.List.contains:boolean(java.lang.Object)"
      c.lineNumber shouldBe Some(7)
      c.columnNumber shouldBe Some(5)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.signature shouldBe "boolean(java.lang.Object)"
    }
  }

  "CPG for code with try-catch-finally statement" should {
    val cpg = code("""
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
      val List(tryNode) = cpg.controlStructure.isTry.l
      tryNode.lineNumber shouldBe Some(5)
      tryNode.columnNumber shouldBe Some(3)

      val List(tryBlock) = tryNode.astChildren.order(1).l
      tryBlock.astChildren.isCall.code.l shouldBe List("""println("INSIDE_TRY")""")

      val List(catchBlock) = tryNode.astChildren.isControlStructure.isCatch.astChildren.l
      catchBlock.astChildren.isCall.code.l shouldBe List("""print("Exception caught.")""")

      val List(finallyBlock) = tryNode.astChildren.isControlStructure.isFinally.astChildren.l
      finallyBlock.astChildren.isCall.code.l shouldBe List("""print("reached `finally`-block.")""")
    }
  }

  "CPG for code with try-catch statement" should {
    val cpg = code("""
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
      cs.lineNumber shouldBe Some(5)
      cs.columnNumber shouldBe Some(3)

      val List(c1, c2) = matchTryQ.astChildren.l
      c1.order shouldBe 1
      c2.order shouldBe 2
    }
  }

  "CPG for code with `for-in` loop which has a simple variable loop parameter" should {
    val cpg = code("""
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
      iteratorLocal.typeFullName shouldBe "ANY"

      // TODO: add test for the block in here
      val iteratorAssignment = cpg.call.code("iterator.*itera.*").head
      iteratorAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      iteratorAssignment.name shouldBe Operators.assignment
      iteratorAssignment.methodFullName shouldBe Operators.assignment
      iteratorAssignment.code shouldBe expectedIteratorLocalName + " = " + "l.iterator()"

      val List(iteratorAssignmentLhs: Identifier, iteratorAssignmentRhs: Call) =
        iteratorAssignment.argument.l: @unchecked
      iteratorAssignmentLhs.argumentIndex shouldBe 1
      iteratorAssignmentLhs.code shouldBe expectedIteratorLocalName
      iteratorAssignmentLhs.typeFullName shouldBe "ANY"
      iteratorAssignmentRhs.argumentIndex shouldBe 2
      iteratorAssignmentRhs.code shouldBe "l.iterator()"
      iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      iteratorAssignmentRhs.typeFullName shouldBe "java.util.Iterator"
      iteratorAssignmentRhs.methodFullName shouldBe "java.util.List.iterator:java.util.Iterator()"
      iteratorAssignmentRhs.signature shouldBe "java.util.Iterator()"
      iteratorLocal.referencingIdentifiers.id.l.contains(iteratorAssignmentLhs.id) shouldBe true

      val List(iteratorAssignmentRhsArg: Identifier) = iteratorAssignmentRhs.argument.l: @unchecked
      iteratorAssignmentRhsArg.code shouldBe "l"
      iteratorAssignmentRhsArg.name shouldBe "l"
      iteratorAssignmentRhsArg.argumentIndex shouldBe 0
      iteratorAssignmentRhsArg.typeFullName shouldBe "java.util.List"

      val controlStructure = cpg.controlStructure.head
      controlStructure.controlStructureType shouldBe ControlStructureTypes.WHILE
      controlStructure.order shouldBe 3
      controlStructure.condition.size shouldBe 1

      val List(controlStructureFirstChild: Call, controlStructureSecondChild: Block) =
        controlStructure.astChildren.l: @unchecked
      controlStructureFirstChild.order shouldBe 1
      controlStructureFirstChild.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      controlStructureFirstChild.methodFullName shouldBe "kotlin.collections.Iterator.hasNext:boolean()"
      controlStructureFirstChild.name shouldBe "hasNext"
      controlStructureFirstChild.signature shouldBe "boolean()"
      controlStructureSecondChild.order shouldBe 2

      val List(loopParameter: Local, getNext: Call, blockInsideBody: Block) =
        controlStructureSecondChild.astChildren.l: @unchecked
      loopParameter.order shouldBe 1
      loopParameter.code shouldBe "entry"
      getNext.order shouldBe 2
      getNext.methodFullName shouldBe Operators.assignment
      getNext.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      getNext.code shouldBe "entry = iterator_1.next()"

      val List(getNextFirstArg: Identifier, getNextSecondArg: Call) = getNext.argument.l: @unchecked
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

      val List(getNextSecondArgFirstArg: Identifier) = getNextSecondArg.argument.l: @unchecked
      getNextSecondArgFirstArg.order shouldBe 1
      getNextSecondArgFirstArg.argumentIndex shouldBe 0
      getNextSecondArgFirstArg.code shouldBe "iterator_1"
      getNextSecondArgFirstArg.typeFullName shouldBe "ANY"
      iteratorLocal.referencingIdentifiers.id.l.contains(getNextSecondArgFirstArg.id) shouldBe true

      blockInsideBody.order shouldBe 3 // ????
      val List(statementInsideBody: Call) = blockInsideBody.astChildren.l: @unchecked
      statementInsideBody.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      statementInsideBody.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      statementInsideBody.code shouldBe "println(entry)"
      statementInsideBody.argument.size shouldBe 1

      val List(statementInsideBodyFirstArg: Identifier) = statementInsideBody.argument.l: @unchecked
      statementInsideBodyFirstArg.order shouldBe 1
      statementInsideBodyFirstArg.argumentIndex shouldBe 1
      statementInsideBodyFirstArg.code shouldBe "entry"
      loopParameter.referencingIdentifiers.id.l.contains(statementInsideBodyFirstArg.id) shouldBe true
    }
  }

  "CPG for code with `for-in` loop which has a destructuring declaration loop parameter" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val x: String, val y: Int)
        |
        |fun main() {
        |    val a1 = AClass("1x", 1)
        |    val a2 = AClass("2x", 2)
        |    val aList = listOf(a1, a2)
        |    for ((dVal1, dVal2) in aList) {
        |        println(dVal1)
        |        println(dVal2)
        |    }
        |//prints:
        |//```
        |//1x
        |//1
        |//2x
        |//2
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly lowered representation" in {
      val expectedIteratorLocalName = "iterator_1"
      val List(iteratorLocal)       = cpg.local.nameExact(expectedIteratorLocalName).l
      iteratorLocal.name shouldBe expectedIteratorLocalName
      iteratorLocal.code shouldBe expectedIteratorLocalName
      iteratorLocal.typeFullName shouldBe "ANY"

      // TODO: add test for the block in here
      val iteratorAssignment = cpg.call.code("iterator.*itera.*").head
      iteratorAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      iteratorAssignment.name shouldBe Operators.assignment
      iteratorAssignment.methodFullName shouldBe Operators.assignment
      iteratorAssignment.code shouldBe expectedIteratorLocalName + " = " + "aList.iterator()"

      val List(iteratorAssignmentLhs: Identifier, iteratorAssignmentRhs: Call) =
        iteratorAssignment.argument.l: @unchecked
      iteratorAssignmentLhs.argumentIndex shouldBe 1
      iteratorAssignmentLhs.code shouldBe expectedIteratorLocalName
      iteratorAssignmentLhs.typeFullName shouldBe "ANY"
      iteratorAssignmentRhs.argumentIndex shouldBe 2
      iteratorAssignmentRhs.code shouldBe "aList.iterator()"
      iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      iteratorAssignmentRhs.typeFullName shouldBe "java.util.Iterator"
      iteratorAssignmentRhs.methodFullName shouldBe "java.util.List.iterator:java.util.Iterator()"
      iteratorAssignmentRhs.signature shouldBe "java.util.Iterator()"
      iteratorLocal.referencingIdentifiers.id.l.contains(iteratorAssignmentLhs.id) shouldBe true

      val List(iteratorAssignmentRhsArg: Identifier) = iteratorAssignmentRhs.argument.l: @unchecked
      iteratorAssignmentRhsArg.code shouldBe "aList"
      iteratorAssignmentRhsArg.name shouldBe "aList"
      iteratorAssignmentRhsArg.argumentIndex shouldBe 0
      iteratorAssignmentRhsArg.typeFullName shouldBe "java.util.List"

      val controlStructure = cpg.controlStructure.head
      controlStructure.controlStructureType shouldBe ControlStructureTypes.WHILE
      controlStructure.order shouldBe 3
      controlStructure.condition.size shouldBe 1

      val List(controlStructureFirstChild: Call, controlStructureSecondChild: Block) =
        controlStructure.astChildren.l: @unchecked
      controlStructureFirstChild.order shouldBe 1
      controlStructureFirstChild.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      controlStructureFirstChild.methodFullName shouldBe "kotlin.collections.Iterator.hasNext:boolean()"
      controlStructureFirstChild.name shouldBe "hasNext"
      controlStructureFirstChild.signature shouldBe "boolean()"
      controlStructureSecondChild.order shouldBe 2

      val List(dVal1: Local, dVal2: Local, tmp: Local) = controlStructureSecondChild.astChildren.take(3).l: @unchecked
      dVal1.order shouldBe 1
      dVal1.code shouldBe "dVal1"
      dVal1.name shouldBe "dVal1"
      dVal1.typeFullName shouldBe "java.lang.String"
      dVal2.order shouldBe 2
      dVal2.code shouldBe "dVal2"
      dVal2.name shouldBe "dVal2"
      dVal2.typeFullName shouldBe "int"
      tmp.order shouldBe 3
      tmp.code shouldBe "tmp_1"
      tmp.name shouldBe "tmp_1"
      // tmp.typeFullName shouldBe "xxx"
      // TODO: test more here

      val List(getNext: Call, component1: Call, component2: Call) =
        controlStructureSecondChild.astChildren.slice(3, 6).l: @unchecked
      getNext.order shouldBe 4
      getNext.methodFullName shouldBe Operators.assignment
      getNext.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      getNext.code shouldBe "tmp_1 = iterator_1.next()"
      component1.order shouldBe 5
      component1.code shouldBe "dVal1 = tmp_1.component1()"
      component1.name shouldBe Operators.assignment
      component1.methodFullName shouldBe Operators.assignment
      component1.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      component1.signature shouldBe ""
      component2.order shouldBe 6
      component2.code shouldBe "dVal2 = tmp_1.component2()"
      component2.name shouldBe Operators.assignment
      component2.methodFullName shouldBe Operators.assignment
      component2.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      component2.signature shouldBe ""

      val List(getNextFirstArg: Identifier, getNextSecondArg: Call) = getNext.argument.l: @unchecked
      getNextFirstArg.order shouldBe 1
      getNextFirstArg.argumentIndex shouldBe 1
      getNextFirstArg.code shouldBe "tmp_1"
      getNextFirstArg.name shouldBe "tmp_1"
      tmp.referencingIdentifiers.id.l.contains(getNextFirstArg.id) shouldBe true

      getNextSecondArg.order shouldBe 2
      getNextSecondArg.argumentIndex shouldBe 2
      getNextSecondArg.code shouldBe "iterator_1.next()"
      getNextSecondArg.methodFullName shouldBe "kotlin.collections.Iterator.next:java.lang.Object()"
      getNextSecondArg.name shouldBe "next"
      getNextSecondArg.signature shouldBe "java.lang.Object()"
      getNextSecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(getNextSecondArgFirstArg: Identifier) = getNextSecondArg.argument.l: @unchecked
      getNextSecondArgFirstArg.order shouldBe 1
      getNextSecondArgFirstArg.argumentIndex shouldBe 0
      getNextSecondArgFirstArg.code shouldBe "iterator_1"
      getNextSecondArgFirstArg.typeFullName shouldBe "ANY"
      iteratorLocal.referencingIdentifiers.id.l.contains(getNextSecondArgFirstArg.id) shouldBe true

      val List(component1FirstArg: Identifier, component1SecondArg: Call) = component1.argument.l: @unchecked
      component1FirstArg.order shouldBe 1
      component1FirstArg.argumentIndex shouldBe 1
      component1FirstArg.code shouldBe "dVal1"
      dVal1.referencingIdentifiers.id.l.contains(component1FirstArg.id) shouldBe true

      component1SecondArg.order shouldBe 2
      component1SecondArg.argumentIndex shouldBe 2
      component1SecondArg.code shouldBe "tmp_1.component1()"
      component1SecondArg.methodFullName shouldBe "mypkg.AClass.component1:java.lang.String()"
      component1SecondArg.name shouldBe "component1"
      component1SecondArg.signature shouldBe "java.lang.String()"
      component1SecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(tmpInComponent1Call: Identifier) = component1SecondArg.argument.l: @unchecked
      tmp.referencingIdentifiers.id.l.contains(tmpInComponent1Call.id) shouldBe true

      val List(component2FirstArg: Identifier, component2SecondArg: Call) = component2.argument.l: @unchecked
      component2FirstArg.order shouldBe 1
      component2FirstArg.argumentIndex shouldBe 1
      component2FirstArg.code shouldBe "dVal2"
      dVal2.referencingIdentifiers.id.l.contains(component2FirstArg.id) shouldBe true

      component2SecondArg.order shouldBe 2
      component2SecondArg.argumentIndex shouldBe 2
      component2SecondArg.code shouldBe "tmp_1.component2()"
      component2SecondArg.methodFullName shouldBe "mypkg.AClass.component2:int()"
      component2SecondArg.name shouldBe "component2"
      component2SecondArg.signature shouldBe "int()"
      component2SecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(tmpInComponent2Call: Identifier) = component2SecondArg.argument.l: @unchecked
      tmp.referencingIdentifiers.id.l.contains(tmpInComponent2Call.id) shouldBe true

      val List(blockInsideBody: Block) = controlStructureSecondChild.astChildren.drop(6).l: @unchecked
      blockInsideBody.order shouldBe 7
      val List(statementInsideBody: Call, secondStatementInsideBody: Call) = blockInsideBody.astChildren.l: @unchecked
      statementInsideBody.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      statementInsideBody.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      statementInsideBody.code shouldBe "println(dVal1)"
      statementInsideBody.argument.size shouldBe 1

      secondStatementInsideBody.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      secondStatementInsideBody.methodFullName shouldBe "kotlin.io.println:void(int)"
      secondStatementInsideBody.code shouldBe "println(dVal2)"
      secondStatementInsideBody.argument.size shouldBe 1

      val List(statementInsideBodyFirstArg: Identifier) = statementInsideBody.argument.l: @unchecked
      statementInsideBodyFirstArg.order shouldBe 1
      statementInsideBodyFirstArg.argumentIndex shouldBe 1
      statementInsideBodyFirstArg.code shouldBe "dVal1"
      statementInsideBodyFirstArg.typeFullName shouldBe "java.lang.String"
      dVal1.referencingIdentifiers.id.l.contains(statementInsideBodyFirstArg.id) shouldBe true

      val List(secondStatementInsideBodyFirstArg: Identifier) = secondStatementInsideBody.argument.l: @unchecked
      secondStatementInsideBodyFirstArg.order shouldBe 1
      secondStatementInsideBodyFirstArg.argumentIndex shouldBe 1
      secondStatementInsideBodyFirstArg.code shouldBe "dVal2"
      secondStatementInsideBodyFirstArg.typeFullName shouldBe "int"
      dVal2.referencingIdentifiers.id.l.contains(secondStatementInsideBodyFirstArg.id) shouldBe true
    }
  }

  "CPG for code with `for-in` loop which has a destructuring declaration loop parameter with an `_` entry" should {
    val cpg = code("""
        |package mypkg
        |
        |data class AClass(val x: String, val y: Int)
        |
        |fun main() {
        |    val a1 = AClass("1x", 1)
        |    val a2 = AClass("2x", 2)
        |    val aList = listOf(a1, a2)
        |    for ((dVal1, _) in aList) {
        |        println(dVal1)
        |    }
        |//prints:
        |//```
        |//1x
        |//2x
        |//```
        |}
        |""".stripMargin)

    "should contain a correctly lowered representation" in {
      val expectedIteratorLocalName = "iterator_1"
      val List(iteratorLocal)       = cpg.local.nameExact(expectedIteratorLocalName).l
      iteratorLocal.name shouldBe expectedIteratorLocalName
      iteratorLocal.code shouldBe expectedIteratorLocalName
      iteratorLocal.typeFullName shouldBe "ANY"

      // TODO: add test for the block in here
      val iteratorAssignment = cpg.call.code("iterator.*itera.*").head
      iteratorAssignment.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      iteratorAssignment.name shouldBe Operators.assignment
      iteratorAssignment.methodFullName shouldBe Operators.assignment
      iteratorAssignment.code shouldBe expectedIteratorLocalName + " = " + "aList.iterator()"

      val List(iteratorAssignmentLhs: Identifier, iteratorAssignmentRhs: Call) =
        iteratorAssignment.argument.l: @unchecked
      iteratorAssignmentLhs.argumentIndex shouldBe 1
      iteratorAssignmentLhs.code shouldBe expectedIteratorLocalName
      iteratorAssignmentLhs.typeFullName shouldBe "ANY"
      iteratorAssignmentRhs.argumentIndex shouldBe 2
      iteratorAssignmentRhs.code shouldBe "aList.iterator()"
      iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      iteratorAssignmentRhs.typeFullName shouldBe "java.util.Iterator"
      iteratorAssignmentRhs.methodFullName shouldBe "java.util.List.iterator:java.util.Iterator()"
      iteratorAssignmentRhs.signature shouldBe "java.util.Iterator()"
      iteratorLocal.referencingIdentifiers.id.l.contains(iteratorAssignmentLhs.id) shouldBe true

      val List(iteratorAssignmentRhsArg: Identifier) = iteratorAssignmentRhs.argument.l: @unchecked
      iteratorAssignmentRhsArg.code shouldBe "aList"
      iteratorAssignmentRhsArg.name shouldBe "aList"
      iteratorAssignmentRhsArg.argumentIndex shouldBe 0
      iteratorAssignmentRhsArg.typeFullName shouldBe "java.util.List"

      val controlStructure = cpg.controlStructure.head
      controlStructure.controlStructureType shouldBe ControlStructureTypes.WHILE
      controlStructure.order shouldBe 3
      controlStructure.condition.size shouldBe 1

      val List(controlStructureFirstChild: Call, controlStructureSecondChild: Block) =
        controlStructure.astChildren.l: @unchecked
      controlStructureFirstChild.order shouldBe 1
      controlStructureFirstChild.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      controlStructureFirstChild.methodFullName shouldBe "kotlin.collections.Iterator.hasNext:boolean()"
      controlStructureFirstChild.name shouldBe "hasNext"
      controlStructureFirstChild.signature shouldBe "boolean()"
      controlStructureSecondChild.order shouldBe 2

      val List(dVal1: Local, tmp: Local) = controlStructureSecondChild.astChildren.take(2).l: @unchecked
      dVal1.order shouldBe 1
      dVal1.code shouldBe "dVal1"
      dVal1.name shouldBe "dVal1"
      dVal1.typeFullName shouldBe "java.lang.String"
      tmp.order shouldBe 2
      tmp.code shouldBe "tmp_1"
      tmp.name shouldBe "tmp_1"
      // tmp.typeFullName shouldBe "xxx"
      // TODO: test more here

      val List(getNext: Call, component1: Call) =
        controlStructureSecondChild.astChildren.slice(2, 4).l: @unchecked
      getNext.order shouldBe 3
      getNext.methodFullName shouldBe Operators.assignment
      getNext.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      getNext.code shouldBe "tmp_1 = iterator_1.next()"
      component1.order shouldBe 4
      component1.code shouldBe "dVal1 = tmp_1.component1()"
      component1.name shouldBe Operators.assignment
      component1.methodFullName shouldBe Operators.assignment
      component1.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      component1.signature shouldBe ""

      val List(getNextFirstArg: Identifier, getNextSecondArg: Call) = getNext.argument.l: @unchecked
      getNextFirstArg.order shouldBe 1
      getNextFirstArg.argumentIndex shouldBe 1
      getNextFirstArg.code shouldBe "tmp_1"
      getNextFirstArg.name shouldBe "tmp_1"
      tmp.referencingIdentifiers.id.l.contains(getNextFirstArg.id) shouldBe true

      getNextSecondArg.order shouldBe 2
      getNextSecondArg.argumentIndex shouldBe 2
      getNextSecondArg.code shouldBe "iterator_1.next()"
      getNextSecondArg.methodFullName shouldBe "kotlin.collections.Iterator.next:java.lang.Object()"
      getNextSecondArg.name shouldBe "next"
      getNextSecondArg.signature shouldBe "java.lang.Object()"
      getNextSecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(getNextSecondArgFirstArg: Identifier) = getNextSecondArg.argument.l: @unchecked
      getNextSecondArgFirstArg.order shouldBe 1
      getNextSecondArgFirstArg.argumentIndex shouldBe 0
      getNextSecondArgFirstArg.code shouldBe "iterator_1"
      getNextSecondArgFirstArg.typeFullName shouldBe "ANY"
      iteratorLocal.referencingIdentifiers.id.l.contains(getNextSecondArgFirstArg.id) shouldBe true

      val List(component1FirstArg: Identifier, component1SecondArg: Call) = component1.argument.l: @unchecked
      component1FirstArg.order shouldBe 1
      component1FirstArg.argumentIndex shouldBe 1
      component1FirstArg.code shouldBe "dVal1"
      dVal1.referencingIdentifiers.id.l.contains(component1FirstArg.id) shouldBe true

      component1SecondArg.order shouldBe 2
      component1SecondArg.argumentIndex shouldBe 2
      component1SecondArg.code shouldBe "tmp_1.component1()"
      component1SecondArg.methodFullName shouldBe "mypkg.AClass.component1:java.lang.String()"
      component1SecondArg.name shouldBe "component1"
      component1SecondArg.signature shouldBe "java.lang.String()"
      component1SecondArg.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH

      val List(tmpInComponent1Call: Identifier) = component1SecondArg.argument.l: @unchecked
      tmp.referencingIdentifiers.id.l.contains(tmpInComponent1Call.id) shouldBe true

      val List(blockInsideBody: Block) = controlStructureSecondChild.astChildren.drop(4).l: @unchecked
      blockInsideBody.order shouldBe 5
      val List(statementInsideBody: Call) = blockInsideBody.astChildren.l: @unchecked
      statementInsideBody.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      statementInsideBody.methodFullName shouldBe "kotlin.io.println:void(java.lang.Object)"
      statementInsideBody.code shouldBe "println(dVal1)"
      statementInsideBody.argument.size shouldBe 1

      val List(statementInsideBodyFirstArg: Identifier) = statementInsideBody.argument.l: @unchecked
      statementInsideBodyFirstArg.order shouldBe 1
      statementInsideBodyFirstArg.argumentIndex shouldBe 1
      statementInsideBodyFirstArg.code shouldBe "dVal1"
      statementInsideBodyFirstArg.typeFullName shouldBe "java.lang.String"
      dVal1.referencingIdentifiers.id.l.contains(statementInsideBodyFirstArg.id) shouldBe true
    }
  }

  // TODO: also add test for the loop range, when it is with downTo or whatever
}
