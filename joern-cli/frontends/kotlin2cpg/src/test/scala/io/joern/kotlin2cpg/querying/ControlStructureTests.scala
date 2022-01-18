package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Operators}
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ControlStructureTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple if-else" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
        |    for (i in 1..5) {
        |      if (i == 1) {
        |        continue
        |      }
        |      if (i == 5) {
        |        break
        |      }
        |      print(i)
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
      cpg.method.name("methodFoo").ifBlock.condition.code.l shouldBe List("x > 1", "i == 1", "i == 5")
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

    "should identify `for` block" in {
      cpg.method.name("methodFoo").forBlock.code.size shouldBe 1
    }

    "should identify `while` block" in {
      cpg.method.name("methodFoo").whileBlock.code.size shouldBe 1
    }

    "should identify `do` block" in {
      cpg.method.name("methodFoo").doBlock.code.size shouldBe 1
    }

    "should identify `break`" in {
      cpg.method.name("methodFoo").break.code.l shouldBe List("break")
    }

    "should identify `continue`" in {
      cpg.method.name("methodFoo").continue.code.l shouldBe List("continue")
    }
  }

  "CPG for code with simple `for`-statements" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      c.methodFullName shouldBe "kotlin.collections.Collection.contains:kotlin.Boolean(kotlin.String)"
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(5)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH.toString
      c.signature shouldBe "kotlin.Boolean(kotlin.String)"
    }
  }

  "CPG for code with try-catch-finally statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      val List(cs) = matchTryQ.l
      cs.lineNumber shouldBe Some(4)
      cs.columnNumber shouldBe Some(3)

      val List(c1, c2, c3) = matchTryQ.astChildren.l
      c1.order shouldBe 1
      c2.order shouldBe 2
      c3.order shouldBe 3
    }
  }

  "CPG for code with try-catch statement" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
      val List(cs) = matchTryQ.l
      cs.lineNumber shouldBe Some(4)
      cs.columnNumber shouldBe Some(3)

      val List(c1, c2) = matchTryQ.astChildren.l
      c1.order shouldBe 1
      c2.order shouldBe 2
    }
  }

  "CPG for code with range operators inside for-statements" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |package mypkg
      |
      |fun foo() {
      |  for (i in 1..4 step 2) print(i)
      |
      |  for (i in 8 downTo 1 step 2) print(i)
      |
      |  for (i in 1 until 10) {
      |    print(i)
      |  }
      |}
      |""".stripMargin)

    "should contain CALL nodes for the loop expressions with the correct props set" in {
      val List(c1) = cpg.call.code(".*4.*step.*").l
      c1.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      c1.methodFullName shouldBe "kotlin.ranges.step:kotlin.ranges.IntProgression(kotlin.Int)"
      c1.signature shouldBe "kotlin.ranges.IntProgression(kotlin.Int)"
      c1.typeFullName shouldBe "kotlin.ranges.IntProgression"
      c1.lineNumber shouldBe Some(4)
      c1.columnNumber shouldBe Some(12)

      val List(c2) = cpg.call.code(".*8.*downTo.*step.*").l
      c2.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      c2.methodFullName shouldBe "kotlin.ranges.step:kotlin.ranges.IntProgression(kotlin.Int)"
      c2.signature shouldBe "kotlin.ranges.IntProgression(kotlin.Int)"
      c2.typeFullName shouldBe "kotlin.ranges.IntProgression"
      c2.lineNumber shouldBe Some(6)
      c2.columnNumber shouldBe Some(12)

      val List(c3) = cpg.call.code(".*until.*10").l
      c3.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH.toString
      c3.methodFullName shouldBe "kotlin.ranges.until:kotlin.ranges.IntRange(kotlin.Int)"
      c3.signature shouldBe "kotlin.ranges.IntRange(kotlin.Int)"
      c3.typeFullName shouldBe "kotlin.ranges.IntRange"
      c3.lineNumber shouldBe Some(8)
      c3.columnNumber shouldBe Some(12)
    }
  }

}
