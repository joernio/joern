package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BooleanLogicTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple boolean op usage" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |fun main(args : Array<String>) {
        |  val w = true
        |  val x: Boolean = !w
        |  val y: Boolean = (x < w) || (w < x)
        |  val z: Boolean = (x < w) && (w < x)
        |  val p = (1 shl 2) and 0x000FF000
        |  val q = (1 shr 2) xor 0x000FF000
        |  val r = 2.inv() or (1 ushr 3)
        |  val s = 1 ushl 2
        |}
        |""".stripMargin)

    "should contain correct number of calls" in {
      cpg.call.size should not be 0
    }

    "should contain a call node for the `logicalNot` operator" in {
      cpg.call(Operators.logicalNot).size should not be 0
    }

    "should contain a call node for `logicalNot` with correct fields" in {
      cpg.call(Operators.logicalNot).size shouldBe 1

      val List(p) = cpg.call(Operators.logicalNot).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(3)
      p.code shouldBe "!w"
    }

    "should contain a call node for the `logicalOr` operator" in {
      cpg.call(Operators.logicalOr).size should not be 0
    }

    "should contain a call node for `logicalOr` op with correct fields" in {
      cpg.call(Operators.logicalOr).size shouldBe 1

      val List(p) = cpg.call(Operators.logicalOr).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(4)
      p.code shouldBe "(x < w) || (w < x)"
    }

    "should contain a call node for the `logicalAnd` operator" in {
      cpg.call(Operators.logicalAnd).size should not be 0
    }

    "should contain a call node for `logicalAnd` op with correct fields" in {
      cpg.call(Operators.logicalAnd).size shouldBe 1

      val List(p) = cpg.call(Operators.logicalAnd).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(5)
      p.code shouldBe "(x < w) && (w < x)"
    }

    "should contain a call node for the `and` operator" in {
      cpg.call(Operators.and).size should not be 0
    }

    "should contain a call node for `and` op with correct fields" in {
      cpg.call(Operators.and).size shouldBe 1

      val List(p) = cpg.call(Operators.and).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(6)
      p.code shouldBe "(1 shl 2) and 0x000FF000"
    }

    "should contain a call node for the `xor` operator" in {
      cpg.call(Operators.xor).size should not be 0
    }

    "should contain a call node for `xor` op with correct fields" in {
      cpg.call(Operators.xor).size shouldBe 1

      val List(p) = cpg.call(Operators.xor).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(7)
      p.code shouldBe "(1 shr 2) xor 0x000FF000"
    }

    "should contain a call node for the `or` operator" in {
      cpg.call(Operators.or).size should not be 0
    }

    "should contain a call node for `or` op with correct fields" in {
      cpg.call(Operators.or).size shouldBe 1

      val List(p) = cpg.call(Operators.or).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(8)
      p.code shouldBe "2.inv() or (1 ushr 3)"
    }

    "should contain a call node for the `shiftLeft` operator" in {
      cpg.call(Operators.shiftLeft).size should not be 0
    }

    "should contain two call nodes for `shiftLeft` op with correct fields" in {
      cpg.call(Operators.shiftLeft).size shouldBe 2

      val List(p) = cpg.call(Operators.shiftLeft).order(1).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(6)
      p.code shouldBe "1 shl 2"

      val List(q) = cpg.call(Operators.shiftLeft).order(2).l
      q.argument.size shouldBe 2
      q.lineNumber shouldBe Some(9)
      q.code shouldBe "1 ushl 2"
    }

    "should contain a call node for the `logicalShiftRight` operator" in {
      cpg.call(Operators.logicalShiftRight).size should not be 0
    }

    "should contain a call node for `logicalShiftRight` op with correct fields" in {
      cpg.call(Operators.logicalShiftRight).size shouldBe 1

      val List(p) = cpg.call(Operators.logicalShiftRight).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(8)
      p.code shouldBe "1 ushr 3"
    }

    "should contain a call node for the `aritmethicShiftRight` operator" in {
      cpg.call(Operators.arithmeticShiftRight).size should not be 0
    }

    "should contain a call node for `aritmeticShiftRight` op with correct fields" in {
      cpg.call(Operators.arithmeticShiftRight).size shouldBe 1

      val List(p) = cpg.call(Operators.arithmeticShiftRight).l
      p.argument.size shouldBe 2
      p.lineNumber shouldBe Some(7)
      p.code shouldBe "1 shr 2"
    }
  }
}
