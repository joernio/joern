package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UnaryOpTests extends AnyFreeSpec with Matchers {

  "CPG for code with calls to unary operators" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg(
      """
        |fun main(args : Array<String>) {
        |  val x: Int = 5
        |  val y: Boolean = true
        |  println(+x)
        |  println(-x)
        |  println(!y)
        |  ++x
        |  --x
        |  x++
        |  x--
        |}
        |""".stripMargin
    )

    "should contain correct number of calls" in {
      cpg.call.size should not be 0
    }

    "should contain a call node for the `plus` operator" in {
      cpg.call(Operators.plus).size should not be 0
    }

    "should contain a call node for the `minus` operator" in {
      cpg.call(Operators.minus).size should not be 0
    }

    "should contain a call node for the `logicalNot` operator" in {
      cpg.call(Operators.logicalNot).size should not be 0
    }

    "should contain a call node for the `preIncrement` operator" in {
      cpg.call(Operators.preIncrement).size should not be 0
    }

    "should contain a call node for the `preDecrement` operator" in {
      cpg.call(Operators.preDecrement).size should not be 0
    }

    "should contain a call node for `plus` op with correct fields" in {
      cpg.call(Operators.plus).size shouldBe 1

      val List(p) = cpg.call(Operators.plus).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(4)
      p.code shouldBe "+x"
    }

    "should contain a call node for `minus` op with correct fields" in {
      cpg.call(Operators.minus).size shouldBe 1

      val List(p) = cpg.call(Operators.minus).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(5)
      p.code shouldBe "-x"
    }

    "should contain a call node for `logicalNot` op with correct fields" in {
      cpg.call(Operators.logicalNot).size shouldBe 1

      val List(p) = cpg.call(Operators.logicalNot).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(6)
      p.code shouldBe "!y"
    }

    "should contain a call node for `preIncrement` op with correct fields" in {
      cpg.call(Operators.preIncrement).size shouldBe 1

      val List(p) = cpg.call(Operators.preIncrement).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(7)
      p.code shouldBe "++x"
    }

    "should contain a call node for `preDecrement` op with correct fields" in {
      cpg.call(Operators.preDecrement).size shouldBe 1

      val List(p) = cpg.call(Operators.preDecrement).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(8)
      p.code shouldBe "--x"
    }

    "should contain a call node for `postIncrement` op with correct fields" in {
      cpg.call(Operators.postIncrement).size shouldBe 1

      val List(p) = cpg.call(Operators.postIncrement).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(9)
      p.code shouldBe "x++"
    }

    "should contain a call node for `postDecrement` op with correct fields" in {
      cpg.call(Operators.postDecrement).size shouldBe 1

      val List(p) = cpg.call(Operators.postDecrement).l
      p.argument.size shouldBe 1
      p.lineNumber shouldBe Some(10)
      p.code shouldBe "x--"
    }
  }
}
