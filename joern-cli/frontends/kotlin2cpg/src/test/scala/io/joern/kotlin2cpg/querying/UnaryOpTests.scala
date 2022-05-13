package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class UnaryOpTests extends AnyFreeSpec with Matchers {

  "CPG for code with calls to unary operators" - {
    lazy val cpg = TestContext.buildCpg("""
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
        |""".stripMargin)

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
      val List(c) = cpg.call(Operators.plus).l
      c.code shouldBe "+x"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(5)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `minus` op with correct fields" in {
      val List(c) = cpg.call(Operators.minus).l
      c.code shouldBe "-x"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(6)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `logicalNot` op with correct fields" in {
      val List(c) = cpg.call(Operators.logicalNot).l
      c.code shouldBe "!y"
      c.typeFullName shouldBe "boolean"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(7)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `preIncrement` op with correct fields" in {
      val List(c) = cpg.call(Operators.preIncrement).l
      c.code shouldBe "++x"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(8)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `preDecrement` op with correct fields" in {
      val List(c) = cpg.call(Operators.preDecrement).l
      c.code shouldBe "--x"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(9)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `postIncrement` op with correct fields" in {
      val List(c) = cpg.call(Operators.postIncrement).l
      c.code shouldBe "x++"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(10)

      c.argument.size shouldBe 1
    }

    "should contain a call node for `postDecrement` op with the correct props set" in {
      val List(c) = cpg.call(Operators.postDecrement).l
      c.code shouldBe "x--"
      c.typeFullName shouldBe "int"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.lineNumber shouldBe Some(11)

      c.argument.size shouldBe 1
    }
  }
}
