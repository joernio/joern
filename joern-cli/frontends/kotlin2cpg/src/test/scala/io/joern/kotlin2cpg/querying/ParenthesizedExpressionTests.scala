package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ParenthesizedExpressionTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple paranthesized expression " - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = ("a" + "b")
        |  println(x)
        |}
        |""".stripMargin)

    "should contain a CALL node for the expression inside it" in {
      val List(c) = cpg.call.methodFullName(Operators.addition).l
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(19)
      c.code shouldBe "\"a\" + \"b\""
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with a paranthesized expression as part of a dot-qualified expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = ("a" + "b").toLowerCase()
        |  println(x)
        |}
        |""".stripMargin)

    "should contain a call node for the expression inside it" in {
      val List(c) = cpg.call.methodFullName(Operators.addition).l
      c.lineNumber shouldBe Some(4)
      c.columnNumber shouldBe Some(19)
      c.code shouldBe "\"a\" + \"b\""
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.lang.String"
    }
  }
}
