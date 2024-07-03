package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.semanticcpg.language.*

class ParenthesizedExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple paranthesized expression " should {
    val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = ("a" + "b")
        |  println(x)
        |}
        |""".stripMargin)

    "should contain a CALL node for the expression inside it" in {
      val List(c) = cpg.call.methodFullName(Operators.addition).l
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(19)
      c.code shouldBe "\"a\" + \"b\""
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.lang.String"
    }
  }

  "CPG for code with a paranthesized expression as part of a dot-qualified expression" should {
    val cpg = code("""
        |package mypkg
        |
        |fun foo() {
        |  val x: String = ("a" + "b").toLowerCase()
        |  println(x)
        |}
        |""".stripMargin)

    "should contain a call node for the expression inside it" in {
      val List(c) = cpg.call.methodFullName(Operators.addition).l
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(19)
      c.code shouldBe "\"a\" + \"b\""
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.typeFullName shouldBe "java.lang.String"
    }
  }
}
