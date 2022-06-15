package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import io.shiftleft.semanticcpg.language._

class ObjectExpressionTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple object expression" should {
    lazy val cpg = code("""
        |fun foo() {
        |  val bar = object {
        |    override val baz = 1
        |  }
        |  println(bar.y)
        |}
        |""".stripMargin)

    "should contain an unknown node for the object expression" in {
      val List(u) = cpg.all.filter(_.isInstanceOf[Unknown]).map(_.asInstanceOf[Unknown]).l
      u.lineNumber shouldBe Some(3)
      u.columnNumber shouldBe Some(12)
    }
  }

  "CPG for code with simple object expression with apply called after it" should {
    lazy val cpg = code("""
        |package main
        |
        |fun main() {
        |    val o = object { var prop = 1 }.apply { prop = 2 }
        |    println(o.prop) // prints `2`
        |}
        |
        |""".stripMargin)

    "should contain a CALL node for the `apply` with the correct props set" in {
      val List(c) = cpg.call.methodFullName(".*apply.*").l
      c.methodFullName shouldBe "java.lang.Object.apply:java.lang.Object(kotlin.Function1)"
      c.signature shouldBe "java.lang.Object(kotlin.Function1)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    }
  }
}
