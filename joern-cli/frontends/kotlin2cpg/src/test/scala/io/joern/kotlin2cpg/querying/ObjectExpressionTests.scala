package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Unknown
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ObjectExpressionTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple object expression" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |fun foo() {
        |  val bar = object {
        |    override val baz = 1
        |  }
        |  println(bar.y)
        |}
        |""".stripMargin)

    "should contain an unknown node for the object expression" in {
      val List(u) = cpg.all.filter(_.isInstanceOf[Unknown]).map(_.asInstanceOf[Unknown]).l
      u.lineNumber shouldBe Some(2)
      u.columnNumber shouldBe Some(12)
    }
  }

  "CPG for code with simple object expression with apply called after it" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
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
