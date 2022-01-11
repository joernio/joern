package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.{Unknown}
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class ObjectExpressionTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple object expression" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
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
}
