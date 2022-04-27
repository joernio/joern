package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.semanticcpg.language._
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MethodReturnTests extends AnyFreeSpec with Matchers {

  "CPG for code with simple method with two parameters" - {
    lazy val cpg = TestContext.buildCpg("""
      |fun foo(x: Int, y: Double): Int {
      |  return x * 2
      |}
      |""".stripMargin)

    "should have METHOD_RETURN node with correct fields" in {
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.code shouldBe "RET"
      x.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      x.order shouldBe 5
      x.lineNumber shouldBe Some(1)
      x.columnNumber shouldBe Some(4)
    }

    "should allow traversing to method" in {
      cpg.methodReturn.method.isExternal(false).name.l shouldBe List("foo")
    }
  }
}
