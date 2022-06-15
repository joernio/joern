package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies}
import io.shiftleft.semanticcpg.language._

class MethodReturnTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  "CPG for code with simple method with two parameters" should {
    lazy val cpg = code("""
      |fun foo(x: Int, y: Double): Int {
      |  return x * 2
      |}
      |""".stripMargin)

    "should have a METHOD_RETURN node with correct props set" in {
      val List(x) = cpg.method.name("foo").methodReturn.l
      x.code shouldBe "int"
      x.evaluationStrategy shouldBe EvaluationStrategies.BY_VALUE
      x.order shouldBe 4
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(4)
    }

    "should allow traversing to method" in {
      cpg.methodReturn.method.isExternal(false).name.l shouldBe List("foo")
    }
  }
}
