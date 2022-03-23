package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class LocalTests extends AnyFreeSpec with Matchers {

  "CPG for code simple local declarations" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |fun foo() {
        |  val x: Int = 1
        |  val y: Int = 2
        |  println(x + y)
        |}
        |""".stripMargin)

    "should contain locals `x` and `y` with correct fields set" in {
      val List(x) = cpg.local("x").l
      x.code shouldBe "val x: Int = 1" // TODO: decide whether to remove the `= 1`
      x.typeFullName shouldBe "java.lang.Integer"
      x.lineNumber shouldBe Some(2)
      x.columnNumber shouldBe Some(6)
      x.order shouldBe 1

      val List(y) = cpg.local("y").l
      y.code shouldBe "val y: Int = 2" // TODO: decide whether to remove the `= 2`
      y.typeFullName shouldBe "java.lang.Integer"
      y.lineNumber shouldBe Some(3)
      y.columnNumber shouldBe Some(6)
      y.order shouldBe 3
    }
  }

  "CPG for code simple local declarations without explicit types" - {
    lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
        |fun foo() {
        |  val x = 1
        |  val y = 2
        |  println(x + y)
        |}
        |""".stripMargin)

    "should contain locals `x` and `y` with correct TYPE_FULL_NAMEs set" in {
      val List(x: Local) = cpg.local("x").l
      x.typeFullName shouldBe "java.lang.Integer"

      val List(y: Local) = cpg.local("y").l
      y.typeFullName shouldBe "java.lang.Integer"
    }
  }

}
