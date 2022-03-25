package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SafeQualifiedExpressionsTests extends AnyFreeSpec with Matchers {
  implicit val resolver = NoResolve

  "CPG for code with safe qualified expression" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main() {
        |  val r = Runtime.getRuntime()
        |  r?.exec("ls")
        |  r.exec("ls")
        |}
        |""".stripMargin)

    "should contain a CALL node for `r?.exec.*` with the correct properties set" in {
      val List(c) = cpg.call.code(".*exec.*").take(1).l
      c.methodFullName shouldBe "java.lang.Runtime.exec:java.lang.Process(java.lang.String)"
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(2)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "should contain a CALL node for `r?.exec.*` and `r.exec.*` with the same METHOD_FULL_NAME" in {
      val List(c1) = cpg.call.code(".*exec.*").take(1).l
      val List(c2) = cpg.call.code(".*exec.*").drop(1).take(1).l
      c1.methodFullName shouldBe c2.methodFullName
    }
  }
}
