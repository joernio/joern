package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language.*

class SafeQualifiedExpressionsTests extends KotlinCode2CpgFixture(withOssDataflow = false) {
  implicit val resolver: ICallResolver = NoResolve

  "CPG for code with safe qualified expression" should {
    val cpg = code("""
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
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(2)
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
    }

    "should contain a CALL node for `r?.exec.*` and `r.exec.*` with the same METHOD_FULL_NAME" in {
      val List(c1) = cpg.call.code(".*exec.*").take(1).l
      val List(c2) = cpg.call.code(".*exec.*").slice(1, 2).l
      c1.methodFullName shouldBe c2.methodFullName
    }
  }
}
