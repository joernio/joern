package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.TestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HigherOrderFnsTests extends AnyFreeSpec with Matchers {
  "CPG for code with a call to stdlib's `fold`" - {
    lazy val cpg = TestContext.buildCpg("""
        |package mypkg
        |
        |fun main(args : Array<String>) {
        |     val items: List<Int> = listOf(1, 2, 3, 4, 5)
        |     val folded =
        |         items.fold(0, {acc: Int, i: Int ->
        |             val result = acc + i
        |             result
        |         })
        |     println(folded)
        |}
        |
        |""".stripMargin)

    "should contain a CALL node with the correct METHOD_FULL_NAME" in {
      val List(c) = cpg.call.methodFullName(".*fold.*").l
      c.methodFullName shouldBe "java.lang.Iterable.fold:java.lang.Object(java.lang.Object,kotlin.Function2)"
      c.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      c.signature shouldBe "java.lang.Object(java.lang.Object,kotlin.Function2)"
      c.typeFullName shouldBe "java.lang.Integer"
      c.lineNumber shouldBe Some(6)
      c.columnNumber shouldBe Some(9)
    }
  }
}
