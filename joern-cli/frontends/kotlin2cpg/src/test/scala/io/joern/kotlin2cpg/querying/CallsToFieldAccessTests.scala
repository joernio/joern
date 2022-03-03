package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CallsToFieldAccessTests extends AnyFreeSpec with Matchers {
  "CPG for code with class method referencing member in a call" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |class AClass(private val x: String) {
        |    fun printX() {
        |        println(x)
        |    }
        |}
        |
        |fun main() {
        |    val a = AClass("A_MESSAGE")
        |    a.printX()
        |}
        |""".stripMargin)

    "should contain a CALL node for the referenced member with the correct props set" in {
      val List(c) = cpg.call.codeExact("println(x)").argument.isCall.l
      c.code shouldBe "this.x"
      c.name shouldBe "x"
      c.dispatchType shouldBe DispatchTypes.DYNAMIC_DISPATCH
      c.lineNumber shouldBe Some(5)
      c.columnNumber shouldBe Some(16)
    }
  }
}
