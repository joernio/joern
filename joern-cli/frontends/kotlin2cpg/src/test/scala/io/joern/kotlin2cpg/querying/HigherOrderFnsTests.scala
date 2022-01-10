package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class HigherOrderFnsTests extends AnyFreeSpec with Matchers {
  "CPG for code with a call to fold" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |import kotlin.collections.listOf
        |import kotlin.collections.List
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
      // TODO: delete the `acc: ` inside the fullname
      c.methodFullName shouldBe "kotlin.collections.Iterable<kotlin.Int>.fold:kotlin.Int(kotlin.Int,(acc:kotlin.Int,kotlin.Int)->kotlin.Int)"
    }
  }
}
