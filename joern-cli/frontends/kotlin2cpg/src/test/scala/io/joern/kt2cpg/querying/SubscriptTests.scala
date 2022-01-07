package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class SubscriptTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kt2CpgTestContext.buildCpg("""
      |fun foo(): Int {
      |  val names = listOf(1, 2, 3)
      |  return names[0]
      |}
      |""".stripMargin)

  "should find a call node for `Operators.indexAccess`" in {
    val List(c) = cpg.call(Operators.indexAccess).l
    c.code shouldBe "names[0]"
  }
}
