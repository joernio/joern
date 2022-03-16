package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kt2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class DataClassTests extends AnyFreeSpec with Matchers {
  "CPG for code with simple data class" - {
    lazy val cpg = Kt2CpgTestContext.buildCpg("""
        |package mypkg
        |
        |data class Result(val p: Int, val q: String)
        |fun main() {
        |  val x = Result(41414141, "AMESSAGE")
        |  println(x.p)
        |  println(x.q)
        |}
        |""".stripMargin)

    "should contain METHOD nodes for the implicit componentX-methods of the data class" in {
      val List(firstMethod, secondMethod) = cpg.typeDecl.nameExact("Result").method.name("component.*").l

      firstMethod.name shouldBe "component1"
      firstMethod.fullName shouldBe "mypkg.Result.component1:java.lang.Integer()"
      firstMethod.signature shouldBe "java.lang.Integer()"
      firstMethod.block.size shouldBe 1

      secondMethod.name shouldBe "component2"
      secondMethod.fullName shouldBe "mypkg.Result.component2:java.lang.String()"
      secondMethod.signature shouldBe "java.lang.String()"
      secondMethod.block.size shouldBe 1
    }
  }
}
