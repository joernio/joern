package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.Kotlin2CpgTestContext
import io.shiftleft.semanticcpg.language._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MetaDataTests extends AnyFreeSpec with Matchers {

  lazy val cpg = Kotlin2CpgTestContext.buildCpg("""
      |class ClassFoo {}
      |""".stripMargin)

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe "KOTLIN"
    x.version shouldBe "0.1"
    x.overlays shouldBe List("base", "controlflow", "typerel", "callgraph")
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }
}
