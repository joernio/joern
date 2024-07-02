package io.joern.kotlin2cpg.querying

import io.joern.kotlin2cpg.testfixtures.KotlinCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class MetaDataTests extends KotlinCode2CpgFixture(withOssDataflow = false) {

  val cpg = code("""
      |class ClassFoo {}
      |""".stripMargin)

  "should contain exactly one META_DATA node with all mandatory fields set" in {
    val List(md) = cpg.metaData.l
    md.language shouldBe "KOTLIN"
    md.version shouldBe "0.1"
    md.overlays shouldBe List("base", "controlflow", "typerel", "callgraph")
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }
}
