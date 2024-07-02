package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*

class MetaDataTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
      |class Foo {}
      |""".stripMargin).cpg

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe "JAVA"
    x.version shouldBe "0.1"
    x.overlays shouldBe List("base", "controlflow", "typerel", "callgraph")
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in().l shouldBe List()
    cpg.metaData.out().l shouldBe List()
  }

}
