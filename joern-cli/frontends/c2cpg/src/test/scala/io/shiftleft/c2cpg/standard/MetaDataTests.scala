package io.shiftleft.c2cpg.standard

import io.shiftleft.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.layers.{Base, CallGraph, ControlFlow, TypeRelations}

class MetaDataTests extends CCodeToCpgSuite {

  override val code: String =
    """
      |
      |""".stripMargin

  "should contain exactly one node with all mandatory fields set" in {
    val List(x) = cpg.metaData.l
    x.language shouldBe Languages.NEWC
    x.version shouldBe "0.1"
    x.overlays shouldBe List(Base.overlayName,
                             ControlFlow.overlayName,
                             TypeRelations.overlayName,
                             CallGraph.overlayName)
    // C-frontend does not set hash for entire CPG.
    // Change this assertion if it is supposed to.
    x.hash shouldBe None
  }

  "should not have any incoming or outgoing edges" in {
    cpg.metaData.size shouldBe 1
    cpg.metaData.in.l shouldBe List()
    cpg.metaData.out.l shouldBe List()
  }

}
