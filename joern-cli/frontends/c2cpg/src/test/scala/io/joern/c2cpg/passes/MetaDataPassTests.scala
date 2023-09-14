package io.joern.c2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import io.joern.x2cpg.passes.frontend.MetaDataPass
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class MetaDataPassTests extends AnyWordSpec with Matchers {

  "MetaDataPass" should {
    val cpg = Cpg.empty
    new MetaDataPass(cpg, Languages.C, "").createAndApply()

    "create exactly two nodes" in {
      cpg.graph.allNodes.size shouldBe 2
    }

    "create no edges" in {
      cpg.graph.allNodes.outE.size shouldBe 0
    }

    "create a metadata node with correct language" in {
      cpg.metaData.language.l shouldBe List("C")
    }

    "create a '<global>' NamespaceBlock" in {
      cpg.namespaceBlock.name.l shouldBe List(NamespaceTraversal.globalNamespaceName)
    }

  }
}
