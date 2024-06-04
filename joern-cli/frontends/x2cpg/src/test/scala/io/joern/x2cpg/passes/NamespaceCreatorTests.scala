package io.joern.x2cpg.passes

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.{NodeTypes, Properties}
import io.shiftleft.semanticcpg.language._
import io.joern.x2cpg.passes.base.NamespaceCreator
import io.joern.x2cpg.testfixtures.EmptyGraphFixture
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._

class NamespaceCreatorTests extends AnyWordSpec with Matchers {
  "NamespaceCreateor test " in EmptyGraphFixture { graph =>
    val cpg    = new Cpg(graph)
    val block1 = graph + (NodeTypes.NAMESPACE_BLOCK, Properties.NAME -> "namespace1")
    val block2 = graph + (NodeTypes.NAMESPACE_BLOCK, Properties.NAME -> "namespace1")
    val block3 = graph + (NodeTypes.NAMESPACE_BLOCK, Properties.NAME -> "namespace2")

    val namespaceCreator = new NamespaceCreator(new Cpg(graph))
    namespaceCreator.createAndApply()

    val namespaces = cpg.namespace.l
    namespaces.size shouldBe 2
    namespaces.map(_.name).toSet shouldBe Set("namespace1", "namespace2")

    val namspaceBlocks = cpg.namespace.flatMap(_._namespaceBlockViaRefIn).toSet
    namspaceBlocks shouldBe Set(block1, block2, block3)
  }
}
