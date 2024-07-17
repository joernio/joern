package io.joern.x2cpg.passes

import flatgraph.misc.TestUtils.addNode
import io.shiftleft.codepropertygraph.generated.{Cpg, NodeTypes}
import io.shiftleft.semanticcpg.language.*
import io.joern.x2cpg.passes.base.NamespaceCreator
import io.joern.x2cpg.testfixtures.EmptyGraphFixture
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NamespaceCreatorTests extends AnyWordSpec with Matchers {
  "NamespaceCreateor test " in EmptyGraphFixture { graph =>
    val cpg    = new Cpg(graph)
    val block1 = graph.addNode(NewNamespaceBlock().name("namespace1"))
    val block2 = graph.addNode(NewNamespaceBlock().name("namespace1"))
    val block3 = graph.addNode(NewNamespaceBlock().name("namespace2"))

    val namespaceCreator = new NamespaceCreator(cpg)
    namespaceCreator.createAndApply()

    val namespaces = cpg.namespace.l
    namespaces.size shouldBe 2
    namespaces.map(_.name).toSet shouldBe Set("namespace1", "namespace2")

    val namspaceBlocks = cpg.namespace.flatMap(_._namespaceBlockViaRefIn).toSet
    namspaceBlocks shouldBe Set(block1, block2, block3)
  }
}
