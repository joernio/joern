package io.joern.c2cpg

import io.joern.c2cpg.fixtures.TestProjectFixture
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.codepropertygraph.generated.nodes.{NamespaceBlock, TypeDecl}
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb._
import overflowdb.traversal._

class ProgramStructureTests extends AnyWordSpec with Matchers {

  private val fixture: TestProjectFixture = TestProjectFixture("structure")

  "Program structure of test project" should {

    "contain <global> namespace block node" in {
      val namespaceBlocks =
        fixture.traversalSource
          .label(NamespaceBlock.Label)
          .has(Properties.FULL_NAME -> NamespaceTraversal.globalNamespaceName)
          .l

      namespaceBlocks.size shouldBe 1
    }

    "contain type-decl node" in {
      val nodes = fixture.traversalSource.label(TypeDecl.Label).l
      nodes.size should be > 0
    }

  }

}
