package io.joern.x2cpg

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.NewFile
import io.shiftleft.semanticcpg.utils.FileUtil.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class X2CpgTests extends AnyWordSpec with Matchers {

  // Empty CPGs don't get written to disk, so just add a random node.
  private def getCpg(filename: String): Cpg = {
    val cpg       = X2Cpg.newEmptyCpg(Some(filename))
    val diffGraph = Cpg.newDiffGraphBuilder
    diffGraph.addNode(NewFile())
    flatgraph.DiffGraphApplier.applyDiff(cpg.graph, diffGraph)
    cpg
  }

  "initCpg" should {

    "create an empty in-memory CPG when no output path is given" in {
      val cpg = X2Cpg.newEmptyCpg(None)
      cpg.graph.allNodes.hasNext shouldBe false
      cpg.close()
    }

    "create file if it does not exist" in {
      val file = FileUtil.newTemporaryFile("x2cpg")
      FileUtil.delete(file)
      Files.exists(file) shouldBe false
      val cpg = getCpg(file.toString)
      cpg.close()
      Files.exists(file) shouldBe true
      Files.size(file) should not be 0
    }

    "overwrite existing file to create CPG" in {
      FileUtil.usingTemporaryFile("x2cpg") { file =>
        Files.exists(file) shouldBe true
        Files.size(file) shouldBe 0
        val cpg = getCpg(file.toString)
        cpg.graph.allNodes.size shouldBe 1
        cpg.close()
        Files.exists(file) shouldBe true
        Files.size(file) should not be 0
      }
    }
  }
}
