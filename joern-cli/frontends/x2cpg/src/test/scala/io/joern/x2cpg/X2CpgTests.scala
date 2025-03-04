package io.joern.x2cpg

import better.files.File
import io.joern.x2cpg.utils.FileUtil
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.Files

class X2CpgTests extends AnyWordSpec with Matchers {

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
      val cpg = X2Cpg.newEmptyCpg(Some(file.toString))
      cpg.close()
      Files.exists(file) shouldBe true
      Files.size(file) should not be 0
    }

    "overwrite existing file to create empty CPG" in {
      File.usingTemporaryFile("x2cpg") { file =>
        file.exists shouldBe true
        Files.size(file.path) shouldBe 0
        val cpg = X2Cpg.newEmptyCpg(Some(file.path.toString))
        cpg.graph.allNodes.hasNext shouldBe false
        cpg.close()
        file.exists shouldBe true
        Files.size(file.path) should not be 0
      }
    }
  }
}
