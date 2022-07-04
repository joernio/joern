package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.ConfigFile
import io.shiftleft.codepropertygraph.generated.NodeTypes
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import overflowdb.traversal.TraversalSource

class ConfigPassTest extends AnyWordSpec with Matchers {

  private def configFiles(cpg: Cpg): List[ConfigFile] = {
    val result = TraversalSource(cpg.graph).label(NodeTypes.CONFIG_FILE).cast[ConfigFile].toList
    result.size should not be 0
    result
  }

  "ConfigPass for Vue files" should {

    "generate ConfigFiles correctly for simply Vue project" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.vue"
        val fileB = dir / "b.vue"
        fileA.write("someCodeA();")
        fileB.write("someCodeB();")
        val cpg       = Cpg.emptyCpg
        val filenames = Seq(fileA, fileB)
        new ConfigPass(cpg, filenames, Config(inputPath = dir.pathAsString)).createAndApply()

        val allConfigFiles = configFiles(cpg)

        val configFileA =
          allConfigFiles.find(_.name == "a.vue").getOrElse(fail("ConfigFile 'a.vue' not found!"))
        val configFileB =
          allConfigFiles.find(_.name == "b.vue").getOrElse(fail("ConfigFile 'b.vue' not found!"))

        configFileA.content shouldBe "someCodeA();"
        configFileB.content shouldBe "someCodeB();"
      }
    }

  }

  "ConfigPass for other config files" should {

    "generate ConfigFiles correctly for simple JS project" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.conf.js"
        val fileB = dir / "b.config.js"
        val fileC = dir / "c.json"

        fileA.write("a")
        fileB.write("b")
        fileC.write("c")

        val cpg   = Cpg.emptyCpg
        val files = Seq(fileA, fileB, fileC)
        new ConfigPass(cpg, files, Config(inputPath = dir.pathAsString)).createAndApply()

        val allConfigFiles = configFiles(cpg)

        val configFileA =
          allConfigFiles
            .find(_.name == "a.conf.js")
            .getOrElse(fail("ConfigFile 'a.conf.js' not found!"))
        val configFileB =
          allConfigFiles
            .find(_.name == "b.config.js")
            .getOrElse(fail("ConfigFile 'b.config.js' not found!"))
        val configFileC =
          allConfigFiles.find(_.name == "c.json").getOrElse(fail("ConfigFile 'c.json' not found!"))

        configFileA.content shouldBe "a"
        configFileB.content shouldBe "b"
        configFileC.content shouldBe "c"
      }
    }

  }

  "ConfigPass for html files" should {

    "generate ConfigFiles correctly for simple JS project with html files" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.html"
        val fileB = dir / "b.html"

        fileA.write("a")
        fileB.write("b")

        val cpg   = Cpg.emptyCpg
        val files = Seq(fileA, fileB)
        new ConfigPass(cpg, files, Config(inputPath = dir.pathAsString)).createAndApply()

        val allConfigFiles = configFiles(cpg)
        val configFileA =
          allConfigFiles
            .find(_.name == "a.html")
            .getOrElse(fail("ConfigFile 'a.html' not found!"))
        val configFileB =
          allConfigFiles
            .find(_.name == "b.html")
            .getOrElse(fail("ConfigFile 'b.html' not found!"))

        configFileA.content shouldBe "a"
        configFileB.content shouldBe "b"
      }
    }

  }

  "PrivateKeyFilePass" should {

    "generate ConfigFiles correctly" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.key"
        fileA.write("-----BEGIN RSA PRIVATE KEY-----\n123456789\n-----END RSA PRIVATE KEY-----")
        val fileB = dir / "b.key"
        fileB.write("-----BEGIN SOME OTHER KEY-----\nthis is fine\n-----END SOME OTHER KEY-----")

        val cpg   = Cpg.emptyCpg
        val files = Seq(fileA, fileB)
        new PrivateKeyFilePass(cpg, files, Config(inputPath = dir.pathAsString)).createAndApply()

        val allConfigFiles = configFiles(cpg)
        val configFileA =
          allConfigFiles.find(_.name == "a.key").getOrElse(fail("ConfigFile 'a.key' not found!"))
        configFileA.content shouldBe "Content omitted for security reasons."

        allConfigFiles.find(_.name == "b.key") shouldBe empty
      }
    }

  }
}
