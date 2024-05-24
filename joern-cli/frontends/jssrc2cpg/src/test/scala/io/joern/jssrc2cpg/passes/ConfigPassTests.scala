package io.joern.jssrc2cpg.passes

import better.files.File
import io.joern.jssrc2cpg.Config
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConfigPassTests extends AnyWordSpec with Matchers {

  "ConfigPass for Vue files" should {

    "generate ConfigFiles correctly for simply Vue project" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.vue"
        val fileB = dir / "b.vue"
        fileA.write("someCodeA();")
        fileB.write("someCodeB();")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        val List(configFileA, configFileB) = cpg.configFile.l
        configFileA.name shouldBe "a.vue"
        configFileA.content shouldBe "someCodeA();"
        configFileB.name shouldBe "b.vue"
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

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        val List(configFileA, configFileB, configFileC) = cpg.configFile.l
        configFileA.name shouldBe "a.conf.js"
        configFileA.content shouldBe "a"
        configFileB.name shouldBe "b.config.js"
        configFileB.content shouldBe "b"
        configFileC.name shouldBe "c.json"
        configFileC.content shouldBe "c"
      }
    }

    "ignore ConfigFiles correctly for simple JS project" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.conf.js"
        val fileB = dir / "b.config.js"
        val fileC = dir / "c.json"
        fileA.write("a")
        fileB.write("b")
        fileC.write("c")

        // should be ignored
        val d = (dir / Defines.NodeModulesFolder).createDirectory()
        (d / "d.json").write("d")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        val List(configFileA, configFileB, configFileC) = cpg.configFile.l
        configFileA.name shouldBe "a.conf.js"
        configFileA.content shouldBe "a"
        configFileB.name shouldBe "b.config.js"
        configFileB.content shouldBe "b"
        configFileC.name shouldBe "c.json"
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

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        val List(configFileA, configFileB) = cpg.configFile.l
        configFileA.name shouldBe "a.html"
        configFileA.content shouldBe "a"
        configFileB.name shouldBe "b.html"
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

        val cpg = Cpg.empty
        new PrivateKeyFilePass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        val List(configFileA) = cpg.configFile.l
        configFileA.name shouldBe "a.key"
        configFileA.content shouldBe "Content omitted for security reasons."
      }
    }

    "ignore ConfigFiles correctly" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val d     = (dir / Defines.NodeModulesFolder).createDirectory()
        val fileA = d / "a.key"
        fileA.write("-----BEGIN RSA PRIVATE KEY-----\n123456789\n-----END RSA PRIVATE KEY-----")

        val cpg = Cpg.empty
        new PrivateKeyFilePass(cpg, Config().withInputPath(dir.pathAsString)).createAndApply()

        cpg.configFile shouldBe empty
      }
    }

  }
}
