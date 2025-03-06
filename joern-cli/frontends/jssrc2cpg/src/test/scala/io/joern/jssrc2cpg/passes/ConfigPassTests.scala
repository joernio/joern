package io.joern.jssrc2cpg.passes

import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.FileUtil
import io.joern.x2cpg.utils.FileUtil.*
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.nio.file.{Files, Path}

class ConfigPassTests extends AnyWordSpec with Matchers {

  "ConfigPass for Vue files" should {

    "generate ConfigFiles correctly for simply Vue project" in {
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.vue"
        val fileB = dir / "b.vue"

        Files.writeString(fileA, "someCodeA();")
        Files.writeString(fileB, "someCodeB();")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.toString)).createAndApply()

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
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.conf.js"
        val fileB = dir / "b.config.js"
        val fileC = dir / "c.json"

        Files.writeString(fileA, "a")
        Files.writeString(fileB, "b")
        Files.writeString(fileC, "c")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.toString)).createAndApply()

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
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.conf.js"
        val fileB = dir / "b.config.js"
        val fileC = dir / "c.json"

        Files.writeString(fileA, "a")
        Files.writeString(fileB, "b")
        Files.writeString(fileC, "c")

        // should be ignored
        val d = (dir / Defines.NodeModulesFolder).createWithParentsIfNotExists(asDirectory = true)
        Files.writeString((d / "d.json"), "d")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.toString)).createAndApply()

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
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA = dir / "a.html"
        val fileB = dir / "b.html"
        Files.writeString(fileA, "a")
        Files.writeString(fileB, "b")

        val cpg = Cpg.empty
        new ConfigPass(cpg, Config().withInputPath(dir.toString)).createAndApply()

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
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val fileA         = dir / "a.key"
        val fileAContents = "-----BEGIN RSA PRIVATE KEY-----\n123456789\n-----END RSA PRIVATE KEY-----"

        val fileB         = dir / "b.key"
        val fileBContents = "-----BEGIN SOME OTHER KEY-----\nthis is fine\n-----END SOME OTHER KEY-----"

        Files.writeString(fileA, fileAContents)
        Files.writeString(fileB, fileBContents)

        val cpg = Cpg.empty
        new PrivateKeyFilePass(cpg, Config().withInputPath(dir.toString)).createAndApply()

        val List(configFileA) = cpg.configFile.l
        configFileA.name shouldBe "a.key"
        configFileA.content shouldBe "Content omitted for security reasons."
      }
    }

    "ignore ConfigFiles correctly" in {
      FileUtil.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val d            = (dir / Defines.NodeModulesFolder).createWithParentsIfNotExists(asDirectory = true)
        val fileA        = d / "a.key"
        val fileAContent = "-----BEGIN RSA PRIVATE KEY-----\n123456789\n-----END RSA PRIVATE KEY-----"

        Files.writeString(fileA, fileAContent)

        val cpg = Cpg.empty
        new PrivateKeyFilePass(cpg, Config().withInputPath(dir.toString)).createAndApply()

        cpg.configFile shouldBe empty
      }
    }

  }
}
