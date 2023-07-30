package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import better.files.File
import io.joern.jssrc2cpg.utils.PackageJsonParser
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg.newEmptyCpg

class DependenciesPassTest extends AbstractPassTest {

  "DependenciesPass" should {

    "ignore empty package.json" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PackageJsonFilename
        json.write("")
        PackageJsonParser.isValidProjectPackageJson(json.path) shouldBe false
      }
    }

    "ignore package.json without any useful content" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PackageJsonFilename
        json.write("""
            |{
            |  "name": "something",
            |  "version": "0.1.0",
            |  "description": "foobar",
            |  "main": "./target_node/index.js",
            |  "private": true
            |}
            |""".stripMargin)
        PackageJsonParser.isValidProjectPackageJson(json.path) shouldBe false
      }
    }

    "ignore package.json without dependencies" in {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PackageJsonFilename
        json.write("{}")
        PackageJsonParser.isValidProjectPackageJson(json.path) shouldBe false
      }
    }

    "generate dependency nodes correctly (no dependencies at all)" in DependencyFixture("", "{}") { cpg =>
      cpg.dependency.size shouldBe 0
    }

    "generate dependency nodes correctly (empty dependency)" in DependencyFixture(
      "",
      """
        |{
        |  "dependencies": {
        |  }
        |}
        |""".stripMargin
    ) { cpg =>
      cpg.dependency.size shouldBe 0
    }

    "generate dependency nodes correctly (simple lock dependencies)" in DependencyFixture(
      code = "",
      packageJsonContent = """
          |{
          |  "dependencies": {
          |    "dep1": {
          |      "version": "0.1"
          |    },
          |    "dep2": {
          |      "version": "0.2"
          |    }
          |  }
          |}
          |""".stripMargin,
      packageJsonName = PackageJsonParser.PackageJsonLockFilename
    ) { cpg =>
      val List(depA, depB) = cpg.dependency.l
      depA.name shouldBe "dep1"
      depA.version shouldBe "0.1"
      depB.name shouldBe "dep2"
      depB.version shouldBe "0.2"
    }

    "generate dependency nodes correctly (simple dependency)" in DependencyFixture(
      code = "",
      packageJsonContent = """
                             |{
                             |  "dependencies": {
                             |    "dep1": "0.1"
                             |  }
                             |}
                             |""".stripMargin
    ) { cpg =>
      val List(depA) = cpg.dependency.l
      depA.name shouldBe "dep1"
      depA.version shouldBe "0.1"
    }

    "generate dependency nodes correctly (different types of dependencies)" in DependencyFixture(
      code = "",
      packageJsonContent = """
        {
          "dependencies": {
            "dep1": "0.1"
          },
          "devDependencies": {
            "dep2": "0.2"
          },
          "peerDependencies": {
            "dep3": "0.3"
          },
          "optionalDependencies": {
            "dep4": "0.4"
          }
        }
        """.stripMargin
    ) { cpg =>
      val List(depA, depB, depC, depD) = cpg.dependency.l
      depA.name shouldBe "dep1"
      depA.version shouldBe "0.1"
      depB.name shouldBe "dep2"
      depB.version shouldBe "0.2"
      depC.name shouldBe "dep3"
      depC.version shouldBe "0.3"
      depD.name shouldBe "dep4"
      depD.version shouldBe "0.4"
    }

  }

  private object DependencyFixture extends Fixture {
    def apply(
      code: String,
      packageJsonContent: String,
      packageJsonName: String = PackageJsonParser.PackageJsonFilename
    )(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("jssrc2cpgTest") { dir =>
        val file = dir / "file.js"
        val json = dir / packageJsonName
        file.write(code)
        json.write(packageJsonContent)
        val cpg    = newEmptyCpg()
        val config = Config().withInputPath(dir.toString())
        new DependenciesPass(cpg, config).createAndApply()
        f(cpg)
      }
    }
  }

}
