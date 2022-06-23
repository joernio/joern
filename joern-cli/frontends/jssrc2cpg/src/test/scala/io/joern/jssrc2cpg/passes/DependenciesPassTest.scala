package io.joern.jssrc2cpg.passes

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.PropertyNames
import better.files.File
import io.joern.jssrc2cpg.utils.PackageJsonParser
import io.joern.jssrc2cpg.Config
import io.joern.x2cpg.X2Cpg.newEmptyCpg
import overflowdb.traversal._

class DependenciesPassTest extends AbstractPassTest {

  "DependenciesPass" should {

    "ignore empty package.json" in {
      File.usingTemporaryDirectory("js2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PACKAGE_JSON_FILENAME
        json.write("")
        PackageJsonParser.isValidProjectPackageJson(json.path) shouldBe false
      }
    }

    "ignore package.json without any useful content" in {
      File.usingTemporaryDirectory("js2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PACKAGE_JSON_FILENAME
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
      File.usingTemporaryDirectory("js2cpgTest") { dir =>
        val json = dir / PackageJsonParser.PACKAGE_JSON_FILENAME
        json.write("{}")
        PackageJsonParser.isValidProjectPackageJson(json.path) shouldBe false
      }
    }

    "generate dependency nodes correctly (no dependencies at all)" in DependencyFixture("", "{}") { cpg =>
      getDependencies(cpg).size shouldBe 0
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
      getDependencies(cpg).size shouldBe 0
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
      packageJsonName = PackageJsonParser.PACKAGE_JSON_LOCK_FILENAME
    ) { cpg =>
      def deps = getDependencies(cpg)
      deps.size shouldBe 2
      deps.has(PropertyNames.NAME, "dep1").has(PropertyNames.VERSION, "0.1").size shouldBe 1
      deps.has(PropertyNames.NAME, "dep2").has(PropertyNames.VERSION, "0.2").size shouldBe 1
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
      def deps = getDependencies(cpg)
      deps.size shouldBe 1
      deps.has(PropertyNames.NAME, "dep1").has(PropertyNames.VERSION, "0.1").size shouldBe 1
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
      def deps = getDependencies(cpg)
      deps.size shouldBe 4
      deps.has(PropertyNames.NAME, "dep1").has(PropertyNames.VERSION, "0.1").size shouldBe 1
      deps.has(PropertyNames.NAME, "dep2").has(PropertyNames.VERSION, "0.2").size shouldBe 1
      deps.has(PropertyNames.NAME, "dep3").has(PropertyNames.VERSION, "0.3").size shouldBe 1
      deps.has(PropertyNames.NAME, "dep4").has(PropertyNames.VERSION, "0.4").size shouldBe 1
    }

  }

  private object DependencyFixture extends Fixture {
    def apply(
      code: String,
      packageJsonContent: String,
      packageJsonName: String = PackageJsonParser.PACKAGE_JSON_FILENAME
    )(f: Cpg => Unit): Unit = {
      File.usingTemporaryDirectory("js2cpgTest") { dir =>
        val file = dir / "file.js"
        val json = dir / packageJsonName
        file.write(code)
        json.write(packageJsonContent)
        val cpg    = newEmptyCpg()
        val config = Config(inputPath = dir.toString())
        new DependenciesPass(cpg, config).createAndApply()
        f(cpg)
      }
    }
  }

}
