package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DependencyPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "requirements.txt with exact pinning" should {
    lazy val cpg = code("Flask==1.1.2\nrequests==2.28.0\n", "requirements.txt")

    "create dependency nodes with correct names and versions" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 2

      deps.head.name shouldBe "Flask"
      deps.head.version shouldBe "1.1.2"

      deps.last.name shouldBe "requests"
      deps.last.version shouldBe "2.28.0"
    }
  }

  "requirements.txt with various version specifiers" should {
    lazy val cpg = code(
      """Flask>=2.0.0
        |numpy~=1.21
        |pandas!=1.3.0
        |scipy<=1.8.0
        |click>7.0
        |Jinja2<3.0
        |""".stripMargin,
      "requirements.txt"
    )

    "create dependency nodes for all version specifiers" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 6

      deps.map(_.name) should contain theSameElementsAs List("Flask", "numpy", "pandas", "scipy", "click", "Jinja2")
      deps.find(_.name == "Flask").get.version shouldBe "2.0.0"
      deps.find(_.name == "numpy").get.version shouldBe "1.21"
      deps.find(_.name == "pandas").get.version shouldBe "1.3.0"
      deps.find(_.name == "scipy").get.version shouldBe "1.8.0"
      deps.find(_.name == "click").get.version shouldBe "7.0"
      deps.find(_.name == "Jinja2").get.version shouldBe "3.0"
    }
  }

  "requirements.txt with extras and markers" should {
    lazy val cpg = code(
      """requests[security]>=2.20.0
        |celery[redis]>=5.0; python_version >= "3.8"
        |# This is a comment
        |-r other-requirements.txt
        |bare-package
        |""".stripMargin,
      "requirements.txt"
    )

    "parse packages with extras, skip comments and includes" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 3

      deps.find(_.name == "requests").get.version shouldBe "2.20.0"
      deps.find(_.name == "celery").get.version shouldBe "5.0"
      deps.find(_.name == "bare-package").get.version shouldBe ""
    }
  }

  "pyproject.toml with PEP 621 dependencies" should {
    lazy val cpg = code(
      """[project]
        |name = "my-project"
        |version = "1.0.0"
        |dependencies = [
        |    "Flask>=2.0",
        |    "requests==2.28.0",
        |    "numpy",
        |]
        |
        |[project.optional-dependencies]
        |dev = ["pytest>=7.0"]
        |""".stripMargin,
      "pyproject.toml"
    )

    "create dependency nodes from [project.dependencies]" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 3

      deps.find(_.name == "Flask").get.version shouldBe "2.0"
      deps.find(_.name == "requests").get.version shouldBe "2.28.0"
      deps.find(_.name == "numpy").get.version shouldBe ""
    }
  }

  "pyproject.toml with Poetry dependencies" should {
    lazy val cpg = code(
      """[tool.poetry]
        |name = "my-project"
        |version = "1.0.0"
        |
        |[tool.poetry.dependencies]
        |python = "^3.8"
        |Flask = ">=2.0"
        |requests = "2.28.0"
        |numpy = {version = ">=1.20", optional = true}
        |
        |[tool.poetry.dev-dependencies]
        |pytest = "^7.0"
        |""".stripMargin,
      "pyproject.toml"
    )

    "create dependency nodes from [tool.poetry.dependencies], skipping python" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 3

      deps.find(_.name == "Flask").get.version shouldBe "2.0"
      deps.find(_.name == "requests").get.version shouldBe "2.28.0"
      deps.find(_.name == "numpy").get.version shouldBe "1.20"
    }
  }

  "setup.cfg with install_requires" should {
    lazy val cpg = code(
      """[metadata]
        |name = my-project
        |version = 1.0.0
        |
        |[options]
        |install_requires =
        |    Flask>=2.0
        |    requests==2.28.0
        |    numpy
        |
        |[options.extras_require]
        |dev = pytest>=7.0
        |""".stripMargin,
      "setup.cfg"
    )

    "create dependency nodes from [options] install_requires" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 3

      deps.find(_.name == "Flask").get.version shouldBe "2.0"
      deps.find(_.name == "requests").get.version shouldBe "2.28.0"
      deps.find(_.name == "numpy").get.version shouldBe ""
    }
  }
}
