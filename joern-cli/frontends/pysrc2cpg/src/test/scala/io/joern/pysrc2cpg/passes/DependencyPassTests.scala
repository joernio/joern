package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class DependencyPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "requirements.txt with exact pinning (==)" should {
    lazy val cpg = code(
      """Flask==1.1.2
        |requests==2.28.0
        |""".stripMargin,
      "requirements.txt"
    )

    "create dependency nodes with name and version" in {
      val deps = cpg.dependency.l.sortBy(_.name)
      deps.size shouldBe 2
      deps.head.name shouldBe "Flask"
      deps.head.version shouldBe "1.1.2"
      deps.last.name shouldBe "requests"
      deps.last.version shouldBe "2.28.0"
    }
  }

  "requirements.txt with flexible specifiers" should {
    lazy val cpg = code(
      """flask>=2.0.0
        |requests~=2.28
        |numpy<=1.24.0
        |pandas!=1.5.0
        |scipy>1.9
        |matplotlib<3.8
        |bare-package
        |# this is a comment
        |-r other-requirements.txt
        |--index-url https://pypi.org/simple
        |package-with-extras[security]>=1.0
        |conditional-pkg>=1.0; python_version >= "3.8"
        |""".stripMargin,
      "requirements.txt"
    )

    "create dependency nodes for all specifier styles" in {
      val deps    = cpg.dependency.l
      val depMap  = deps.map(d => d.name -> d.version).toMap
      depMap("flask") shouldBe "2.0.0"
      depMap("requests") shouldBe "2.28"
      depMap("numpy") shouldBe "1.24.0"
      depMap("pandas") shouldBe "1.5.0"
      depMap("scipy") shouldBe "1.9"
      depMap("matplotlib") shouldBe "3.8"
      depMap("bare-package") shouldBe ""
      depMap("package-with-extras") shouldBe "1.0"
      depMap("conditional-pkg") shouldBe "1.0"
    }

    "skip comments, includes, and option flags" in {
      val depNames = cpg.dependency.name.l.toSet
      depNames should not contain "comment"
      depNames should not contain "other-requirements.txt"
      depNames should not contain "index-url"
    }
  }

  "pyproject.toml with PEP 621 dependencies" should {
    lazy val cpg = code(
      """[project]
        |name = "my-project"
        |dependencies = [
        |    "flask>=2.0",
        |    "requests~=2.28",
        |    "click",
        |]
        |
        |[tool.other]
        |something = "else"
        |""".stripMargin,
      "pyproject.toml"
    )

    "create dependency nodes from PEP 621 format" in {
      val deps   = cpg.dependency.l
      val depMap = deps.map(d => d.name -> d.version).toMap
      depMap("flask") shouldBe "2.0"
      depMap("requests") shouldBe "2.28"
      depMap("click") shouldBe ""
    }
  }

  "pyproject.toml with Poetry dependencies" should {
    lazy val cpg = code(
      """[tool.poetry.dependencies]
        |python = "^3.8"
        |flask = "^2.0"
        |requests = {version = "^2.28", optional = true}
        |
        |[tool.poetry.dev-dependencies]
        |pytest = "^7.0"
        |""".stripMargin,
      "pyproject.toml"
    )

    "create dependency nodes from Poetry format" in {
      val deps     = cpg.dependency.l
      val depNames = deps.map(_.name).toSet
      depNames should contain("flask")
      depNames should contain("requests")
      depNames should not contain "python"

      val depMap = deps.map(d => d.name -> d.version).toMap
      depMap("flask") shouldBe "^2.0"
      depMap("requests") shouldBe "^2.28"
    }
  }

  "setup.cfg with install_requires" should {
    lazy val cpg = code(
      """[metadata]
        |name = my-project
        |
        |[options]
        |install_requires =
        |    flask>=2.0
        |    requests~=2.28
        |    click
        |
        |[options.extras_require]
        |dev = pytest
        |""".stripMargin,
      "setup.cfg"
    )

    "create dependency nodes from setup.cfg format" in {
      val deps   = cpg.dependency.l
      val depMap = deps.map(d => d.name -> d.version).toMap
      depMap("flask") shouldBe "2.0"
      depMap("requests") shouldBe "2.28"
      depMap("click") shouldBe ""
    }
  }

}
