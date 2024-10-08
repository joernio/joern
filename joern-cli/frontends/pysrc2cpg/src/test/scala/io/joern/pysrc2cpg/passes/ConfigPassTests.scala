package io.joern.pysrc2cpg.passes

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ConfigPassTests extends PySrc2CpgFixture(withOssDataflow = false) {

  "The CPG for a `requirements.txt` file" should {
    lazy val cpg = code("Flask==1.1.2", "requirements.txt")
    "contain a CONFIG_FILE node" in {
      val List(c) = cpg.configFile.l
      c.content shouldBe "Flask==1.1.2"
      c.name shouldBe "requirements.txt"
    }
  }

  "Pipfile should be included" in {
    val cpg = code(
      """
        |[[source]]
        |url = "https://pypi.org/simple"
        |verify_ssl = true
        |name = "pypi"
        |""".stripMargin,
      "Pipfile"
    )

    val config = cpg.configFile.name("Pipfile").head
    config.content should include("verify_ssl = true")
  }

  "Pipfile.lock should be included" in {
    val cpg = code(
      """
        |{
        |     "_meta": {
        |        "hash": {
        |            "sha256": "293ad83ead15eb7bfef8a768f1853fc4cfa31b32ab85ae6962a2630b57cf569b"
        |        },
        |        "pipfile-spec": 6,
        |        "requires": {
        |            "python_full_version": "3.8.18",
        |            "python_version": "3.8"
        |        },
        |        "sources": [
        |            {
        |                "name": "pypi",
        |                "url": "https://pypi.org/simple",
        |                "verify_ssl": true
        |            }
        |        ]
        |    }
        |}
        |""".stripMargin,
      "Pipfile.lock"
    )

    val config = cpg.configFile.name("Pipfile.lock").head
    config.content should include("\"name\": \"pypi\"")
  }

}
