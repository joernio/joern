package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ConfigFileTests extends JimpleCode2CpgFixture {

  "a compilation with config files" should {

    val cpg = code(
      """
        |<foo></foo>
        |""".stripMargin,
      "web.xml"
    )

    "be in the CPG" in {
      cpg.configFile.name.contains("web.xml") shouldBe true
    }

  }

}
