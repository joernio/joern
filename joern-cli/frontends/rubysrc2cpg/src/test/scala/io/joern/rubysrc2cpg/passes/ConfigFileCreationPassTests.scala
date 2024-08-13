package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class ConfigFileCreationPassTests extends RubyCode2CpgFixture {

  "yaml files should be included" in {
    val cpg = code(
      """
        |foo:
        |  bar
        |""".stripMargin,
      "config.yaml"
    )

    val config = cpg.configFile.name("config.yaml").head
    config.content should include("foo:")
  }

  "yml files should be included" in {
    val cpg = code(
      """
        |foo:
        |  bar
        |""".stripMargin,
      "config.yml"
    )

    val config = cpg.configFile.name("config.yml").head
    config.content should include("foo:")
  }

  "xml files should be included" in {
    val cpg = code(
      """
        |<foo>
        |  <p>bar<p>
        |</foo>
        |""".stripMargin,
      "config.xml"
    )

    val config = cpg.configFile.name("config.xml").head
    config.content should include("<p>bar<p>")
  }

  "erb files should be included" in {
    val cpg = code(
      """
        |<%= 1 + 2 %>
        |""".stripMargin,
      "foo.erb"
    )

    val config = cpg.configFile.name("foo.erb").head
    config.content should include("1 + 2")
  }

}
