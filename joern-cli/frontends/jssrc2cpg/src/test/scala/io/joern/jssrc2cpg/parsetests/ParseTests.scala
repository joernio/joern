package io.joern.jssrc2cpg.parsetests

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.semanticcpg.language._

class ParseTests extends JsSrc2CpgSuite {

  override val code: String =
    """
      |console.log("Hello World!")
      |""".stripMargin

  "should contain the parsed file" in {
    cpg.file.name.l.exists(_.endsWith(".js")) shouldBe true
  }
}
