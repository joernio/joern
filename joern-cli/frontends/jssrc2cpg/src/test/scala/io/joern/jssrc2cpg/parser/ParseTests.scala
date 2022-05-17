package io.joern.jssrc2cpg.parser

import io.joern.jssrc2cpg.testfixtures.JsSrc2CpgSuite
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._

class ParseTests extends JsSrc2CpgSuite {

  private val cpg: Cpg = code("""
    |console.log("Hello World!");
    |console.log("Hello Again!!!");
    |""".stripMargin)

  "should contain the parsed file" in {
    cpg.file.name.l.exists(_.endsWith(".js")) shouldBe true
  }

}
