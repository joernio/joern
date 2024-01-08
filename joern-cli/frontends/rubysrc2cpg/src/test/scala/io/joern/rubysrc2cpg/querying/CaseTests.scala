package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class CaseTests extends RubyCode2CpgFixture {
  "`case x ... end` should be represented with" in {
    val cpg = code("""
      |case 0
      |  when 0 
      |    0
      |  when 1 then 1
      |end
      |""".stripMargin)

    cpg.method(":program").dotAst.foreach(println)
  }

  "`case ... end` without expression" in {
    val cpg = code("""
      |case
      |  when true then 1
      |end
      |""".stripMargin)

  }

  "case with else" in {
    val cpg = code("""
      |case 0
      |  when 1 then 1
      |  else 0
      |end
      |""".stripMargin)

  }
}
