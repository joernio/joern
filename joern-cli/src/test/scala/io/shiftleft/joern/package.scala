package io.shiftleft

import io.shiftleft.cpgqueryingtests.codepropertygraph.{CpgFactory, LanguageFrontend}

package object joern {

  /**
    * Create a CPG for `code` using default semantics
    * @param code string containing C code
    * */
  def createTestCpg(code: String) = {
    val semanticFilename = "src/main/resources/default.semantics"
    new CpgFactory(LanguageFrontend.Fuzzyc, semanticFilename).buildCpg(code)
  }

}
