package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.cpgqueryingtests.codepropertygraph.{CpgFactory, LanguageFrontend}

/**
  * Base class for creating a test CPG with default semantics.
  * @param code string containing C code
  * */
class TestCpg[T] {

  private val semanticFilename = "joern-cli/src/main/resources/default.semantics"

  def buildCpg(code: String)(test: Cpg => T): T = {
    new CpgFactory(LanguageFrontend.Fuzzyc, semanticFilename).buildCpg(code)(test)
  }

}
