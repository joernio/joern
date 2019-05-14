package io.shiftleft.joern

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.cpgqueryingtests.codepropertygraph.{CpgFactory, LanguageFrontend}

/**
  * Base class for creating a test CPG with default semantics.
  * @param code string containing C code
  * */
class TestCpg(code: String) {
  private val semanticFilename = "joern-cli/src/main/resources/default.semantics"
  val cpg: Cpg = new CpgFactory(LanguageFrontend.Fuzzyc, semanticFilename).buildCpg(code)
}
