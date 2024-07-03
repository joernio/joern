package io.joern.macros

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import io.joern.macros.QueryMacros.withStrRep
import io.joern.console.*
import io.shiftleft.semanticcpg.language.*

class QueryMacroTests extends AnyWordSpec with Matchers {
  "Query macros" should {
    "have correct string representation" in {
      withStrRep(cpg => cpg.method).strRep shouldBe "cpg => cpg.method"
    }
  }

}
