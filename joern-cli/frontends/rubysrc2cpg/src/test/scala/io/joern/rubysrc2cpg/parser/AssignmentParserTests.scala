package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class AssignmentParserTests extends RubyParserFixture with Matchers {
  "Single assignment" in {
    test("x=1")
  }

  "Multiple assignment" in {
    test("p, q = [foo(), bar()]")
  }
}
