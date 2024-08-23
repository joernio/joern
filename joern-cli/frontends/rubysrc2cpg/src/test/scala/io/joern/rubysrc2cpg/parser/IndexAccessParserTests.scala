package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class IndexAccessParserTests extends RubyParserFixture with Matchers {
  "Index access" in {
    test("a[1]")
    test("a[1,2]")
    test("a[2,]", "a[2]")
    test("a[2=>3,]", "a[2=> 3]")
  }
}
