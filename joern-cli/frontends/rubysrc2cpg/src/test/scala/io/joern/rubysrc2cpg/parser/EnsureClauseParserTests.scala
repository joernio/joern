package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class EnsureClauseParserTests extends RubyParserFixture with Matchers {
  "ensure statement" in {
    test("""def refund
        | ensure
        |   redirect_to paddle_charge_path(@charge)
        |end
        |""".stripMargin)
  }
}
