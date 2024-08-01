package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ControlStructureParserTests extends RubyParserFixture with Matchers {
  "while" in {
    test(
      """while x > 0 do
        |end
        |""".stripMargin,
      """while x > 0
        |end""".stripMargin
    )
  }

  "if" in {
    test(
      """if __LINE__ > 1 then
        |end
        |""".stripMargin,
      """if __LINE__ > 1
        |end""".stripMargin
    )

    test(
      """if __LINE__ > 1 then
        |else
        |end
        |""".stripMargin,
      """if __LINE__ > 1
        |else
        |end""".stripMargin
    )

    test(
      """if __LINE__ > 1 then
        |elsif __LINE__ > 0 then
        |end
        |""".stripMargin,
      """if __LINE__ > 1
        |elsif __LINE__ > 0
        |end""".stripMargin
    )

    test(
      "a = if (y > 3) then 123 elsif(y < 6) then 2003 elsif(y < 10) then 982 else 456 end",
      """a = if y > 3
      |123
      |elsif y < 6
      |2003
      |elsif y < 10
      |982
      |else
      |456
      |end""".stripMargin
    )
  }

  "for loops" in {
    test(
      """for i in 1..10 do
        |end
        |""".stripMargin,
      """for i in 1..10
        |end""".stripMargin
    )

    test(
      """for i in 1..x do
        |end
        |""".stripMargin,
      """for i in 1..x
        |end""".stripMargin
    )
  }
}
