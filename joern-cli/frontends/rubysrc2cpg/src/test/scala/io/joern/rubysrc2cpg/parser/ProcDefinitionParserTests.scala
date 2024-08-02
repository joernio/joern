package io.joern.rubysrc2cpg.parser

import io.joern.rubysrc2cpg.testfixtures.RubyParserFixture
import org.scalatest.matchers.should.Matchers

class ProcDefinitionParserTests extends RubyParserFixture with Matchers {
  "one-line proc definition" in {
    test("-> {}")

    test(
      "-> do ; end",
      """-> do
        |end""".stripMargin
    )

    test(
      "-> do 1 end",
      """-> do
        |1
        |end""".stripMargin
    )

    test("-> (x) {}", "->(x) {}")

    test(
      "-> (x) do ; end",
      """->(x) do
        |end""".stripMargin
    )

    test("->(x = 1) {}", "->(x=1) {}")

    test(
      "-> (foo: 1) do ; end",
      """->(foo:1) do
        |end""".stripMargin
    )

    test(
      "->(x, y) {puts x; puts y}",
      """->(x,y) {
        |puts x
        |puts y
        |}""".stripMargin
    )
  }
}
