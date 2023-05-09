package io.joern.dataflowengineoss.semanticsloader

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParserTests extends AnyWordSpec with Matchers {

  class Fixture() {
    val parser = new Parser()
  }

  "Parser" should {

    "parse a lone method name with new line" in new Fixture() {
      private val semantics = parser.parse("\"foo\"\n")
      semantics.size shouldBe 1
    }

    "parse a method name followed by mappings" in new Fixture() {
      private val semantics = parser.parse("\"foo\" 1->-1 2->3\n")
      semantics match {
        case List(x) =>
          x.methodFullName shouldBe "foo"
          x.mappings shouldBe List(ParamMapping(PosArg(1), PosArg(-1)), ParamMapping(PosArg(2), PosArg(3)))
        case _ => fail()
      }
    }

    "allow multiple new lines between semantics" in new Fixture() {
      private val semantics = parser.parse("\"foo\"\n\n\n\"bar\"")
      semantics.size shouldBe 2
    }

    "parse a lone method name even without new line" in new Fixture() {
      private val semantics = parser.parse("\"foo\"")
      semantics.size shouldBe 1
    }

    "skip invalid lines and still parse valid ones" in new Fixture() {
      private val semantics = parser.parse("\"abc\"\nfoo\n\"bar\"")
      semantics match {
        case List(x, y) =>
          x.methodFullName shouldBe "abc"
          y.methodFullName shouldBe "bar"
        case _ => fail()
      }
    }

    "parse named argument parameters" in new Fixture() {
      private val semantics = parser.parse("\"foo\" \"param1\"->2 3->\"param2\"")
      semantics.size shouldBe 1
      semantics shouldBe List(
        FlowSemantic(
          "foo",
          List(ParamMapping(NamedArg("param1"), PosArg(2)), ParamMapping(PosArg(3), NamedArg("param2")))
        )
      )
    }
  }

}
