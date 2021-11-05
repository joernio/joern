package io.shiftleft.dataflowengineoss.semanticsloader

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
          x.mappings shouldBe List((1, -1), (2, 3))
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
      private val semantics: Seq[FlowSemantic] = parser.parse("\"abc\"\nfoo\n\"bar\"")
      semantics match {
        case List(x, y) =>
          x.methodFullName shouldBe "abc"
          y.methodFullName shouldBe "bar"
        case _ => fail()
      }
    }
  }

}
