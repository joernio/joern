package io.joern.x2cpg

import org.scalatest.funsuite.AnyFunSuite
import org.apache.commons.lang3.StringUtils
import org.scalatest.matchers.should.Matchers

class ShortenCodeTests extends AnyFunSuite with Matchers {

  // Minimal processor to satisfy the self-type and abstract members
  trait TestProcessor
  class TestBuilder extends TestProcessor with AstNodeBuilder[Unit, TestProcessor] {
    protected def line(node: Unit): Option[Int]      = None
    protected def column(node: Unit): Option[Int]    = None
    protected def lineEnd(node: Unit): Option[Int]   = None
    protected def columnEnd(node: Unit): Option[Int] = None
    protected def code(node: Unit): String           = ""

    // Expose the protected method for testing
    def runShorten(s: String, maxCodeLength: Int = envMax): String = shortenCode(s, maxCodeLength)
  }

  private val builder = new TestBuilder

  // Do not test env access, we set the default value directly instead
  private val envMax    = 1000
  private val threshold = math.max(50, envMax)

  test("shortenCode returns input unchanged when length <= threshold") {
    val input = "a" * threshold
    val out   = builder.runShorten(input)
    out shouldBe input
  }

  test("shortenCode returns input unchanged when length < 4") {
    val input = "a" * 3
    val out   = builder.runShorten(input)
    out shouldBe input
  }

  test("shortenCode returns input unchanged when maxCodeLength < 4") {
    val input         = "a" * (threshold + 5)
    val maxCodeLength = -1
    val out           = builder.runShorten(input, maxCodeLength)
    out shouldBe input
  }

  test("shortenCode abbreviates input longer than threshold using StringUtils.abbreviate semantics") {
    val input    = "b" * (threshold + 5)
    val expected = StringUtils.abbreviate(input, threshold)
    val out      = builder.runShorten(input)
    out shouldBe expected
    out.length shouldBe threshold
  }
}
