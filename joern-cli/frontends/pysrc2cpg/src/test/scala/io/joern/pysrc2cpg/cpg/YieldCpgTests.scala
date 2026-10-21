package io.joern.pysrc2cpg.cpg

import io.joern.pysrc2cpg.testfixtures.PySrc2CpgFixture
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers

class YieldCpgTests extends PySrc2CpgFixture with Matchers {
  "bare yield" should {
    val cpg = code("""def gen():
        |  yield
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.head
      returnNode.code shouldBe "yield"
      returnNode.lineNumber shouldBe Some(2)
    }

    "have no ast children as arguments" in {
      cpg.ret.astChildren.isIdentifier.l shouldBe empty
    }
  }

  "yield with value" should {
    val cpg = code("""def gen():
        |  yield x
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.head
      returnNode.code shouldBe "yield x"
      returnNode.lineNumber shouldBe Some(2)
    }

    "test return node ast children" in {
      cpg.ret.astChildren.order(1).isIdentifier.head.code shouldBe "x"
    }
  }

  "yield in a loop" should {
    val cpg = code("""def gen(items):
        |  for x in items:
        |    yield x
        |""".stripMargin)

    "have a return node with yield code" in {
      val returnNode = cpg.ret.code("yield.*").head
      returnNode.code shouldBe "yield x"
    }
  }

  "yield from" should {
    val cpg = code("""def gen():
        |  yield from other_gen()
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.code("yield from.*").head
      returnNode.code shouldBe "yield from other_gen()"
      returnNode.lineNumber shouldBe Some(2)
    }

    "test return node ast children" in {
      cpg.ret.code("yield from.*").astChildren.order(1).isCall.head.code shouldBe "other_gen()"
    }
  }

  "yield from with identifier" should {
    val cpg = code("""def gen(items):
        |  yield from items
        |""".stripMargin)

    "test return node properties" in {
      val returnNode = cpg.ret.code("yield from.*").head
      returnNode.code shouldBe "yield from items"
      returnNode.lineNumber shouldBe Some(2)
    }

    "test return node ast children" in {
      cpg.ret.code("yield from.*").astChildren.order(1).isIdentifier.head.code shouldBe "items"
    }
  }
}
