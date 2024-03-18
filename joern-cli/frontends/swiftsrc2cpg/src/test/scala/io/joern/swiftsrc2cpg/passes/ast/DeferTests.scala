package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class DeferTests extends AstSwiftSrc2CpgSuite {

  "DeferTests" should {

    "testDefer1" in {
      val cpg = code("""
        |if score < 10 {
        |  // Prints "The score is:"
        |  // Prints "6"
        |  // i.e., evaluates defer statements in reverse order
        |  defer {
        |    print(score)
        |  }
        |  defer {
        |    print("The score is:")
        |  }
        |  score += 5
        |}
        |""".stripMargin)
      val List(ifBlock)                           = cpg.ifBlock.l
      val List(scoreCall, defer2Call, defer1Call) = ifBlock.whenTrue.astChildren.isCall.l
      scoreCall.code shouldBe "score += 5"
      scoreCall.order shouldBe 1
      defer2Call.code shouldBe "print(\"The score is:\")"
      defer2Call.order shouldBe 2
      defer1Call.code shouldBe "print(score)"
      defer1Call.order shouldBe 3
    }

    "testDefer2" in {
      val cpg = code("""
        |if foo() {
        |  defer {
        |    print(a1)
        |    print(a2)
        |  }
        |  defer {
        |    print(b1)
        |    print(b2)
        |  }
        |  bar()
        |}
        |""".stripMargin)
      val List(ifBlock) = cpg.ifBlock.l
      val List(barCall) = ifBlock.whenTrue.astChildren.isCall.l
      barCall.code shouldBe "bar()"
      barCall.order shouldBe 1
      val List(deferBlock2, deferBlock1) = ifBlock.whenTrue.astChildren.isBlock.l
      deferBlock2.astChildren.code.l shouldBe List("print(b1)", "print(b2)")
      deferBlock2.order shouldBe 2
      deferBlock1.astChildren.code.l shouldBe List("print(a1)", "print(a2)")
      deferBlock1.order shouldBe 3
    }

  }

}
