package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class HashTests extends RubyCode2CpgFixture {

  "`{}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
                     |{}
                     |""".stripMargin)

    val List(hashCall) = cpg.call.l

    hashCall.methodFullName shouldBe RubyOperators.hashInitializer
    hashCall.code shouldBe "{}"
    hashCall.lineNumber shouldBe Some(2)
  }

  "`{x:1}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
                     |{x:1}
                     |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l
    hashCall.code shouldBe "{x:1}"
    hashCall.lineNumber shouldBe Some(2)

    val List(assocCall) = hashCall.argument.isCall.name(RubyOperators.association).l
    val List(x, one)    = assocCall.argument.l
    x.code shouldBe "x"
    one.code shouldBe "1"
  }

  "Inclusive Range of primitive ordinal type should expand in hash key" in {
    val cpg = code("""
        |{1..3:"abc"}
        |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializer :: Nil =>
        inside(hashInitializer.argument.l) {
          case firstCall :: secondCall :: thirdCall :: Nil =>
            firstCall.code shouldBe "1: \"abc\""
            secondCall.code shouldBe "2: \"abc\""
            thirdCall.code shouldBe "3: \"abc\""
          case _ => fail("Expected 3 calls (one per item in range)")
        }
      case _ => fail("Expected one hash initializer function")
    }

  }

}
