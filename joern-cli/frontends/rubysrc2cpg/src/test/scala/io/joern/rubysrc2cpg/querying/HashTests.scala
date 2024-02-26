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
        |{1..3:"abc", 4..5:"ade"}
        |{'a'..'c': "abc"}
        |""".stripMargin)

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializerInt :: hashInitializerString :: Nil =>
        inside(hashInitializerInt.argument.l) {
          case firstCall :: secondCall :: thirdCall :: fourthCall :: fifthCall :: Nil =>
            firstCall.code shouldBe "<tmp-0>[1] = \"abc\""
            secondCall.code shouldBe "<tmp-0>[2] = \"abc\""
            thirdCall.code shouldBe "<tmp-0>[3] = \"abc\""
            fourthCall.code shouldBe "<tmp-0>[4] = \"ade\""
            fifthCall.code shouldBe "<tmp-0>[5] = \"ade\""
          case _ => fail("Expected 5 calls (one per item in range)")
        }

        inside(hashInitializerString.argument.l) {
          case firstCall :: secondCall :: thirdCall :: Nil =>
            println(firstCall.code)
            firstCall.code shouldBe "<tmp-1>['a'] = \"abc\""
            secondCall.code shouldBe "<tmp-1>['b'] = \"abc\""
            thirdCall.code shouldBe "<tmp-1>['c'] = \"abc\""
          case _ => fail("Expected 3 calls (one per item in range)")
        }
      case _ => fail("Expected one hash initializer function")
    }

  }

  "Exclusive Range of primitive ordinal type should expand in hash key" in {
    val cpg = code("""
                     |{1...3:"abc"}
                     |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializer :: Nil =>
        inside(hashInitializer.argument.l) {
          case firstCall :: secondCall :: Nil =>
            println(firstCall.code)
            firstCall.code shouldBe "<tmp-0>[1] = \"abc\""
            secondCall.code shouldBe "<tmp-0>[2] = \"abc\""
          case _ => fail("Expected 2 calls (one per item in range)")
        }
      case _ => fail("Expected one hash initializer function")
    }
  }

  "Non-Primitive ordinal type should not expand in hash key" in {
    val cpg = code("""
        |{:a...:b:"a"}
        |""".stripMargin)

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializer :: Nil =>
        inside(hashInitializer.argument.isCall.l) {
          case rangeExprArg :: Nil =>
            inside(rangeExprArg.argument.l) {
              case lhs :: rhs :: Nil =>
                lhs.code shouldBe "<tmp-0>[:a...:b]"
                rhs.code shouldBe "\"a\""
              case _ => fail("Expected LHS and RHS for association")
            }

          case _ => fail("Expected one argument for range expression")
        }
      case _ => fail("Expected one hash initializer function")
    }
  }

}
