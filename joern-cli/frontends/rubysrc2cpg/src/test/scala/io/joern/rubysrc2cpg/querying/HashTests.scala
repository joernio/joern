package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.passes.Defines.RubyOperators
import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.joern.rubysrc2cpg.passes.GlobalTypes.kernelPrefix
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier, Literal, TypeRef}
import io.shiftleft.semanticcpg.language.*

class HashTests extends RubyCode2CpgFixture {
  "`{}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
                     |{}
                     |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l

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

    val List(assocCall) = hashCall.inCall.astSiblings.assignment.l
    val List(x, one)    = assocCall.argument.l
    x.code shouldBe "<tmp-0>[x]"
    one.code shouldBe "1"
  }

  "`{:x => 1}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
        |{:x => 1}
        |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l
    hashCall.code shouldBe "{:x => 1}"
    hashCall.lineNumber shouldBe Some(2)

    val List(assocCall) = hashCall.inCall.astSiblings.assignment.l
    val List(x, one)    = assocCall.argument.l
    x.code shouldBe "<tmp-0>[:x]"
    one.code shouldBe "1"
  }

  "`{:x => /(eu|us)/}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
        |{:x => /(eu|us)/}
        |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l
    hashCall.code shouldBe "{:x => /(eu|us)/}"
    hashCall.lineNumber shouldBe Some(2)

    val List(assocCall) = hashCall.inCall.astSiblings.assignment.l
    val List(x, regexp) = assocCall.argument.l
    x.code shouldBe "<tmp-0>[:x]"
    regexp.code shouldBe "/(eu|us)/"
  }

  "`{:x : /(eu|us)/}` is represented by a `hashInitializer` operator call" in {
    val cpg = code("""
        |{:x : /(eu|us)/}
        |""".stripMargin)

    val List(hashCall) = cpg.call.name(RubyOperators.hashInitializer).l
    hashCall.code shouldBe "{:x : /(eu|us)/}"
    hashCall.lineNumber shouldBe Some(2)

    val List(assocCall) = hashCall.inCall.astSiblings.assignment.l
    val List(x, regexp) = assocCall.argument.l
    x.code shouldBe "<tmp-0>[:x]"
    regexp.code shouldBe "/(eu|us)/"
  }

  "Inclusive Range of primitive ordinal type should expand in hash key" in {
    val cpg = code("""
        |{1..3:"abc", 4..5:"ade"}
        |{'a'..'c': "abc"}
        |""".stripMargin)

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializerInt :: hashInitializerString :: Nil =>
        inside(hashInitializerInt.inCall.astSiblings.assignment.l) {
          case firstCall :: secondCall :: thirdCall :: fourthCall :: fifthCall :: Nil =>
            firstCall.code shouldBe "<tmp-0>[1] = \"abc\""
            secondCall.code shouldBe "<tmp-0>[2] = \"abc\""
            thirdCall.code shouldBe "<tmp-0>[3] = \"abc\""
            fourthCall.code shouldBe "<tmp-0>[4] = \"ade\""
            fifthCall.code shouldBe "<tmp-0>[5] = \"ade\""

            inside(firstCall.argument.l) {
              case (lhs: Call) :: (rhs: Literal) :: Nil =>
                lhs.code shouldBe "<tmp-0>[1]"
                lhs.name shouldBe Operators.indexAccess

                rhs.code shouldBe "\"abc\""
                rhs.typeFullName shouldBe s"$kernelPrefix.String"
              case _ => fail("Expected LHS and RHS after lowering")
            }

            inside(fourthCall.argument.l) { case (lhs: Call) :: (rhs: Literal) :: Nil =>
              lhs.code shouldBe "<tmp-0>[4]"
              lhs.name shouldBe Operators.indexAccess

              rhs.code shouldBe "\"ade\""
              rhs.typeFullName shouldBe s"$kernelPrefix.String"
            }
          case _ => fail("Expected 5 calls (one per item in range)")
        }

        inside(hashInitializerString.inCall.astSiblings.assignment.l) {
          case firstCall :: secondCall :: thirdCall :: Nil =>
            firstCall.code shouldBe "<tmp-1>['a'] = \"abc\""
            secondCall.code shouldBe "<tmp-1>['b'] = \"abc\""
            thirdCall.code shouldBe "<tmp-1>['c'] = \"abc\""

            inside(firstCall.argument.l) {
              case (lhs: Call) :: (rhs: Literal) :: Nil =>
                lhs.code shouldBe "<tmp-1>['a']"
                lhs.name shouldBe Operators.indexAccess

                rhs.code shouldBe "\"abc\""
                rhs.typeFullName shouldBe s"$kernelPrefix.String"
              case _ => fail("Expected LHS and RHS after lowering")
            }
          case _ => fail("Expected 3 calls (one per item in range)")
        }
      case _ => fail("Expected one hash initializer function")
    }

  }

  "Exclusive Range of primitive ordinal type should expand in hash key" in {
    val cpg = code("""
                     |{1...3:"abc"}
                     |""".stripMargin)

    inside(cpg.call.name(RubyOperators.hashInitializer).l) {
      case hashInitializer :: Nil =>
        inside(hashInitializer.inCall.astSiblings.l) {
          case (firstCall: Call) :: (secondCall: Call) :: (tmp: Identifier) :: Nil =>
            firstCall.code shouldBe "<tmp-0>[1] = \"abc\""
            secondCall.code shouldBe "<tmp-0>[2] = \"abc\""
            tmp.name shouldBe "<tmp-0>"
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
        inside(hashInitializer.inCall.astSiblings.assignment.l) {
          case rangeExprArg :: Nil =>
            inside(rangeExprArg.argument.l) {
              case (lhs: Call) :: (rhs: Literal) :: Nil =>
                lhs.name shouldBe Operators.indexAccess
                lhs.code shouldBe "<tmp-0>[:a...:b]"

                inside(lhs.argument.isCall.l) {
                  case rangeExp :: Nil =>
                    rangeExp.name shouldBe Operators.range
                  case _ => fail("Expected range operator for non-primitive range key")
                }

                rhs.typeFullName shouldBe s"$kernelPrefix.String"
                rhs.code shouldBe "\"a\""
              case _ => fail("Expected LHS and RHS for association")
            }

          case _ => fail("Expected one argument for range expression")
        }
      case _ => fail("Expected one hash initializer function")
    }
  }

  "an implicit hash constructor (Hash::[]) should be lowered to a hash initializer" in {
    val cpg = code("""
        |x = Hash [1 => "a", 2 => "b", 3 => "c"]
        |""".stripMargin)

    inside(cpg.call.nameExact("[]").l) {
      case hashCall :: Nil =>
        hashCall.code shouldBe "Hash [1 => \"a\", 2 => \"b\", 3 => \"c\"]"
        hashCall.lineNumber shouldBe Some(2)
        hashCall.methodFullName shouldBe s"$kernelPrefix.Hash:[]"
        hashCall.typeFullName shouldBe s"$kernelPrefix.Hash"

        inside(hashCall.astChildren.l) {
          case (_: Call) :: (_: TypeRef) :: (one: Call) :: (two: Call) :: (three: Call) :: Nil =>
            one.code shouldBe "1 => \"a\""
            two.code shouldBe "2 => \"b\""
            three.code shouldBe "3 => \"c\""
          case xs => fail(s"Expected 3 literals under the hash initializer, instead got [${xs.code.mkString(", ")}]")
        }
      case xs => fail(s"Expected a single array initializer call, instead got [${xs.code.mkString(", ")}]")
    }
  }

}
