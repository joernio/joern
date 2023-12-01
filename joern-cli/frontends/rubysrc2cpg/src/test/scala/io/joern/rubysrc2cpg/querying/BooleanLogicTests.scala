package io.joern.rubysrc2cpg.querying

import io.joern.rubysrc2cpg.testfixtures.RubyCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class BooleanLogicTests extends RubyCode2CpgFixture {

  "`or` is represented by a `logicalOr` operator call" in {
    val cpg = code("""
                     |1 or 2
                     |""".stripMargin)

    val List(orCall) = cpg.call(Operators.logicalOr).l
    orCall.lineNumber shouldBe Some(2)
    orCall.code shouldBe "1 or 2"

    val List(one, two) = orCall.argument.l
    one.code shouldBe "1"
    two.code shouldBe "2"
  }

  "`||` is represented by a `logicalOr` operator call" in {
    val cpg = code("""
                     |1 || 2
                     |""".stripMargin)

    val List(orCall) = cpg.call(Operators.logicalOr).l
    orCall.lineNumber shouldBe Some(2)
    orCall.code shouldBe "1 || 2"

    val List(one, two) = orCall.argument.l
    one.code shouldBe "1"
    two.code shouldBe "2"
  }

  "`and` is represented by a `logicalAnd` operator call" in {
    val cpg = code("""
                     |1 and 2
                     |""".stripMargin)

    val List(andCall) = cpg.call(Operators.logicalAnd).l
    andCall.lineNumber shouldBe Some(2)
    andCall.code shouldBe "1 and 2"

    val List(one, two) = andCall.argument.l
    one.code shouldBe "1"
    two.code shouldBe "2"
  }

  "`&&` is represented by a `logicalAnd` operator call" in {
    val cpg = code("""
                     |1 && 2
                     |""".stripMargin)

    val List(andCall) = cpg.call(Operators.logicalAnd).l
    andCall.lineNumber shouldBe Some(2)
    andCall.code shouldBe "1 && 2"

    val List(one, two) = andCall.argument.l
    one.code shouldBe "1"
    two.code shouldBe "2"
  }

  "`not` is represented by a `logicalNot` operator call" in {
    val cpg = code("""
                     |not 1
                     |""".stripMargin)

    val List(notCall) = cpg.call(Operators.logicalNot).l
    notCall.lineNumber shouldBe Some(2)
    notCall.code shouldBe "not 1"

    val List(one) = notCall.argument.l
    one.code shouldBe "1"
  }

  "`!` is represented by a `logicalNot` operator call" in {
    val cpg = code("""
                     |!1
                     |""".stripMargin)

    val List(notCall) = cpg.call(Operators.logicalNot).l
    notCall.code shouldBe "!1"
    notCall.lineNumber shouldBe Some(2)

    val List(one) = notCall.argument.l
    one.code shouldBe "1"
  }

  "`or`-`and` are left-associative" in {
    val cpg = code("""
                     |1 or 2 or 3
                     |4 and 5 and 6
                     |""".stripMargin)

    val List(or3, or12)    = cpg.call.methodFullName(Operators.logicalOr).l
    val List(one, two)     = or12.argument.l
    val List(or12_, three) = or3.argument.l

    or12_ shouldBe or12
    one.code shouldBe "1"
    two.code shouldBe "2"
    three.code shouldBe "3"

    val List(and6, and45) = cpg.call.methodFullName(Operators.logicalAnd).l
    val List(four, five)  = and45.argument.l
    val List(and45_, six) = and6.argument.l

    and45_ shouldBe and45
    four.code shouldBe "4"
    five.code shouldBe "5"
    six.code shouldBe "6"
  }

  "`or`-`and` have the same precedence level" in {
    val cpg = code("""
                     |1 or 2 and 3
                     |4 and 5 or 6
                     |""".stripMargin)

    val List(and3)        = cpg.call(Operators.logicalAnd).lineNumber(2).l
    val List(or12_)       = cpg.call(Operators.logicalOr).lineNumber(2).l
    val List(or12, three) = and3.argument.l

    or12_ shouldBe or12
    or12.code shouldBe "1 or 2"
    three.code shouldBe "3"

    val List(or6)        = cpg.call(Operators.logicalOr).lineNumber(3).l
    val List(and45_)     = cpg.call(Operators.logicalAnd).lineNumber(3).l
    val List(and45, six) = or6.argument.l

    and45_ shouldBe and45
    and45.code shouldBe "4 and 5"
    six.code shouldBe "6"
  }

  "`not` binds tighter than `or`" in {
    val cpg = code("""
                     |1 or not 2
                     |not 3 or 4
                     |""".stripMargin)

    val List(or1not2)    = cpg.call(Operators.logicalOr).lineNumber(2).l
    val List(not2)       = cpg.call(Operators.logicalNot).lineNumber(2).l
    val List(one, not2_) = or1not2.argument.l

    not2_ shouldBe not2
    not2.code shouldBe "not 2"
    or1not2.code shouldBe "1 or not 2"
    one.code shouldBe "1"

    val List(not3or4)     = cpg.call(Operators.logicalOr).lineNumber(3).l
    val List(not3)        = cpg.call(Operators.logicalNot).lineNumber(3).l
    val List(not3_, four) = not3or4.argument.l

    not3_ shouldBe not3
    not3.code shouldBe "not 3"
    not3or4.code shouldBe "not 3 or 4"
    four.code shouldBe "4"
  }

}
