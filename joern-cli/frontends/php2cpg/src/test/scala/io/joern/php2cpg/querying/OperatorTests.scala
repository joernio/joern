package io.joern.php2cpg.querying

import io.joern.php2cpg.parser.Domain.PhpOperators
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._

class OperatorTests extends PhpCode2CpgFixture {

  val filenameKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  "assignment operators" should {
    "have the correct arguments set" in {
      val cpg = code("""<?php
                      |$a = 2
                      |""".stripMargin)

      val assignment = cpg.call.l match {
        case List(call) if call.name == Operators.assignment => call
        case result => fail(s"Expected assign call but found $result")
      }

      assignment.argument.l match {
        case List(target: Identifier, source: Literal) =>
          target.name shouldBe "a"
          target.code shouldBe "a"
          target.argumentIndex shouldBe 1

          source.code shouldBe "2"
          source.argumentIndex shouldBe 2

        case result => s"Found unexpected call args $result"
      }
    }

    "have the correct method names set" in {
      val testData = List(
        ("$a = $b", Operators.assignment),
        ("$a = &$b", Operators.assignment),
        ("$a &= $b", Operators.assignmentAnd),
        ("$a |= $b", Operators.assignmentOr),
        ("$a ^= $b", Operators.assignmentXor),
        ("$a ??= $b", PhpOperators.assignmentCoalesce),
        ("$a .= $b", PhpOperators.assignmentConcat),
        ("$a /= $b", Operators.assignmentDivision),
        ("$a -= $b", Operators.assignmentMinus),
        ("$a %= $b", Operators.assignmentModulo),
        ("$a *= $b", Operators.assignmentMultiplication),
        ("$a += $b", Operators.assignmentPlus),
        ("$a **= $b", Operators.assignmentExponentiation),
        ("$a <<= $b", Operators.assignmentShiftLeft),
        ("$a >>= $b", Operators.assignmentArithmeticShiftRight)
      )

      testData.foreach { case (testCode, expectedType) =>
        val cpg = code(s"<?php\n$testCode", fileName = s"Test${filenameKeyPool.next}.php")
        cpg.call.name.l shouldBe expectedType :: Nil
        cpg.call.methodFullName.l shouldBe expectedType :: Nil
      }
    }
  }

  "binary operators" should {
    "have the correct arguments set" in {
      val cpg = code("""<?php
                      |$a + 2
                      |""".stripMargin)

      val addition = cpg.call.l match {
        case List(call) if call.name == Operators.plus => call
        case result => fail(s"Expected assign call but found $result")
      }

      addition.argument.l match {
        case List(target: Identifier, source: Literal) =>
          target.name shouldBe "a"
          target.code shouldBe "a"
          target.argumentIndex shouldBe 1

          source.code shouldBe "2"
          source.argumentIndex shouldBe 2

        case result => s"Found unexpected call args $result"
      }
    }

    "have the correct method names set" in {
      val testData = List(
        ("1 & 2", Operators.and),
        ("1 | 2", Operators.or),
        ("1 ^ 2", Operators.xor),
        ("$a && $b", Operators.logicalAnd),
        ("$a || $b", Operators.logicalOr),
        ("$a ?? $b", PhpOperators.coalesce),
        ("$a . $b", PhpOperators.concat),
        ("$a / $b", Operators.division),
        ("$a == $b", Operators.equals),
        ("$a >= $b", Operators.greaterEqualsThan),
        ("$a > $b", Operators.greaterThan),
        ("$a === $b", PhpOperators.identical),
        ("$a and $b", Operators.logicalAnd),
        ("$a or $b", Operators.logicalOr),
        ("$a xor $b", PhpOperators.logicalXor),
        ("$a - $b", Operators.minus),
        ("$a % $b", Operators.modulo),
        ("$a * $b", Operators.multiplication),
        ("$a != $b", Operators.notEquals),
        ("$a <> $b", Operators.notEquals),
        ("$a !== $b", PhpOperators.notIdentical),
        ("$a + $b", Operators.plus),
        ("$a ** $b", Operators.exponentiation),
        ("$a << $b", Operators.shiftLeft),
        ("$a >> $b", Operators.arithmeticShiftRight),
        ("$a <= $b", Operators.lessEqualsThan),
        ("$a < $b", Operators.lessThan),
        ("$a <=> $b", PhpOperators.spaceship)
      )

      testData.foreach { case (testCode, expectedType) =>
        val cpg = code(s"<?php\n$testCode", fileName = s"Test${filenameKeyPool.next}.php")
        cpg.call.name.l shouldBe expectedType :: Nil
        cpg.call.methodFullName.l shouldBe expectedType :: Nil
      }
    }
  }
}
