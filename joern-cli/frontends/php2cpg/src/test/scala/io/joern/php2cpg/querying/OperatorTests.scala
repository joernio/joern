package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.TypeConstants
import io.joern.php2cpg.parser.Domain.{PhpBuiltins, PhpDomainTypeConstants}
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Literal, TypeRef}
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
        case result                                          => fail(s"Expected assign call but found $result")
      }

      assignment.argument.l match {
        case List(target: Identifier, source: Literal) =>
          target.name shouldBe "a"
          target.code shouldBe "$a"
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
        ("$a ??= $b", PhpBuiltins.assignmentCoalesceOp),
        ("$a .= $b", PhpBuiltins.assignmentConcatOp),
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

  "unary operators" should {
    "have the correct arguments set" in {
      val cpg = code("""<?php
                      |+$a
                      |""".stripMargin)

      val addition = cpg.call.l match {
        case List(call) if call.name == Operators.plus => call
        case result                                    => fail(s"Expected plus call but found $result")
      }

      addition.argument.l match {
        case List(expr: Identifier) =>
          expr.name shouldBe "a"
          expr.code shouldBe "$a"
          expr.argumentIndex shouldBe 1

        case result => s"Found unexpected call args $result"
      }
    }

    "have the correct method names set" in {
      val testData = List(
        ("~$a", Operators.not),
        ("!$a", Operators.logicalNot),
        ("$a--", Operators.postDecrement),
        ("$a++", Operators.postIncrement),
        ("--$a", Operators.preDecrement),
        ("++$a", Operators.preIncrement),
        ("-$a", Operators.minus),
        ("+$a", Operators.plus)
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
        case result                                    => fail(s"Expected plus call but found $result")
      }

      addition.argument.l match {
        case List(target: Identifier, source: Literal) =>
          target.name shouldBe "a"
          target.code shouldBe "$a"
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
        ("$a ?? $b", PhpBuiltins.coalesceOp),
        ("$a . $b", PhpBuiltins.concatOp),
        ("$a / $b", Operators.division),
        ("$a == $b", Operators.equals),
        ("$a >= $b", Operators.greaterEqualsThan),
        ("$a > $b", Operators.greaterThan),
        ("$a === $b", PhpBuiltins.identicalOp),
        ("$a and $b", Operators.logicalAnd),
        ("$a or $b", Operators.logicalOr),
        ("$a xor $b", PhpBuiltins.logicalXorOp),
        ("$a - $b", Operators.minus),
        ("$a % $b", Operators.modulo),
        ("$a * $b", Operators.multiplication),
        ("$a != $b", Operators.notEquals),
        ("$a <> $b", Operators.notEquals),
        ("$a !== $b", PhpBuiltins.notIdenticalOp),
        ("$a + $b", Operators.plus),
        ("$a ** $b", Operators.exponentiation),
        ("$a << $b", Operators.shiftLeft),
        ("$a >> $b", Operators.arithmeticShiftRight),
        ("$a <= $b", Operators.lessEqualsThan),
        ("$a < $b", Operators.lessThan),
        ("$a <=> $b", PhpBuiltins.spaceshipOp)
      )

      testData.foreach { case (testCode, expectedType) =>
        val cpg = code(s"<?php\n$testCode", fileName = s"Test${filenameKeyPool.next}.php")
        cpg.call.name.l shouldBe expectedType :: Nil
        cpg.call.methodFullName.l shouldBe expectedType :: Nil
      }
    }
  }

  "cast operations" should {
    "have the correct arguments set" in {
      val cpg = code("<?php\n(int) $a")

      val cast = cpg.call.nameExact(Operators.cast).l match {
        case List(cast) => cast
        case result     => fail(s"Expected cast call but got $result")
      }

      cast.typeFullName shouldBe "int"
      cast.code shouldBe "(int) $a"
      cast.lineNumber shouldBe Some(2)
      cast.argument.l match {
        case List(typeRef: TypeRef, expr: Identifier) =>
          typeRef.typeFullName shouldBe "int"
          typeRef.argumentIndex shouldBe 1

          expr.name shouldBe "a"
          expr.argumentIndex shouldBe 2

        case result => fail(s"Expected cast args but got $result")
      }
    }

    "have the correct types" in {
      val testData = List(
        ("(array) $x", PhpDomainTypeConstants.array),
        ("(bool) $x", PhpDomainTypeConstants.bool),
        ("(double) $x", PhpDomainTypeConstants.double),
        ("(int) $x", PhpDomainTypeConstants.int),
        ("(object) $x", PhpDomainTypeConstants.obj),
        ("(string) $x", PhpDomainTypeConstants.string),
        ("(unset) $x", PhpDomainTypeConstants.unset)
      )

      testData.foreach { case (testCode, expectedType) =>
        val cpg = code(s"<?php\n$testCode", fileName = s"Test${filenameKeyPool.next}.php")
        cpg.call.nameExact(Operators.cast).argument(1).l match {
          case List(typeRef: TypeRef) =>
            typeRef.typeFullName shouldBe expectedType
            typeRef.code shouldBe expectedType

          case result => fail(s"Expected typeRef arg for $testCode but found $result")
        }
      }
    }
  }

  "isset calls" should {
    "handle a single argument" in {
      val cpg = code("<?php\nisset($a)")

      val call = cpg.call.nameExact(PhpBuiltins.issetFunc).l match {
        case List(call) => call
        case result     => fail(s"Expected isset call but found $result")
      }

      call.methodFullName shouldBe PhpBuiltins.issetFunc
      call.typeFullName shouldBe TypeConstants.Bool
      call.lineNumber shouldBe Some(2)

      call.argument.l match {
        case List(arg: Identifier) =>
          arg.name shouldBe "a"
          arg.argumentIndex shouldBe 1

        case result => fail(s"Expected isset argument but got $result")
      }
    }

    "handle multiple arguments" in {
      val cpg = code("<?php\nisset($a, $b, $c)")

      val call = cpg.call.nameExact(PhpBuiltins.issetFunc).l match {
        case List(call) => call
        case result     => fail(s"Expected isset call but found $result")
      }

      call.methodFullName shouldBe PhpBuiltins.issetFunc
      call.typeFullName shouldBe TypeConstants.Bool
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.lineNumber shouldBe Some(2)

      call.argument.l match {
        case List(aArg: Identifier, bArg: Identifier, cArg: Identifier) =>
          aArg.name shouldBe "a"
          aArg.argumentIndex shouldBe 1

          bArg.name shouldBe "b"
          bArg.argumentIndex shouldBe 2

          cArg.name shouldBe "c"
          cArg.argumentIndex shouldBe 3

        case result => fail(s"Expected isset arguments but got $result")
      }
    }
  }

  "print calls should be created correctly" in {
    val cpg = code("<?php\nprint(\"Hello, world\");")

    cpg.call.nameExact(PhpBuiltins.printFunc).l match {
      case List(printCall) =>
        printCall.methodFullName shouldBe "print"
        printCall.typeFullName shouldBe TypeConstants.Int
        printCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        printCall.lineNumber shouldBe Some(2)

        printCall.argument.l match {
          case List(arg: Literal) => arg.code shouldBe "\"Hello, world\""
          case result             => fail(s"Expected string argument but found $result")
        }

      case result => fail(s"Expected print call but found $result")
    }
  }

  "ternary operators" should {
    "be created correctly for general cond ? then : else style operators" in {
      val cpg = code("<?php\n$a ? $b : $c")
      val call = cpg.call.nameExact(Operators.conditional).l match {
        case List(conditionalOp) => conditionalOp
        case result              => fail(s"Expected conditional operator but found $result")
      }

      call.methodFullName shouldBe Operators.conditional
      call.lineNumber shouldBe Some(2)

      call.argument.l match {
        case List(aArg: Identifier, bArg: Identifier, cArg: Identifier) =>
          aArg.name shouldBe "a"
          aArg.argumentIndex shouldBe 1

          bArg.name shouldBe "b"
          bArg.argumentIndex shouldBe 2

          cArg.name shouldBe "c"
          cArg.argumentIndex shouldBe 3

        case result => fail(s"Expected 3 conditional args but found $result")
      }
    }

    "be created correctly for the shorthand elvis operator" in {
      val cpg = code("<?php\n$a ?: $b")
      val call = cpg.call.nameExact(PhpBuiltins.elvisOp).l match {
        case List(elvisOp) => elvisOp
        case result        => fail(s"Expected elvis operator but found $result")
      }

      call.methodFullName shouldBe PhpBuiltins.elvisOp
      call.lineNumber shouldBe Some(2)

      call.argument.l match {
        case List(aArg: Identifier, bArg: Identifier) =>
          aArg.name shouldBe "a"
          aArg.argumentIndex shouldBe 1

          bArg.name shouldBe "b"
          bArg.argumentIndex shouldBe 2

        case result => fail(s"Expected 2 elvis operator args but found $result")
      }
    }
  }
}
