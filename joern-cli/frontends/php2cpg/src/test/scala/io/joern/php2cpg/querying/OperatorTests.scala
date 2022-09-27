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

      val assignment = inside(cpg.call.l) {
        case List(call) if call.name == Operators.assignment => call
      }

      inside(assignment.argument.l) { case List(target: Identifier, source: Literal) =>
        target.name shouldBe "a"
        target.code shouldBe "$a"
        target.argumentIndex shouldBe 1

        source.code shouldBe "2"
        source.argumentIndex shouldBe 2
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
        cpg.call.code.l shouldBe testCode :: Nil
      }
    }
  }

  "unary operators" should {
    "have the correct arguments set" in {
      val cpg = code("""<?php
                      |+$a
                      |""".stripMargin)

      val addition = inside(cpg.call.l) {
        case List(call) if call.name == Operators.plus => call
      }

      inside(addition.argument.l) { case List(expr: Identifier) =>
        expr.name shouldBe "a"
        expr.code shouldBe "$a"
        expr.argumentIndex shouldBe 1
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
        cpg.call.code.l shouldBe testCode :: Nil
      }
    }
  }

  "binary operators" should {
    "have the correct arguments set" in {
      val cpg = code("""<?php
                      |$a + 2
                      |""".stripMargin)

      val addition = inside(cpg.call.l) {
        case List(call) if call.name == Operators.plus => call
      }

      inside(addition.argument.l) { case List(target: Identifier, source: Literal) =>
        target.name shouldBe "a"
        target.code shouldBe "$a"
        target.argumentIndex shouldBe 1

        source.code shouldBe "2"
        source.argumentIndex shouldBe 2
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

      def normalizeLogicalOps(input: String): String = {
        input
          .replaceAll(" or ", " || ")
          .replaceAll(" and ", " && ")
          .replaceAll(" <> ", " != ")
      }

      testData.foreach { case (testCode, expectedType) =>
        val cpg = code(s"<?php\n$testCode", fileName = s"Test${filenameKeyPool.next}.php")
        cpg.call.name.l shouldBe expectedType :: Nil
        cpg.call.methodFullName.l shouldBe expectedType :: Nil
        cpg.call.code.l shouldBe normalizeLogicalOps(testCode) :: Nil
      }
    }
  }

  "cast operations" should {
    "have the correct arguments set" in {
      val cpg = code("<?php\n(int) $a")

      val cast = inside(cpg.call.nameExact(Operators.cast).l) { case List(cast) =>
        cast
      }

      cast.typeFullName shouldBe "int"
      cast.code shouldBe "(int) $a"
      cast.lineNumber shouldBe Some(2)

      inside(cast.argument.l) { case List(typeRef: TypeRef, expr: Identifier) =>
        typeRef.typeFullName shouldBe "int"
        typeRef.argumentIndex shouldBe 1

        expr.name shouldBe "a"
        expr.argumentIndex shouldBe 2
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

        inside(cpg.call.nameExact(Operators.cast).argument(1).l) { case List(typeRef: TypeRef) =>
          typeRef.typeFullName shouldBe expectedType
          typeRef.code shouldBe expectedType
        }
      }
    }
  }

  "isset calls" should {
    "handle a single argument" in {
      val cpg = code("<?php\nisset($a)")

      val call = inside(cpg.call.nameExact(PhpBuiltins.issetFunc).l) { case List(call) =>
        call
      }

      call.methodFullName shouldBe PhpBuiltins.issetFunc
      call.typeFullName shouldBe TypeConstants.Bool
      call.lineNumber shouldBe Some(2)

      inside(call.argument.l) { case List(arg: Identifier) =>
        arg.name shouldBe "a"
        arg.code shouldBe "$a"
        arg.argumentIndex shouldBe 1
      }
    }

    "handle multiple arguments" in {
      val cpg = code("<?php\nisset($a, $b, $c)")

      val call = inside(cpg.call.nameExact(PhpBuiltins.issetFunc).l) { case List(call) =>
        call
      }

      call.methodFullName shouldBe PhpBuiltins.issetFunc
      call.typeFullName shouldBe TypeConstants.Bool
      call.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      call.lineNumber shouldBe Some(2)

      inside(call.argument.l) { case List(aArg: Identifier, bArg: Identifier, cArg: Identifier) =>
        aArg.name shouldBe "a"
        aArg.code shouldBe "$a"
        aArg.argumentIndex shouldBe 1

        bArg.name shouldBe "b"
        bArg.code shouldBe "$b"
        bArg.argumentIndex shouldBe 2

        cArg.name shouldBe "c"
        cArg.code shouldBe "$c"
        cArg.argumentIndex shouldBe 3
      }
    }
  }

  "print calls should be created correctly" in {
    val cpg = code("<?php\nprint(\"Hello, world\");")

    inside(cpg.call.nameExact(PhpBuiltins.printFunc).l) { case List(printCall) =>
      printCall.methodFullName shouldBe "print"
      printCall.typeFullName shouldBe TypeConstants.Int
      printCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      printCall.lineNumber shouldBe Some(2)

      inside(printCall.argument.l) { case List(arg: Literal) =>
        arg.code shouldBe "\"Hello, world\""
      }
    }
  }

  "ternary operators" should {
    "be created correctly for general cond ? then : else style operators" in {
      val cpg = code("<?php\n$a ? $b : $c")

      val call = inside(cpg.call.nameExact(Operators.conditional).l) { case List(conditionalOp) =>
        conditionalOp
      }

      call.methodFullName shouldBe Operators.conditional
      call.code shouldBe "$a ? $b : $c"
      call.lineNumber shouldBe Some(2)

      inside(call.argument.l) { case List(aArg: Identifier, bArg: Identifier, cArg: Identifier) =>
        aArg.name shouldBe "a"
        aArg.code shouldBe "$a"
        aArg.argumentIndex shouldBe 1

        bArg.name shouldBe "b"
        bArg.code shouldBe "$b"
        bArg.argumentIndex shouldBe 2

        cArg.name shouldBe "c"
        cArg.code shouldBe "$c"
        cArg.argumentIndex shouldBe 3
      }
    }

    "be created correctly for the shorthand elvis operator" in {
      val cpg = code("<?php\n$a ?: $b")
      val call = inside(cpg.call.nameExact(PhpBuiltins.elvisOp).l) { case List(elvisOp) =>
        elvisOp
      }

      call.methodFullName shouldBe PhpBuiltins.elvisOp
      call.code shouldBe "$a ?: $b"
      call.lineNumber shouldBe Some(2)

      inside(call.argument.l) { case List(aArg: Identifier, bArg: Identifier) =>
        aArg.name shouldBe "a"
        aArg.code shouldBe "$a"
        aArg.argumentIndex shouldBe 1

        bArg.name shouldBe "b"
        bArg.code shouldBe "$b"
        bArg.argumentIndex shouldBe 2
      }
    }
  }
}
