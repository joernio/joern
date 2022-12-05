package io.joern.php2cpg.querying

import io.joern.php2cpg.astcreation.AstCreator.{NameConstants, TypeConstants}
import io.joern.php2cpg.parser.Domain.{PhpOperators, PhpDomainTypeConstants}
import io.joern.php2cpg.testfixtures.PhpCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{Block, Call, Identifier, Literal, TypeRef}
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

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
        ("$a ??= $b", PhpOperators.assignmentCoalesceOp),
        ("$a .= $b", PhpOperators.assignmentConcatOp),
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
        ("$a ?? $b", PhpOperators.coalesceOp),
        ("$a . $b", PhpOperators.concatOp),
        ("$a / $b", Operators.division),
        ("$a == $b", Operators.equals),
        ("$a >= $b", Operators.greaterEqualsThan),
        ("$a > $b", Operators.greaterThan),
        ("$a === $b", PhpOperators.identicalOp),
        ("$a and $b", Operators.logicalAnd),
        ("$a or $b", Operators.logicalOr),
        ("$a xor $b", PhpOperators.logicalXorOp),
        ("$a - $b", Operators.minus),
        ("$a % $b", Operators.modulo),
        ("$a * $b", Operators.multiplication),
        ("$a != $b", Operators.notEquals),
        ("$a <> $b", Operators.notEquals),
        ("$a !== $b", PhpOperators.notIdenticalOp),
        ("$a + $b", Operators.plus),
        ("$a ** $b", Operators.exponentiation),
        ("$a << $b", Operators.shiftLeft),
        ("$a >> $b", Operators.arithmeticShiftRight),
        ("$a <= $b", Operators.lessEqualsThan),
        ("$a < $b", Operators.lessThan),
        ("$a <=> $b", PhpOperators.spaceshipOp)
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

      val call = inside(cpg.call.nameExact("isset").l) { case List(call) =>
        call
      }

      call.methodFullName shouldBe PhpOperators.issetFunc
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

      val call = inside(cpg.call.nameExact("isset").l) { case List(call) =>
        call
      }

      call.methodFullName shouldBe PhpOperators.issetFunc
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

    inside(cpg.call.nameExact("print").l) { case List(printCall) =>
      printCall.methodFullName shouldBe "__builtin.print"
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
      val call = inside(cpg.call.nameExact(PhpOperators.elvisOp).l) { case List(elvisOp) =>
        elvisOp
      }

      call.methodFullName shouldBe PhpOperators.elvisOp
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

  "the clone operator should be represented with the correct call node" in {
    val cpg = code("<?php\nclone $x")

    inside(cpg.call.l) { case List(cloneCall) =>
      cloneCall.name shouldBe "clone"
      cloneCall.methodFullName shouldBe PhpOperators.cloneFunc
      cloneCall.code shouldBe "clone $x"
      cloneCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      cloneCall.lineNumber shouldBe Some(2)

      inside(cloneCall.argument.l) { case List(xArg: Identifier) =>
        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "the empty call should be represented with the correct call node" in {
    val cpg = code("<?php\nempty($x)")

    inside(cpg.call.l) { case List(emptyCall) =>
      emptyCall.name shouldBe "empty"
      emptyCall.methodFullName shouldBe PhpOperators.emptyFunc
      emptyCall.code shouldBe "empty($x)"
      emptyCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      emptyCall.lineNumber shouldBe Some(2)

      inside(emptyCall.argument.l) { case List(xArg: Identifier) =>
        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "the eval call should be represented with the correct call node" in {
    val cpg = code("<?php\neval($x)")

    inside(cpg.call.l) { case List(evalCall) =>
      evalCall.name shouldBe "eval"
      evalCall.methodFullName shouldBe PhpOperators.evalFunc
      evalCall.code shouldBe "eval($x)"
      evalCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      evalCall.lineNumber shouldBe Some(2)

      inside(evalCall.argument.l) { case List(xArg: Identifier) =>
        xArg.name shouldBe "x"
        xArg.code shouldBe "$x"
      }
    }
  }

  "exit statements" should {
    "be represented with an empty arg list if no args are given" in {
      val cpg = code("<?php\nexit;")
      inside(cpg.call.l) { case List(exitCall) =>
        exitCall.name shouldBe "exit"
        exitCall.methodFullName shouldBe PhpOperators.exitFunc
        exitCall.typeFullName shouldBe TypeConstants.Void
        exitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        exitCall.lineNumber shouldBe Some(2)
        exitCall.argument.size shouldBe 0
        exitCall.astChildren.size shouldBe 0
      }
    }

    "be represented with an empty arg list if an empty args list is given" in {
      val cpg = code("<?php\nexit();")
      inside(cpg.call.l) { case List(exitCall) =>
        exitCall.name shouldBe "exit"
        exitCall.methodFullName shouldBe PhpOperators.exitFunc
        exitCall.typeFullName shouldBe TypeConstants.Void
        exitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        exitCall.lineNumber shouldBe Some(2)
        exitCall.argument.size shouldBe 0
        exitCall.astChildren.size shouldBe 0
      }
    }

    "have the correct arg child if an arg is given" in {
      val cpg = code("<?php\nexit(0);")
      inside(cpg.call.l) { case List(exitCall) =>
        exitCall.name shouldBe "exit"
        exitCall.methodFullName shouldBe PhpOperators.exitFunc
        exitCall.typeFullName shouldBe TypeConstants.Void
        exitCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
        exitCall.lineNumber shouldBe Some(2)

        inside(cpg.argument.l) { case List(literal: Literal) =>
          literal.code shouldBe "0"
        }
      }
    }
  }

  "the error suppress operator should work" in {
    val cpg = code("<?php\n@foo();")

    inside(cpg.call.nameExact(PhpOperators.errorSuppress).l) { case List(errorSuppress) =>
      errorSuppress.methodFullName shouldBe PhpOperators.errorSuppress
      errorSuppress.code shouldBe "@foo()"

      inside(errorSuppress.argument.l) { case List(fooCall: Call) =>
        fooCall.name shouldBe "foo"
        fooCall.code shouldBe "foo()"
      }
    }
  }

  "instanceof with a simple class name should work" in {
    val cpg = code("<?php\n$foo instanceof Foo")

    inside(cpg.call.l) { case List(instanceOfCall) =>
      instanceOfCall.name shouldBe Operators.instanceOf
      instanceOfCall.methodFullName shouldBe Operators.instanceOf
      instanceOfCall.code shouldBe "$foo instanceof Foo"

      inside(instanceOfCall.argument.l) { case List(obj: Identifier, className: Identifier) =>
        obj.name shouldBe "foo"
        obj.code shouldBe "$foo"

        className.name shouldBe "Foo"
        className.code shouldBe "Foo"
      }
    }
  }

  "temporary list implementation should work" in {
    // TODO This is a simple placeholder implementation that represents most of the useful information
    //  in the AST, while being pretty much unusable for dataflow. A better implementation needs to follow.
    val cpg = code("<?php\nlist($a, $b) = $arr;")

    inside(cpg.call.nameExact("list").l) { case List(listCall: Call) =>
      listCall.methodFullName shouldBe PhpOperators.listFunc
      listCall.code shouldBe "list($a,$b)"
      listCall.lineNumber shouldBe Some(2)
      inside(listCall.argument.l) { case List(aArg: Identifier, bArg: Identifier) =>
        aArg.name shouldBe "a"
        aArg.code shouldBe "$a"
        aArg.lineNumber shouldBe Some(2)

        bArg.name shouldBe "b"
        bArg.code shouldBe "$b"
        bArg.lineNumber shouldBe Some(2)
      }
    }
  }

  "include calls" should {
    "be correctly represented for normal includes" in {
      val cpg = code("<?php\ninclude 'path';")

      inside(cpg.call.l) { case List(includeCall: Call) =>
        includeCall.name shouldBe "include"
        includeCall.methodFullName shouldBe "include"
        includeCall.code shouldBe "include \"path\""
        inside(includeCall.argument.l) { case List(pathLiteral: Literal) =>
          pathLiteral.code shouldBe "\"path\""
        }
      }
    }

    "be correctly represented for include_once" in {
      val cpg = code("<?php\ninclude_once 'path';")

      inside(cpg.call.l) { case List(includeOnceCall: Call) =>
        includeOnceCall.name shouldBe "include_once"
        includeOnceCall.methodFullName shouldBe "include_once"
        includeOnceCall.code shouldBe "include_once \"path\""
        inside(includeOnceCall.argument.l) { case List(pathLiteral: Literal) =>
          pathLiteral.code shouldBe "\"path\""
        }
      }
    }

    "be correctly represented for normal requires" in {
      val cpg = code("<?php\nrequire 'path';")

      inside(cpg.call.l) { case List(requireCall: Call) =>
        requireCall.name shouldBe "require"
        requireCall.methodFullName shouldBe "require"
        requireCall.code shouldBe "require \"path\""
        inside(requireCall.argument.l) { case List(pathLiteral: Literal) =>
          pathLiteral.code shouldBe "\"path\""
        }
      }
    }

    "be correctly represented for require once" in {
      val cpg = code("<?php\nrequire_once 'path';")

      inside(cpg.call.l) { case List(requireOnce: Call) =>
        requireOnce.name shouldBe "require_once"
        requireOnce.methodFullName shouldBe "require_once"
        requireOnce.code shouldBe "require_once \"path\""
        inside(requireOnce.argument.l) { case List(pathLiteral: Literal) =>
          pathLiteral.code shouldBe "\"path\""
        }
      }
    }
  }

  "declare calls without statements should be correctly represented" in {
    val cpg = code("<?php\ndeclare(ticks=1, encoding='UTF-8');")

    val declareCall = inside(cpg.method.nameExact(NamespaceTraversal.globalNamespaceName).l) {
      case List(globalMethod) =>
        inside(globalMethod.body.astChildren.l) { case List(declareCall: Call) =>
          declareCall
        }
    }

    declareCall.name shouldBe PhpOperators.declareFunc
    declareCall.methodFullName shouldBe PhpOperators.declareFunc
    declareCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
    declareCall.code shouldBe "declare(ticks=1,encoding=\"UTF-8\")"
    declareCall.lineNumber shouldBe Some(2)

    inside(declareCall.argument.l) { case List(tickAssign: Call, encodingAssign: Call) =>
      tickAssign.name shouldBe Operators.assignment
      tickAssign.lineNumber shouldBe Some(2)
      tickAssign.code shouldBe "ticks=1"

      inside(tickAssign.argument.l) { case List(ticksIdentifier: Identifier, value: Literal) =>
        ticksIdentifier.name shouldBe "ticks"
        ticksIdentifier.code shouldBe "ticks"

        value.code shouldBe "1"
      }

      encodingAssign.name shouldBe Operators.assignment
      encodingAssign.lineNumber shouldBe Some(2)
      encodingAssign.code shouldBe "encoding=\"UTF-8\""

      inside(encodingAssign.argument.l) { case List(encodingIdentifier: Identifier, value: Literal) =>
        encodingIdentifier.name shouldBe "encoding"
        encodingIdentifier.code shouldBe "encoding"

        value.code shouldBe "\"UTF-8\""
      }
    }
  }

  "declare calls with an empty statement list should have the correct block structure" in {
    val cpg = code("""<?php
        |declare(ticks=1) {}
        |""".stripMargin)

    val declareBlock = inside(cpg.method.nameExact(NamespaceTraversal.globalNamespaceName).l) {
      case List(globalMethod) =>
        inside(globalMethod.body.astChildren.l) { case List(declareBlock: Block) =>
          declareBlock
        }
    }

    inside(declareBlock.astChildren.l) { case List(declareCall: Call) =>
      declareCall.code shouldBe "declare(ticks=1)"
    }
  }

  "declare calls with non-empty statement lists should have the correct block structure" in {
    val cpg = code("""<?php
        |declare(ticks=1) {
        |  echo $x;
        |}
        |""".stripMargin)

    val declareBlock = inside(cpg.method.nameExact(NamespaceTraversal.globalNamespaceName).l) {
      case List(globalMethod) =>
        inside(globalMethod.body.astChildren.l) { case List(declareBlock: Block) =>
          declareBlock
        }
    }

    inside(declareBlock.astChildren.l) { case List(declareCall: Call, echoCall: Call) =>
      declareCall.code shouldBe "declare(ticks=1)"
      echoCall.code shouldBe "echo $x"
    }
  }

  "shell_exec calls should be handled" in {
    val cpg = code("<?php\n`ls -la`")

    inside(cpg.call.name("shell_exec").l) { case List(shellCall) =>
      shellCall.methodFullName shouldBe "__builtin.shell_exec"
      shellCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      shellCall.code shouldBe "`ls -la`"
      shellCall.lineNumber shouldBe Some(2)

      inside(shellCall.argument.l) { case List(command: Literal) =>
        command.code shouldBe "\"ls -la\""
      }
    }
  }

  "unset calls should be handled" in {
    val cpg = code("<?php\nunset($a, $b)")

    inside(cpg.call.l) { case List(unsetCall) =>
      unsetCall.name shouldBe "unset"
      unsetCall.methodFullName shouldBe "__builtin.unset"
      unsetCall.code shouldBe "unset($a, $b)"
      unsetCall.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH
      unsetCall.lineNumber shouldBe Some(2)

      inside(unsetCall.argument.l) { case List(aArg: Identifier, bArg: Identifier) =>
        aArg.name shouldBe "a"
        aArg.code shouldBe "$a"
        aArg.lineNumber shouldBe Some(2)

        bArg.name shouldBe "b"
        bArg.code shouldBe "$b"
        bArg.lineNumber shouldBe Some(2)
      }
    }
  }

  "global calls should handle simple and non-simple args" in {
    val cpg = code("<?php\nglobal $a, $$b")

    inside(cpg.call.l) { case List(globalCall) =>
      globalCall.name shouldBe "global"
      globalCall.methodFullName shouldBe "global"
      globalCall.code shouldBe "global $a, $$b"
      globalCall.lineNumber shouldBe Some(2)

      inside(globalCall.argument.l) { case List(aArg: Identifier, bArg: Identifier) =>
        aArg.name shouldBe "a"
        bArg.name shouldBe "b"
      }
    }
  }

  "calls to builtins defined in resources/builtin_functions.txt should be handled correctly" in {
    val cpg = code("<?php\nabs($a)")

    inside(cpg.call.l) { case List(absCall) =>
      absCall.name shouldBe "abs"
      absCall.methodFullName shouldBe "__builtin.abs"
      absCall.code shouldBe "abs($a)"
      absCall.signature.isEmpty shouldBe true
    }
  }
}
