// This test file has been translated from swift/test/Parse/toplevel_library.swift

package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated._
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.semanticcpg.language._

class ToplevelLibraryTests extends AstSwiftSrc2CpgSuite {

  "ToplevelLibraryTests" should {

    "testToplevelLibrary1" in {
      val cpg              = code("var x = 4;")
      val List(assignment) = cpg.call.l
      val List(x)          = assignment.argument.isIdentifier.l
      x.argumentIndex shouldBe 1
      x.name shouldBe "x"
      val List(four) = assignment.argument.isLiteral.l
      four.argumentIndex shouldBe 2
      four.code shouldBe "4"
    }

    "testToplevelLibraryInvalid1" in {
      val cpg = code("""
      |let x = 42
      |x + x;
      |x + x;
      |// Make sure we don't crash on closures at the top level
      |{ }
      |({ 5 }())
      |""".stripMargin)
      cpg.call.code.sorted.l shouldBe List("func <lambda>0 = { }", "let x = 42", "x + x", "x + x", "{ 5 }()")
      cpg.method.fullName.sorted.l shouldBe List(
        "<operator>.assignment",
        "<operator>.plus",
        "Test0.swift:<global>",
        "Test0.swift:<global>:<lambda>0:ANY()",
        "Test0.swift:<global>:<lambda>1:ANY()"
      )
    }

    "testToplevelLibraryInvalid2" in {
      val cpg               = code("for elem in bar() { foo(elem) }")
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l

      val List(localIterator) = loopBlock.astChildren.isLocal.nameExact("_iterator_0").l
      localIterator.code shouldBe "_iterator_0"

      val List(localResult) = loopBlock.astChildren.isLocal.nameExact("_result_0").l
      localResult.code shouldBe "_result_0"

      val List(localI) = loopBlock.astChildren.isLocal.nameExact("elem").l
      localI.code shouldBe "elem"

      val List(iteratorAssignment) =
        loopBlock.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(bar())").l
      iteratorAssignment.name shouldBe Operators.assignment

      val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
      iteratorAssignmentLhs.name shouldBe "_iterator_0"
      iteratorAssignmentLhs.order shouldBe 1
      iteratorAssignmentLhs.argumentIndex shouldBe 1

      val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
      iteratorAssignmentRhs.code shouldBe "<operator>.iterator(bar())"
      iteratorAssignmentRhs.order shouldBe 2
      iteratorAssignmentRhs.argumentIndex shouldBe 2
      iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
      iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
      iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

      val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Call]
      objectKeysCallArg.name shouldBe "bar"
      objectKeysCallArg.code shouldBe "bar()"
      objectKeysCallArg.order shouldBe 1

      val List(varResult) = loopBlock.astChildren.isIdentifier.nameExact("_result_0").l
      varResult.code shouldBe "_result_0"

      val List(varI) = loopBlock.astChildren.isIdentifier.nameExact("elem").l
      varI.code shouldBe "elem"

      val List(loop) = loopBlock.astChildren.isControlStructure.l
      loop.controlStructureType shouldBe ControlStructureTypes.WHILE

      val List(loopTestCall) = loop.astChildren.isCall.codeExact("!(_result_0 = _iterator_0.next()).done").l
      loopTestCall.name shouldBe Operators.not
      loopTestCall.order shouldBe 1

      val List(doneMaCall) = loopTestCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next()).done").l
      doneMaCall.name shouldBe Operators.fieldAccess

      val List(doneMaBase) = doneMaCall.astChildren.isCall.codeExact("(_result_0 = _iterator_0.next())").l
      doneMaBase.name shouldBe Operators.assignment
      doneMaBase.order shouldBe 1
      doneMaBase.argumentIndex shouldBe 1

      val List(doneMaBaseLhs) = doneMaBase.astChildren.isIdentifier.order(1).l
      doneMaBaseLhs.name shouldBe "_result_0"
      doneMaBaseLhs.argumentIndex shouldBe 1

      val List(doneMaBaseRhs) = doneMaBase.astChildren.isCall.order(2).l
      doneMaBaseRhs.code shouldBe "_iterator_0.next()"
      doneMaBaseRhs.argumentIndex shouldBe 2

      val List(doneMember) = doneMaCall.astChildren.isFieldIdentifier.canonicalNameExact("done").l
      doneMember.order shouldBe 2
      doneMember.argumentIndex shouldBe 2

      val List(whileLoopBlock) = loop.astChildren.isBlock.l
      whileLoopBlock.order shouldBe 2

      val List(loopVarAssignmentCall) = whileLoopBlock.astChildren.isCall.codeExact("elem = _result_0.value").l
      loopVarAssignmentCall.name shouldBe Operators.assignment
      loopVarAssignmentCall.order shouldBe 1

      val List(fooCall) = whileLoopBlock.astChildren.isCall.codeExact("foo(elem)").l
      fooCall.name shouldBe "foo"

    }

  }

}
