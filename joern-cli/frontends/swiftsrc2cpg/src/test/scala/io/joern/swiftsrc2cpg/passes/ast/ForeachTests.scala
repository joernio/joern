package io.joern.swiftsrc2cpg.passes.ast

import io.joern.swiftsrc2cpg.testfixtures.AstSwiftSrc2CpgSuite

import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class ForeachTests extends AstSwiftSrc2CpgSuite {

  "ForeachTests" should {

    "testForeach1" in {
      val cpg = code("""
        |for elem in elements {
        |  foo(elem)
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForLoopWithIdentifier(loopBlock)
    }

    "testForeach2" in {
      val cpg = code("""
        |for elem in elements where elem == 1 {
        |  foo(elem)
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForLoopWithIdentifier(loopBlock, withWhere = true)
    }

    "testForeach3" in {
      val cpg = code("""
        |for (a, b, c) in elements {
        |  foo(a, b, c)
        |}
        |""".stripMargin)
      val List(method)      = cpg.method.nameExact("<global>").l
      val List(methodBlock) = method.astChildren.isBlock.l
      val List(loopBlock)   = methodBlock.astChildren.isBlock.l
      checkForLoopWithTuple(loopBlock)
    }

    "testForeach4" in {
      val cpg = code("""
        |for _ in elements {
        |  foo()
        |}
        |""".stripMargin)
      val List(forLoop)  = cpg.forBlock.l
      val List(elements) = forLoop.astChildren.isIdentifier.l
      elements.name shouldBe "elements"
      elements.code shouldBe "elements"
      elements.order shouldBe 1
      val List(fooCall) = forLoop.astChildren.isCall.l
      fooCall.name shouldBe "foo"
      fooCall.code shouldBe "foo()"
      fooCall.order shouldBe 4
    }

  }

  private def checkForLoopWithIdentifier(node: Block, withWhere: Boolean = false): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(localI) = node.astChildren.isLocal.nameExact("elem").l
    localI.code shouldBe "elem"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(elements)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(elements)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "elements"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(varI) = node.astChildren.isIdentifier.nameExact("elem").l
    varI.code shouldBe "elem"

    val List(loop) = node.astChildren.isControlStructure.l
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

    if (withWhere) {
      val List(whereIf) =
        whileLoopBlock.astChildren.isControlStructure.controlStructureTypeExact(ControlStructureTypes.IF).l
      whereIf.code shouldBe "elem == 1"
      whereIf.condition.isCall.code.l shouldBe List("elem == 1")
      val List(fooCall) = whereIf.whenTrue.isCall.codeExact("foo(elem)").l
      fooCall.name shouldBe "foo"
    } else {
      val List(fooCall) = whileLoopBlock.astChildren.isCall.codeExact("foo(elem)").l
      fooCall.name shouldBe "foo"
    }
  }

  private def checkForLoopWithTuple(node: Block): Unit = {
    val List(localIterator) = node.astChildren.isLocal.nameExact("_iterator_0").l
    localIterator.code shouldBe "_iterator_0"

    val List(localResult) = node.astChildren.isLocal.nameExact("_result_0").l
    localResult.code shouldBe "_result_0"

    val List(localA) = node.astChildren.isLocal.nameExact("a").l
    localA.code shouldBe "a"
    val List(localB) = node.astChildren.isLocal.nameExact("b").l
    localB.code shouldBe "b"
    val List(localC) = node.astChildren.isLocal.nameExact("c").l
    localC.code shouldBe "c"

    val List(iteratorAssignment) =
      node.astChildren.isCall.codeExact("_iterator_0 = <operator>.iterator(elements)").l
    iteratorAssignment.name shouldBe Operators.assignment

    val List(iteratorAssignmentLhs) = iteratorAssignment.astChildren.isIdentifier.l
    iteratorAssignmentLhs.name shouldBe "_iterator_0"
    iteratorAssignmentLhs.order shouldBe 1
    iteratorAssignmentLhs.argumentIndex shouldBe 1

    val List(iteratorAssignmentRhs) = iteratorAssignment.astChildren.isCall.l
    iteratorAssignmentRhs.code shouldBe "<operator>.iterator(elements)"
    iteratorAssignmentRhs.order shouldBe 2
    iteratorAssignmentRhs.argumentIndex shouldBe 2
    iteratorAssignmentRhs.name shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.methodFullName shouldBe "<operator>.iterator"
    iteratorAssignmentRhs.dispatchType shouldBe DispatchTypes.STATIC_DISPATCH

    val objectKeysCallArg = iteratorAssignmentRhs.argument(1).asInstanceOf[Identifier]
    objectKeysCallArg.name shouldBe "elements"
    objectKeysCallArg.order shouldBe 1

    val List(varResult) = node.astChildren.isIdentifier.nameExact("_result_0").l
    varResult.code shouldBe "_result_0"

    val List(varA) = node.astChildren.isIdentifier.nameExact("a").l
    varA.code shouldBe "a"
    val List(varB) = node.astChildren.isIdentifier.nameExact("b").l
    varB.code shouldBe "b"
    val List(varC) = node.astChildren.isIdentifier.nameExact("c").l
    varC.code shouldBe "c"

    val List(loop) = node.astChildren.isControlStructure.l
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

    val List(loopVarAssignmentCallA) = whileLoopBlock.astChildren.isCall.codeExact("a = _result_0.value._1").l
    loopVarAssignmentCallA.name shouldBe Operators.assignment
    loopVarAssignmentCallA.order shouldBe 1
    val List(loopVarAssignmentCallB) = whileLoopBlock.astChildren.isCall.codeExact("b = _result_0.value._2").l
    loopVarAssignmentCallB.name shouldBe Operators.assignment
    loopVarAssignmentCallB.order shouldBe 2
    val List(loopVarAssignmentCallC) = whileLoopBlock.astChildren.isCall.codeExact("c = _result_0.value._3").l
    loopVarAssignmentCallC.name shouldBe Operators.assignment
    loopVarAssignmentCallC.order shouldBe 3

    val List(fooCall) = whileLoopBlock.astChildren.isCall.codeExact("foo(a, b, c)").l
    fooCall.name shouldBe "foo"
  }

}
