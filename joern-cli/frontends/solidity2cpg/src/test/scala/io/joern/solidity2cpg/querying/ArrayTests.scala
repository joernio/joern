//package io.joern.solidity2cpg.querying
//
//import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
//import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}
//import io.shiftleft.codepropertygraph.generated.Operators
//import io.shiftleft.codepropertygraph.generated.nodes._
//import io.shiftleft.proto.cpg.Cpg.DispatchTypes
//import io.shiftleft.semanticcpg.language._
//import org.scalatest.Failed
//class ArrayTests extends SolidityCodeToCpgFixture {
//
//  implicit val resolver: ICallResolver = NoResolve
//
//  override val code: String =
//    """
//      |pragma solidity ^0.8.0;
//      |
//      |contract Foo {
//      |
//      |    function foo() public pure {
//      |        uint8[3] memory x = [0, 1 ,2];
//      |    }
//      |
//      |    function bar() public pure {
//      |        uint256[][] memory x = new uint256[][](10);
//      |    }
//      |
//      |    function baz() public pure {
//      |        uint256[] memory x = new uint256[](2);
//      |        x[0] = 1;
//      |        x[1] = x[0] + 2;
//      |    }
//      |}
//      |""".stripMargin
//
//  "should initialize array with three address code initialization expressions" in {
//    def m = cpg.method(".*foo.*")
//
//    val List(placeholderArg: Identifier, arrayInit: Call) =
//      m.assignment.codeExact("$stack2 = uint8[3]").argument.l
//    placeholderArg.code shouldBe "$stack2"
//    placeholderArg.typeFullName shouldBe "uint8[]"
//
//    arrayInit.code shouldBe "uint8[3] memory"
//    arrayInit.methodFullName shouldBe "<operator>.arrayCreator"
//    arrayInit.astChildren.headOption match {
//      case Some(alloc) =>
//        alloc shouldBe a[Literal]
//        alloc.code shouldBe "3"
//      case None => Failed("arrayInitializer should have a literal with the value of 3")
//    }
//
//    val List(stackAt0: Call, arg0: Literal) = m.assignment.codeExact("$stack2[0] = 0").argument.l
//
//    arg0.code shouldBe "0"
//    arg0.typeFullName shouldBe "int"
//
//    stackAt0.code shouldBe "$stack2[0]"
//    stackAt0.methodFullName shouldBe Operators.indexAccess
//    val List(stackPointerAt0: Identifier, zero: Literal) = stackAt0.astChildren.l
//    stackPointerAt0.code shouldBe "$stack2"
//    zero.code shouldBe "0"
//
//    val List(stackAt1: Call, arg1: Literal) = m.assignment.codeExact("$stack2[1] = 1").argument.l
//
//    arg1.code shouldBe "1"
//    arg1.typeFullName shouldBe "int"
//
//    stackAt1.code shouldBe "$stack2[1]"
//    stackAt1.methodFullName shouldBe Operators.indexAccess
//    val List(stackPointerAt1: Identifier, one: Literal) = stackAt1.astChildren.l
//    stackPointerAt1.code shouldBe "$stack2"
//    one.code shouldBe "1"
//
//    val List(stackAt2: Call, arg2: Literal) = m.assignment.codeExact("$stack2[2] = 2").argument.l
//
//    arg2.code shouldBe "2"
//    arg2.typeFullName shouldBe "int"
//
//    stackAt2.code shouldBe "$stack2[2]"
//    stackAt2.methodFullName shouldBe Operators.indexAccess
//    val List(stackPointerAt2: Identifier, two: Literal) = stackAt2.astChildren.l
//    stackPointerAt2.code shouldBe "$stack2"
//    two.code shouldBe "2"
//  }
//
//  "should initialize an array with empty initialization expression" in {
//    def m = cpg.method(".*bar.*")
//
//    val List(arg1: Identifier, arg2: Call) =
//      m.assignment.codeExact("x = new int[5][2]").argument.l
//
//    arg1.typeFullName shouldBe "int[][]"
//
//    arg2.code shouldBe "new int[5][2]"
//    val List(lvl1: Literal, lvl2: Literal) = arg2.argument.l
//    lvl1.code shouldBe "5"
//    lvl2.code shouldBe "2"
//  }
//
//  "should handle arrayIndexAccesses correctly (3-address code form)" in {
//    def m = cpg.method(".*baz.*")
//
//    val List(indexAccess: Call, rhsStub: Identifier) = m.assignment.codeExact("x[1] = $stack3").argument.l
//    indexAccess.name shouldBe Operators.indexAccess
//    indexAccess.methodFullName shouldBe Operators.indexAccess
//
//    withClue("indexAccess on LHS of assignment") {
//      val List(arg1: Identifier, arg2: Literal) = indexAccess.argument.l
//      arg1.code shouldBe "x"
//      arg1.name shouldBe "x"
//      arg1.typeFullName shouldBe "int[]"
//      arg2.code shouldBe "1"
//    }
//
//    withClue("placeholder in expr on RHS of assignment") {
//      rhsStub.name shouldBe "$stack3"
//      rhsStub.typeFullName shouldBe "int"
//      rhsStub.code shouldBe "$stack3"
//    }
//  }
//}
