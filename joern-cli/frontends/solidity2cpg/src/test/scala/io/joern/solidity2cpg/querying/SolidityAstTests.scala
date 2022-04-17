package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}

class SolidityAstTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """
      | // SPDX-License-Identifier: GPL-3.0
      | pragma solidity ^0.8.0;
      |
      | contract Additive{
      |
      |    function Foo(uint256 argc, string memory argv ) public pure {
      |        int256 a = 3;
      |        int256 b = 2.0;
      |        int256 c = a + b;
      |        int256 e = a * b;
      |        int256 f = b/a;
      |    }
      |
      | }
    """.stripMargin
    val vars = Array(
      "Additive",
      "ANY"
    )
  "Identifies function names" in {
   val typeDecl = cpg.typeDecl.map(x => x.fullName)
   var counter = 0
    while (typeDecl.hasNext) {
      typeDecl.next().equals(vars(counter)) shouldBe true
      counter = counter + 1;
    }
  }

  "Identifies function names code" in {
    val typeDecl = cpg.typeDecl.map(x => (x.fullName, x.code))
    while (typeDecl.hasNext) {
     println(typeDecl.next())
    }
  }

//  "Identifies function names" in {
//    val typeDecl = cpg.typeDecl.map(x => x.fullName)
//    var counter = 0
//    while (typeDecl.hasNext) {
//      typeDecl.next().equals(vars(counter)) shouldBe true
//      counter = counter + 1;
//    }
//  }

//  "should allow finding addition in argument to bar" in {
//    implicit val resolver: ICallResolver = NoResolve
//    cpg.method
//      .name("bar")
//      .callIn
//      .argument(1)
//      .containsCallTo("<operator>.(addition|multiplication)")
//      .code
//      .l shouldBe List("x + 10")
//  }
//
//  "should allow finding that addition is not a direct argument of moo" in {
//    implicit val resolver: ICallResolver = NoResolve
//
//    cpg.method
//      .name("moo")
//      .callIn
//      .argument(1)
//      .containsCallTo("<operator>.(addition|multiplication)")
//      .code
//      .l shouldBe List("boo(1+2)")
//
//    cpg.method
//      .name("moo")
//      .callIn
//      .argument(1)
//      .filter(arg =>
//        arg.ast
//          .isCallTo("<operator>.(addition|multiplication)")
//          .not(_.inAstMinusLeaf(arg).isCall)
//          .l
//          .nonEmpty
//      )
//      .code
//      .l shouldBe List()
//  }
//
//  "should identify three control structures" in {
//    cpg.method
//      .name("foo")
//      .ast
//      .isControlStructure
//      .isIf
//      .l
//      .size shouldBe 2
//
//    cpg.method
//      .name("foo")
//      .ast
//      .isControlStructure
//      .isElse
//      .l
//      .size shouldBe 1
//  }
//
//  "should allow basic calling basic 'is' methods on AST node" in {
//    cpg.method.name("foo").ast.isFile.l.size shouldBe 0
//    cpg.method.name("foo").ast.isMember.l.size shouldBe 0
//    cpg.method.name("foo").ast.isModifier.l.size shouldBe 0
//    cpg.method.name("foo").ast.isNamespaceBlock.l.size shouldBe 0
//    cpg.method.name("foo").ast.isParameter.l.size shouldBe 1
//    cpg.method.name("foo").ast.isTypeDecl.l.size shouldBe 0
//  }
//
//  "should identify conditions" in {
//    cpg.method.name("foo").controlStructure.condition.code.l shouldBe List("x > 10", "y > x")
//  }
//
//  "should allow parserTypeName filtering and then ast" in {
//    val query1Size = cpg.method.name("foo").ast.isControlStructure.ast.l.size
//    query1Size should be > 0
//
//    val query2Size = cpg.method
//      .name("foo")
//      .ast
//      .isControlStructure
//      .parserTypeName(".*")
//      .ast
//      .l
//      .size
//    query1Size shouldBe query2Size
//  }
//
//  "should allow filtering on control structures" in {
//    cpg.method
//      .name("foo")
//      .controlStructure(".*x > 10.*")
//      .l
//      .size shouldBe 1
//
//    cpg.method
//      .name("foo")
//      .controlStructure(".*x > 10.*")
//      .whenTrue
//      .ast
//      .isReturn
//      .code
//      .l shouldBe List("return bar(x + 10);")
//
//    cpg.method
//      .name("foo")
//      .controlStructure(".*x > 10.*")
//      .whenFalse
//      .ast
//      .isCall
//      .code(".*printf.*")
//      .code
//      .l shouldBe List("printf(\"reached\")")
//  }
//}
//
//class CAstTests2 extends FuzzyCCodeToCpgSuite {
//
//  override val code =
//    """
//      |void foo(int bar) {
//      | char buf[10];
//      | int i;
//      | for (int i = 0; i < bar; i++) {
//      |   buf[i] = 42;
//      | }
//      |}
//    """.stripMargin
//
//  "should find index `i` used for buf" in {
//
//    cpg.call
//      .name("<operator>.indirectIndexAccess")
//      .argument
//      .argumentIndex(2)
//      .code
//      .l shouldBe List("i")
//  }
//
//  "should find that i is assigned as part of loop header" in {
//
//    cpg.call
//      .name("<operator>.indirectIndexAccess")
//      .argument
//      .argumentIndex(2)
//      .inAstMinusLeaf
//      .isControlStructure
//      .code
//      .l shouldBe List("for (int i = 0; i < bar; i++)")
//
//  }
//
//  "should correctly identify condition of for loop" in {
//    cpg.call
//      .name("<operator>.indirectIndexAccess")
//      .argument
//      .argumentIndex(2)
//      .inAstMinusLeaf
//      .isControlStructure
//      .condition
//      .code
//      .l shouldBe List("i < bar")
//  }

}
