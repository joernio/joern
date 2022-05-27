package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._

class ArithmeticOperationsTests extends SolidityCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      | // SPDX-License-Identifier: GPL-3.0
      | pragma solidity ^0.8.0;
      |
      | contract Additive {
      |
      |    function Foo(uint256 argc, string memory argv) public pure {
      |        uint8 a = 3;
      |        uint256 b = 2; // no floating points in Solidity (yet)
      |        int c = a + b;
      |        uint d = c - a;
      |        uint e = a * b;
      |        int256 f = b / a;
      |    }
      |
      | }
      |""".stripMargin

  val vars = Seq(
    ("a", "uint8"),
    ("b", "uint256"),
    ("c", "int"),
    ("e", "uint"),
    ("f", "int256")
  )

//  (1,METHOD,function Foo(uint256 argc, string argv) public)
//  (0,METHOD_PARAMETER_IN,this)
//  (1,METHOD_PARAMETER_IN,uint256 argc)
//  (2,METHOD_PARAMETER_IN,string memory argv)
//  (3,BLOCK,null)
//  (1,LOCAL,uint8 a)
//  (2,LOCAL,uint256 b)
//  (3,LOCAL,int c)
//  (4,LOCAL,uint d)
//  (5,LOCAL,uint e)
//  (6,LOCAL,int256 f)
//  (7,CALL,a = 3)
//  (1,IDENTIFIER,a)
//  (2,LITERAL,3)
//  (8,CALL,b = 2)
//  (1,IDENTIFIER,b)
//  (2,LITERAL,2)
//  (9,CALL,c = a + b)
//  (1,IDENTIFIER,c)
//  (2,CALL,a + b)
//  (1,IDENTIFIER,a)
//  (2,IDENTIFIER,b)
//  (10,CALL,d = c - a)
//  (1,IDENTIFIER,d)
//  (2,CALL,c - a)
//  (1,IDENTIFIER,c)
//  (2,IDENTIFIER,a)
//  (11,CALL,e = a * b)
//  (1,IDENTIFIER,e)
//  (2,CALL,a * b)
//  (1,IDENTIFIER,a)
//  (2,IDENTIFIER,b)
//  (12,CALL,f = b / a)
//  (1,IDENTIFIER,f)
//  (2,CALL,b / a)
//  (1,IDENTIFIER,b)
//  (2,IDENTIFIER,a)
//  (4,METHOD_RETURN,void)



  "should contain call nodes with <operation>.assignment for all variables" in {
    println(cpg.typeDecl.dotAst.head)
//    println(cpg.assignment.dotAst.head)
//    println(cpg.assignment.name.l)
//    println(cpg.assignment.typeFullName.l)
//    println(cpg.assignment.target.dotAst.head)
//    println(cpg.assignment.dotAst.head)
//    println(cpg.typeDecl.dotAst.head)
<<<<<<< HEAD
//    println(cpg.assignment.name.l)
//    println(cpg.assignment.target.code.l)
//    println(cpg.assignment.typeFullName.l)
=======
>>>>>>> 2562b500953694e462231df77d6405299c99de6f
    val assignments = cpg.assignment
      .map(x => (x.target.code, x.typeFullName))
      .l
    assignments.size shouldBe 6
//    vars.foreach(x => {
//      println(assignments)
//      println(x)
//      assignments contains x shouldBe true
//    })
  }

  "should contain a call node for the addition operator" in {
    val List(op)                           = cpg.call.nameExact(Operators.addition).l
    val List(a: Identifier, b: Identifier) = op.astOut.l
    a.name shouldBe "a"
    b.name shouldBe "b"
  }

  "should contain a call node for the subtraction operator" in {
    val List(op)                           = cpg.call(Operators.subtraction).l
    val List(c: Identifier, a: Identifier) = op.astOut.l
    c.name shouldBe "c"
    a.name shouldBe "a"
  }

  "should contain a call node for the multiplication operator" in {
    val List(op)                           = cpg.call(Operators.multiplication).l
    val List(a: Identifier, b: Identifier) = op.astOut.l
    a.name shouldBe "a"
    b.name shouldBe "b"
  }

  "should contain a call node for the division operator" in {
    val List(op)                           = cpg.call(Operators.division).l
    val List(b: Identifier, a: Identifier) = op.astOut.l
    a.name shouldBe "a"
    b.name shouldBe "b"
  }
}
