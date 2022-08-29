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

  val vars = Seq(("a", "uint8"), ("b", "uint256"), ("c", "int"), ("e", "uint"), ("f", "int256"))

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.assignment
      .map(x => (x.target.code, x.typeFullName))
      .l
    assignments.size shouldBe 6
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
