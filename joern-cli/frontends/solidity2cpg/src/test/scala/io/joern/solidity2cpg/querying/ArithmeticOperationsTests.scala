package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}

class ArithmeticOperationsTests extends SolidityCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

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
      |""".stripMargin

  val vars = Seq(
    ("a", "byte"),
    ("b", "double"),
    ("c", "double"),
    ("d", "double"),
    ("e", "double"),
    ("f", "double"),
    ("g", "long"),
    ("h", "float")
  )

  "should contain call nodes with <operation>.assignment for all variables" in {
    val assignments = cpg.assignment
      .filterNot(_.target.code.startsWith("$"))
      .filterNot(x => List("argc", "argv", "this").contains(x.target.code))
      .map(x => (x.target.code, x.typeFullName))
      .l
    assignments.size shouldBe 8 // includes casting and 3-address code manipulations
    vars.foreach(x => {
      assignments contains x shouldBe true
    })
  }

  "should contain a call node for the addition operator" in {
    val List(op)                           = cpg.call.nameExact(Operators.addition).l
    val List(a: Identifier, b: Identifier) = op.astOut.l
    a.name shouldBe "$stack16"
    b.name shouldBe "b"
  }

  "should contain a call node for the subtraction operator" in {
    val List(op)                           = cpg.call(Operators.subtraction).l
    val List(c: Identifier, a: Identifier) = op.astOut.l
    c.name shouldBe "c"
    a.name shouldBe "$stack17"
  }

  "should contain a call node for the multiplication operator" in {
    val List(op)                           = cpg.call(Operators.multiplication).l
    val List(a: Identifier, b: Identifier) = op.astOut.l
    a.name shouldBe "$stack18"
    b.name shouldBe "b"
  }

  "should contain a call node for the division operator" in {
    val List(op)                           = cpg.call(Operators.division).l
    val List(b: Identifier, a: Identifier) = op.astOut.l
    a.name shouldBe "$stack19"
    b.name shouldBe "b"
  }
}
