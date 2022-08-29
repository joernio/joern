package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._
class CfgTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """
      |pragma solidity ^0.8.0;
      |
      |contract Foo {
      |    event print(uint256 x);
      |    event printString(string x);
      |    function foo(uint256 x, uint256 y) public returns(uint256) {
      |        if (y < 10) {
      |            return 1;
      |        }
      |        if (x < 5) {
      |            sink(x);
      |        }
      |        emit printString("foo");
      |        return 0;
      |    }
      |
      |    function sink(uint256 x) public returns(uint256) {
      |        emit print(x);
      |    }
      |}
    """.stripMargin

  "should find that sink is control dependent on condition" in {
    val controllers = cpg.call("sink").controlledBy.isCall.toSetMutable
    controllers.map(_.code) should contain("y < 10")
    controllers.map(_.code) should contain("x < 5")
  }

  "should find sink(x) is dominated by `x < 5` and `y < 10`" in {
    cpg.call("sink").dominatedBy.isCall.code.toSetMutable shouldBe Set("x < 5", "y < 10")
  }

}
