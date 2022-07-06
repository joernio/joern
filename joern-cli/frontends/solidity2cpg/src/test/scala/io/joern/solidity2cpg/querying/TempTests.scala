package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.Identifier
import io.shiftleft.semanticcpg.language._

class TempTests extends SolidityCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String = {
    """
      | // SPDX-License-Identifier: GPL-3.0
      | pragma solidity ^0.8.0;
      |
      |contract Foo {
      | string h;
      | uint256 a;
      |  address payable winner = payable(msg.sender);
      |  bool prizePaidOut;
      |  bool gameHasEnded;
      |  function main(string[] memory args) public {
      |    foo();
      |  }
      |
      |  function foo() public {
      |  if (a == 0) {
      |    h = "here";
      |    if ( a == 5)
      |     h = "found here";
      |    else
      |      revert();
      |    }
      |
      |  }
      |  function send_payment() public {
      |    if (gameHasEnded && !(prizePaidOut)) {
      |      winner.send(1000); // send a prize to the winner
      |      prizePaidOut = true;
      |    }
      |  }
      |
      |  function safe_send_payment() public {
      |    if (gameHasEnded && !(prizePaidOut)) {
      |      if (winner.send(1000))
      |        prizePaidOut = true;
      |      else revert();
      |    }
      |  }
      |
      |}
      |""".stripMargin


  }
  "here" in {
    println(cpg.typeDecl.dotAst.head)
  }
}
