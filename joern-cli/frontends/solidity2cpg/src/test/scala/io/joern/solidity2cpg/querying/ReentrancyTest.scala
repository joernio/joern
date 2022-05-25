//package io.joern.solidity2cpg.querying
//
//import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
//import io.shiftleft.codepropertygraph.generated.Operators
//import io.shiftleft.codepropertygraph.generated.nodes.Identifier
//import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}
//
//class ReentrancyTest extends SolidityCodeToCpgFixture {
//
//  override val code: String =
//    """
//      |pragma solidity ^0.4.8;
//      |
//      |contract Reentrancy {
//      |
//      |    mapping (address => uint) balances;
//      |    uint256 money;
//      |    function contribute() external payable {
//      |        balances[msg.sender] += msg.value;
//      |    }
//      |
//      |    function withdraw () public {
//      |        if(balances[msg.sender]== 0) {
//      |            throw;
//      |        }
//      |
//      |        if(msg.sender.call.value(balances[msg.sender]) ()){
//      |            balances[msg.sender] = 0;
//      |        } else {
//      |            throw;
//      |        }
//      |    }
//      |    function getFunds () public returns(uint){
//      |         money += 1;
//      |        return address(this).balance;
//      |    }
//      |}
//    """.stripMargin
//
//  "Reentrancy" in {
////    println(cpg.typeDecl.dotAst.head)
////    println(cpg.typeDecl.fullName.l)
////    println(cpg.method.fullName.l)
//  }
//
//}
