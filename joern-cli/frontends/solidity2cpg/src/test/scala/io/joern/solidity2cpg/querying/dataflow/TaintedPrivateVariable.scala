package io.joern.solidity2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}

class TaintedPrivateVariable extends SolidityCodeToCpgFixture {
  implicit val resolver: ICallResolver = NoResolve
  override val code: String = {
    """
      |
      |pragma solidity ^0.8.0;
      |
      |contract Foo {
      |
      |  uint private a ;
      |
      |  function editVariable(uint _a) public {
      |      a = _a;
      |  }
      |
      |}""".stripMargin
  }



  "should throw overflow" in {

    def sink = cpg.method
      .where(_.hasModifier(ModifierTypes.PUBLIC))
      .call
      .name(s"${Operators.assignment}.*")
      .where(_.argument(1).isCall.nameExact(Operators.fieldAccess))
    def source = cpg.method.parameter

//    sink.reachableByFlows(source).p

  }
}




