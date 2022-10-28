package io.joern.solidity2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.solidity2cpg.testfixtures.{SolidityCodeToCpgFixture, SolidityDataflowFixture}
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}

class TaintedPrivateVariable extends SolidityDataflowFixture {
  override val code: String = {
    """
      |
      |pragma solidity ^0.8.0;
      |
      |contract Foo {
      |
      |  uint private a ;
      |  uint256 public b;
      |  uint8 c;
      |  address newOwner;
      |  address owner = msg.sender;
      |  function editVariable(uint _a) public {
      |      a = _a;
      |  }
      |  function confirmOwner()
      |  public
      |  {
      |        if(msg.sender==newOwner)
      |        {
      |            owner=newOwner;
      |        }
      |  }
      |
      |}""".stripMargin
  }



  it should "should find a is tainted" in {

    {
      cpg.method.parameter
      .filter { current =>
        def sink = cpg.method
          .where(_.hasModifier(ModifierTypes.PUBLIC))
          .call
          .name(s"${Operators.assignment}.*")
          .where(_.argument.isCall.nameExact(Operators.fieldAccess).argument.isFieldIdentifier.canonicalName("^((?!this).)$"))

        sink.reachableByFlows(current).nonEmpty
      }

    }.method.name.l shouldBe List("editVariable")

  }

  }




