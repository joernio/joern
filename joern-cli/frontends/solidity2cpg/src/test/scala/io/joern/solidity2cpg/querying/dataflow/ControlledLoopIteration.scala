package io.joern.solidity2cpg.querying.dataflow

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.solidity2cpg.testfixtures.{SolidityCodeToCpgFixture, SolidityDataflowFixture}
import io.shiftleft.codepropertygraph.generated.nodes.ControlStructure
import io.shiftleft.codepropertygraph.generated.{ModifierTypes, Operators}
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}
class ControlledLoopIteration extends SolidityDataflowFixture {
  override val code : String = {
    """
      |contract C{
      |    uint n;
      |
      |    function set(uint _n) public{
      |        n = _n;
      |    }
      |
      |    function bad() public{
      |        uint i;
      |        uint counter;
      |        for(i=0; i<n; i++){
      |            counter = i;
      |        }
      |    }
      |}
      |""".stripMargin
  }
  it should "should find that n is tainted and affects the for loop" in {
    {
      def test = cpg.controlStructure.condition.isCall.argument(2).code.next()
      def sink = cpg.method
        .where(_.hasModifier(ModifierTypes.PUBLIC))
        .call
        .name(s"${Operators.assignment}.*")
        .where(_.argument.isCall.nameExact(Operators.fieldAccess))
        .code(s"${test}.*")

      def source = cpg.method.parameter
      println(sink.reachableByFlows(source).p)
    }
  }
}
