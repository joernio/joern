package io.joern.solidity2cpg.querying
import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.semanticcpg.language.{ICallResolver, NoResolve, toNodeTypeStarters, _}

class CallTests extends SolidityCodeToCpgFixture {

  implicit val resolver: ICallResolver = NoResolve

  override val code: String =
    """
      |pragma solidity ^0.8.0;
      |
      |contract Foo {
      |
      |    function add(uint256 x, uint256 y) public view returns(uint256) {
      |        return x + y;
      |    }
      |
      |    function main(uint256 argc, string memory argv) public view returns(uint256) {
      |        return add(argc, 3);
      |    }
      |}
      |""".stripMargin

  "should contain a call node for `add` with correct fields" in {

    val List(x) = cpg.call("add").l
    x.code shouldBe "add(argc, 3)"
    x.name shouldBe "add"
    x.order shouldBe 2
    x.methodFullName shouldBe "Foo.add:uint256(uint256,uint256)"
    x.signature shouldBe "uint256(uint256,uint256)"
    x.argumentIndex shouldBe 2
    x.lineNumber shouldBe Some(11)
  }

  "should allow traversing from call to arguments" in {
    cpg.call("add").argument.size shouldBe 2
    val List(arg1) = cpg.call("add").argument(1).l
    arg1.isInstanceOf[nodes.Identifier] shouldBe true
    arg1.asInstanceOf[nodes.Identifier].name shouldBe "argc"
    arg1.code shouldBe "argc"
    arg1.order shouldBe 1
    arg1.argumentIndex shouldBe 1

    val List(arg2) = cpg.call("add").argument(2).l
    arg2.asInstanceOf[nodes.Literal].code shouldBe "3"
    arg2.isInstanceOf[nodes.Literal] shouldBe true
    arg2.code shouldBe "3"
    arg2.order shouldBe 2
    arg2.argumentIndex shouldBe 2
  }

  "should allow traversing from call to surrounding method" in {
    val List(x) = cpg.call.nameExact("add").method.l
    x.name shouldBe "main"
  }

  "should allow traversing from call to callee method" in {
    val List(x) = cpg.call("add").callee.l
    x.name shouldBe "add"
  }

  "should allow traversing from argument to parameter" in {
    val List(x) = cpg.call("add").argument(1).parameter.l
    x.name shouldBe "x"
  }

}
