package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.proto.cpg.Cpg.DispatchTypes
import io.shiftleft.semanticcpg.language._

/** These tests are based off of those found in javasrc2cpg but modified to fit to Jimple's 3-address code rule and flat
  * AST.
  */
class ConstructorInvocationTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """
      |pragma solidity ^0.8.0;
      |
      |contract Foo {
      |    uint256  x;
      |    constructor (uint256 _x) {
      |        x = _x;
      |    }
      |
      |    function getValue() public virtual view returns(uint256){
      |        return x;
      |    }
      |
      |}
      |
      |
      |contract Bar is Foo {
      |    constructor(uint256 _x) Foo( _x) {
      |        Foo.x = _x;
      |    }
      |// Solidity can only have 1 instance of a constructor
      |
      |    function getValue() public override view returns (uint256) {
      |        return Foo.x;
      |    }
      |
      |}
      |
      |contract Baz {
      |    constructor () {
      |        Bar b = new Bar(4);
      |    }
      |}
      |""".stripMargin

  "it should create correct method nodes for constructors" in {
    cpg.method.name("<init>").where(_.fullName("^Foo.*")).l match {
      case List(cons: Method) =>
        cons.fullName shouldBe "Foo.<init>:void(uint256)"
        cons.signature shouldBe "void(uint256)"
        cons.code shouldBe "constructor(uint256 _x)"
        cons.parameter.size shouldBe 2
        val objParam = cons.parameter.index(0).head
        objParam.name shouldBe "this"
        objParam.typeFullName shouldBe "Foo"
        val otherParam = cons.parameter.index(1).head
        otherParam.name shouldBe "_x"
        otherParam.typeFullName shouldBe "uint256"
        otherParam.dynamicTypeHintFullName shouldBe Seq()

      case res =>
        fail(s"Expected single Foo constructor, but got $res")
    }

    cpg.method.name("<init>").where(_.fullName("^Bar.*")).l match {
      case List(cons1: Method) =>
        cons1.fullName shouldBe "Bar.<init>:void(uint256)"
        cons1.signature shouldBe "void(uint256)"
        cons1.code shouldBe "constructor(uint256 _x)"
        cons1.parameter.size shouldBe 2
        cons1.parameter.index(0).head.name shouldBe "this"
        cons1.parameter.index(1).head.name shouldBe "_x"
        cons1.modifier.size shouldBe 1
        cons1.modifier.modifierType.next() shouldBe "Foo"
        cons1.modifier.code.next() shouldBe "Foo (_x)"
      case res =>
        fail(s"Expected 2 Bar constructors, but got $res")
    }
  }

}
