package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language._

class MemberTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """// SPDX-License-Identifier: GPL-3.0
      |pragma solidity ^0.8.4;
      |
      |contract Coin {
      |
      |   address public minter;
      |
      |   mapping (address => uint) public balances;
      |
      |   uint private storedData;
      |
      |}
      |""".stripMargin

  private def hasMod(member: Member, modifierType: String): Boolean = {
    member.modifier.exists(_.modifierType == modifierType)
  }

  "should contain MEMBER node with correct properties" in {
    val List(x, y, z) = cpg.member.l
    x.name shouldBe "minter"
    x.code shouldBe "address public minter"
    x.typeFullName shouldBe "address"
    x.order shouldBe 2
    hasMod(x, ModifierTypes.PUBLIC)

    y.name shouldBe "balances"
    y.code shouldBe "mapping (address => uint) public balances"
    y.typeFullName shouldBe "mapping (address => uint)" // don't worry about dynamic key-val types for now
    y.order shouldBe 3
    hasMod(y, ModifierTypes.PUBLIC)

    z.name shouldBe "storedData"
    z.code shouldBe "uint private storedData"
    z.typeFullName shouldBe "uint"
    z.order shouldBe 4
    hasMod(z, ModifierTypes.PRIVATE)
  }

  "should allow traversing from MEMBER to TYPE_DECL" in {
    val x = cpg.member.typeDecl.l
    x.map(x => {
      x.name shouldBe "Coin"
    })

  }
}
