package io.joern.solidity2cpg.querying

import io.joern.solidity2cpg.testfixtures.SolidityCodeToCpgFixture
import io.shiftleft.semanticcpg.language._

// Note: Type nodes are automatically generated from types stored in Global and from TypeDecl nodes
class TypeTests extends SolidityCodeToCpgFixture {

  override val code: String =
    """ contract Foo {
      |   struct Request {
      |     bytes data;
      |     function(uint) external callback;
      |   }
      |   address public minter;
      |   mapping (address => uint) public balances;
      |
      |   function myFunc(bool param) public pure returns (uint) {
      |     int256 y;
      |     return 1;
      |   }
      | }
      |
      |""".stripMargin

  "should create TYPE node with correct fields for class member" in {
    val List(x) = cpg.typ.name("address").l
    x.name shouldBe "address"
    x.fullName shouldBe "address"
    x.typeDeclFullName shouldBe "address"

    // Don't worry about key-val dynamic types yet
    val List(y) = cpg.typ.name("mapping").l
    y.name shouldBe "mapping"
    y.fullName shouldBe "mapping"
    y.typeDeclFullName shouldBe "mapping"
  }

  "should create TYPE node with correct fields for class struct and its members" in {
    val List(x) = cpg.typ.name("Request").l
    x.name shouldBe "Request"
    x.fullName shouldBe "Foo.Request"
    x.typeDeclFullName shouldBe "Foo.Request"

    val List(y) = cpg.typ.name("bytes").l
    y.name shouldBe "bytes"
    y.fullName shouldBe "bytes"
    y.typeDeclFullName shouldBe "bytes"

    // This may seem weird but makes sense in terms of callback types
    val List(z) = cpg.typ.nameExact("function(uint)").l
    z.name shouldBe "function(uint)"
    z.fullName shouldBe "function(uint)"
    z.typeDeclFullName shouldBe "function(uint)"
  }

  "should create TYPE node with correct fields for return type" in {
    val List(x) = cpg.typ.name("uint").l
    x.name shouldBe "uint"
    x.fullName shouldBe "uint"
    x.typeDeclFullName shouldBe "uint"
  }

  "should create TYPE node with correct fields for parameter type" in {
    val List(x) = cpg.typ.name("bool").l
    x.name shouldBe "bool"
    x.fullName shouldBe "bool"
    x.typeDeclFullName shouldBe "bool"
  }

  "should create TYPE node with correct fields for local type" in {
    val List(x) = cpg.typ.name("int256").l
    x.name shouldBe "int256"
    x.fullName shouldBe "int256"
    x.typeDeclFullName shouldBe "int256"
  }

  "should allow traversing from member's TYPE to member" in {
    val List(x) = cpg.typ("address").memberOfType.l
    x.name shouldBe "minter"
  }

  "should allow traversing from return params TYPE to return param" in {
    val List(x) = cpg.typ("uint").methodReturnOfType.l
    x.typeFullName shouldBe "uint"
  }

  "should allow traversing from params TYPE to param" in {
    val List(x) = cpg.typ("bool").parameterOfType.l
    x.name shouldBe "param"
  }

  "should allow traversing from local's TYPE to local" in {
    val List(x) = cpg.typ("int256").localOfType.l
    x.name shouldBe "y"
  }

}
