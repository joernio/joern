package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class TypeTests extends FuzzyCCodeToCpgSuite {

  override val code =
    """
      | class Foo {
      |  member_type x;
      | };
      |
      | ret_type myFunc(param_type param) {
      |   local_type y;
      | }
      |""".stripMargin

  "should create TYPE node with correct fields for class member" in {
    val List(x) = cpg.typ.name("member_type").l
    x.name shouldBe "member_type"
    x.fullName shouldBe "member_type"
    x.typeDeclFullName shouldBe "member_type"
  }

  "should create TYPE node with correct fields for return type" in {
    val List(x) = cpg.typ.name("ret_type").l
    x.name shouldBe "ret_type"
    x.fullName shouldBe "ret_type"
    x.typeDeclFullName shouldBe "ret_type"
  }

  "should create TYPE node with correct fields for parameter type" in {
    val List(x) = cpg.typ.name("param_type").l
    x.name shouldBe "param_type"
    x.fullName shouldBe "param_type"
    x.typeDeclFullName shouldBe "param_type"
  }

  "should create TYPE node with correct fields for local type" in {
    val List(x) = cpg.typ.name("local_type").l
    x.name shouldBe "local_type"
    x.fullName shouldBe "local_type"
    x.typeDeclFullName shouldBe "local_type"
  }

  "should allow traversing from member's TYPE to member" in {
    val List(x) = cpg.typ("member_type").memberOfType.l
    x.name shouldBe "x"
  }

  "should allow traversing from return params TYPE to return param" in {
    val List(x) = cpg.typ("ret_type").methodReturnOfType.l
    x.typeFullName shouldBe "ret_type"
  }

  "should allow traversing from params TYPE to param" in {
    val List(x) = cpg.typ("param_type").parameterOfType.l
    x.name shouldBe "param"
  }

  "should allow traversing from local's TYPE to local" in {
    val List(x) = cpg.typ("local_type").localOfType.l
    x.name shouldBe "y"
  }

}
