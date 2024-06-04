package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language._

class TypeTests extends JimpleCode2CpgFixture {

  lazy val cpg: Cpg = code("""
      | package foo;
      |
      | class Foo {
      |   Long x;
      |
      |   Integer myFunc(Object param) {
      |     Double y;
      |     return 1;
      |   }
      | }
      |""".stripMargin)

  "should create TYPE node with correct fields for class member" in {
    val List(x) = cpg.typ.name("Long").l
    x.name shouldBe "Long"
    x.fullName shouldBe "java.lang.Long"
    x.typeDeclFullName shouldBe "java.lang.Long"
  }

  "should create TYPE node with correct fields for return type" in {
    val List(x) = cpg.typ.name("Integer").l
    x.name shouldBe "Integer"
    x.fullName shouldBe "java.lang.Integer"
    x.typeDeclFullName shouldBe "java.lang.Integer"
  }

  "should create TYPE node with correct fields for parameter type" in {
    val List(x) = cpg.typ.name("Object").l
    x.name shouldBe "Object"
    x.fullName shouldBe "java.lang.Object"
    x.typeDeclFullName shouldBe "java.lang.Object"
  }

//  "should create TYPE node with correct fields for local type" in {
//    val List(x) = cpg.typ.name("Double").l
//    x.name shouldBe "Double"
//    x.fullName shouldBe "java.lang.Double"
//    x.typeDeclFullName shouldBe "java.lang.Double"
//  }

  "should allow traversing from member's TYPE to member" in {
    val List(x) = cpg.typ("Long").memberOfType.l
    x.name shouldBe "x"
  }

  "should allow traversing from return params TYPE to return param" in {
    val List(x) = cpg.typ.fullName("java.lang.Integer").methodReturnOfType.l
    x.typeFullName shouldBe "java.lang.Integer"
  }

  "should allow traversing from params TYPE to param" in {
    val List(x) = cpg.typ("Object").parameterOfType.l
    x.name shouldBe "param"
  }

//  "should allow traversing from local's TYPE to local" in {
//    val List(x) = cpg.typ("java.lang.Double").localOfType.l
//    x.name shouldBe "y"
//  }

}
