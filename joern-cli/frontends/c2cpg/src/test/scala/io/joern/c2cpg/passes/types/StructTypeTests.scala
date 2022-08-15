package io.joern.c2cpg.passes.types

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class StructTypeTests extends CCodeToCpgSuite {

  "Type decl test project" should {
    val cpg = code("""
        |struct Foo {
        |    int x, y;
        |    char *foo;
        |};
        |""".stripMargin)

    "contain one internal type decl node for Foo" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.isExternal shouldBe false
    }

    "contain three member nodes" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.member.size shouldBe 3
    }

    "contain edges from Foo to three members" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.astChildren.isMember.size shouldBe 3
    }

    "contain correct code fields for all members" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.member.code.toSetImmutable shouldBe Set("x", "y", "*foo")
    }
  }

}
