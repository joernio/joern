package io.joern.c2cpg.passes.types

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language._

class StructTypeTests extends CCodeToCpgSuite {

  "Struct with array members" should {
    val cpg = code("""
        |#define BYTES_COUNT 5
        |struct Foo {
        |  unsigned char id[BYTES_COUNT];
        |  unsigned char extended[BYTES_COUNT - 2];
        |  unsigned char data[20];
        |};
        |""".stripMargin)

    "contain correct fields for all members" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.member.name.toSetImmutable shouldBe Set("id", "extended", "data")
      typeDeclNode.member.code.toSetImmutable shouldBe Set("id[BYTES_COUNT]", "extended[BYTES_COUNT - 2]", "data[20]")
    }

    "initialize array members correctly" in {
      val List(fakeInitMethod)                        = cpg.method.nameExact("<sinit>").l
      val List(idInitCall, extInitCall, dataInitCall) = fakeInitMethod.call.nameExact(Operators.arrayInitializer).l

      idInitCall.code shouldBe "id[BYTES_COUNT]"
      val List(argIdInit) = idInitCall.argument.l
      argIdInit.code shouldBe "BYTES_COUNT"

      extInitCall.code shouldBe "extended[BYTES_COUNT - 2]"
      val List(argExtInit) = extInitCall.argument.l
      argExtInit.code shouldBe "BYTES_COUNT - 2"
      val List(subtractionCall) = extInitCall.ast.isCall.nameExact(Operators.subtraction).l
      subtractionCall.code shouldBe "5 - 2"

      dataInitCall.code shouldBe "data[20]"
      val List(argDataInit) = dataInitCall.argument.l
      argDataInit.code shouldBe "20"
    }

  }

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
