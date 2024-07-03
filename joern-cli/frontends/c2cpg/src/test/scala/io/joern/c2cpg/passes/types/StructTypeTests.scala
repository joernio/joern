package io.joern.c2cpg.passes.types

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class StructTypeTests extends C2CpgSuite {

  "Typedef struct with member" should {
    val cpg = code("""
        |typedef struct {
        |  uint32_t bar;
        |} Foo;
        |""".stripMargin)

    "contain correct fields for all members" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.member.name.toSetImmutable shouldBe Set("bar")
      typeDeclNode.member.typ.fullName.toSetImmutable shouldBe Set("uint32_t")
    }
  }

  "Struct with array members" should {
    val cpg = code("""
        |#define SIZE 5
        |struct Foo {
        |  char a[SIZE];
        |  char b[SIZE - 1];
        |  char c[10];
        |};
        |""".stripMargin)

    "contain correct fields for all members" in {
      val List(typeDeclNode) = cpg.typeDecl.nameExact("Foo").l
      typeDeclNode.member.name.toSetImmutable shouldBe Set("a", "b", "c")
      typeDeclNode.member.code.toSetImmutable shouldBe Set("a[SIZE]", "b[SIZE - 1]", "c[10]")
    }

    "initialize array members correctly" in {
      val List(clInitMethod)                    = cpg.method.nameExact(io.joern.x2cpg.Defines.StaticInitMethodName).l
      val List(aInitCall, bInitCall, cInitCall) = clInitMethod.call.nameExact(Operators.arrayInitializer).l

      aInitCall.code shouldBe "a[SIZE]"
      val List(argAInit) = aInitCall.argument.l
      argAInit.code shouldBe "SIZE"

      bInitCall.code shouldBe "b[SIZE - 1]"
      val List(argBInit) = bInitCall.argument.l
      argBInit.code shouldBe "SIZE - 1"
      val List(subtractionCall) = bInitCall.ast.isCall.nameExact(Operators.subtraction).l
      subtractionCall.code shouldBe "SIZE - 1"

      cInitCall.code shouldBe "c[10]"
      val List(argCInit) = cInitCall.argument.l
      argCInit.code shouldBe "10"
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
