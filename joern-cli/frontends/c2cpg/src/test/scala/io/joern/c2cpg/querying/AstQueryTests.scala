package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

class AstQueryTests extends C2CpgSuite {

  private implicit val resolver: ICallResolver = NoResolve

  "querying the AST (code example 1)" should {
    val cpg = code("""
      | int foo(int y) {
      |   int x = 10;
      |   if (x > 10) {
      |     moo(boo(1+2));
      |     return bar(x + 10);
      |   } else {
      |     if (y > x) {
      |       printf("reached");
      |     }
      |   }
      | }
    """.stripMargin)

    "identify four blocks" in {
      cpg.method.name("foo").ast.isBlock.l.size shouldBe 4
    }

    "allow finding addition in argument to bar" in {
      cpg.method
        .name("bar")
        .callIn
        .argument(1)
        .containsCallTo("<operator>.(addition|multiplication)")
        .code
        .l shouldBe List("x + 10")
    }

    "allow finding that addition is not a direct argument of moo" in {
      cpg.method
        .name("moo")
        .callIn
        .argument(1)
        .containsCallTo("<operator>.(addition|multiplication)")
        .code
        .l shouldBe List("boo(1+2)")

      cpg.method
        .name("moo")
        .callIn
        .argument(1)
        .filter(arg =>
          arg.ast
            .isCallTo("<operator>.(addition|multiplication)")
            .not(_.inAstMinusLeaf(arg).isCall)
            .l
            .nonEmpty
        )
        .code
        .l shouldBe List()
    }

    "identify three control structures" in {
      cpg.method
        .name("foo")
        .ast
        .isControlStructure
        .isIf
        .l
        .size shouldBe 2

      cpg.method
        .name("foo")
        .ast
        .isControlStructure
        .isElse
        .l
        .size shouldBe 1
    }

    "allow basic calling basic 'is' methods on AST node" in {
      cpg.method.name("foo").ast.isFile.l.size shouldBe 0
      cpg.method.name("foo").ast.isMember.l.size shouldBe 0
      cpg.method.name("foo").ast.isModifier.l.size shouldBe 0
      cpg.method.name("foo").ast.isNamespaceBlock.l.size shouldBe 0
      cpg.method.name("foo").ast.isParameter.l.size shouldBe 1
      cpg.method.name("foo").ast.isTypeDecl.l.size shouldBe 0
    }

    "identify conditions" in {
      cpg.method.name("foo").controlStructure.condition.code.l shouldBe List("x > 10", "y > x")
    }

    "allow parserTypeName filtering and then ast" in {
      val query1Size = cpg.method.name("foo").ast.isControlStructure.ast.l.size
      query1Size should be > 0

      val query2Size = cpg.method
        .name("foo")
        .ast
        .isControlStructure
        .parserTypeName(".*")
        .ast
        .l
        .size
      query1Size shouldBe query2Size
    }

    "allow filtering on control structures" in {
      cpg.method
        .name("foo")
        .controlStructure(".*x > 10.*")
        .l
        .size shouldBe 1

      cpg.method
        .name("foo")
        .controlStructure(".*x > 10.*")
        .whenTrue
        .ast
        .isReturn
        .code
        .l shouldBe List("return bar(x + 10);")

      cpg.method
        .name("foo")
        .controlStructure(".*x > 10.*")
        .whenFalse
        .ast
        .isCall
        .code(".*printf.*")
        .code
        .l shouldBe List("printf(\"reached\")")
    }

    "astSiblings" in {
      val local       = cpg.local.code("int x").head
      val astSiblings = local.astSiblings.toSet

      val assignment = cpg.call.code("x = 10").head
      val condition  = cpg.controlStructure.code(".*(x > 10).*").head

      astSiblings should contain(assignment)
      astSiblings should contain(condition)
      astSiblings should not contain local
      astSiblings should have size 2
    }
  }

  "querying the AST (code example 2)" should {
    val cpg = code("""
        |void foo(int bar) {
        | char buf[10];
        | int i;
        | for (int i = 0;i < bar;i++) {
        |   buf[i] = 42;
        | }
        |}
    """.stripMargin)

    "find index `i` used for buf" in {
      cpg.call
        .name("<operator>.indirectIndexAccess")
        .argument
        .argumentIndex(2)
        .code
        .l shouldBe List("i")
    }

    "find that i is assigned as part of loop header" in {
      cpg.call
        .name("<operator>.indirectIndexAccess")
        .argument
        .argumentIndex(2)
        .inAstMinusLeaf
        .isControlStructure
        .code
        .l shouldBe List("for (int i = 0;i < bar;i++)")

    }

    "correctly identify condition of for loop" in {
      cpg.call
        .name("<operator>.indirectIndexAccess")
        .argument
        .argumentIndex(2)
        .inAstMinusLeaf
        .isControlStructure
        .condition
        .code
        .l shouldBe List("i < bar")
    }
  }

}
