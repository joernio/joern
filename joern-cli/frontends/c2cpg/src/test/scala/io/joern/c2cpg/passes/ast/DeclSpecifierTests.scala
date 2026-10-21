package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.AstC2CpgSuite
import io.shiftleft.semanticcpg.language.*

class DeclSpecifierTests extends AstC2CpgSuite {

  "A typedef with additional declaration specifiers" should {

    "be recognized when a C++ attribute precedes the typedef keyword" in {
      val cpg = code(
        """
          |typedef int plain_t;
          |[[deprecated]] typedef int attr_t;
          |""".stripMargin,
        "test.cpp"
      )
      cpg.typeDecl.nameExact("plain_t").aliasTypeFullName.l shouldBe List("int")
      cpg.typeDecl.nameExact("attr_t").aliasTypeFullName.l shouldBe List("int")
      cpg.local.nameExact("attr_t").l shouldBe empty
    }

    "be recognized when a GNU attribute precedes the typedef keyword in C" in {
      val cpg = code("""
          |typedef int plain_t;
          |__attribute__((deprecated)) typedef int attr_t;
          |""".stripMargin)
      cpg.typeDecl.nameExact("plain_t").aliasTypeFullName.l shouldBe List("int")
      cpg.typeDecl.nameExact("attr_t").aliasTypeFullName.l shouldBe List("int")
      cpg.local.nameExact("attr_t").l shouldBe empty
    }

    "be recognized when the typedef keyword is not the first specifier" in {
      // Declaration specifiers may appear in any order, so `int typedef t;` and
      // `const typedef int t;` are legal C and C++ typedef declarations.
      val cpg = code("""
          |int typedef reordered_t;
          |const typedef int const_first_t;
          |""".stripMargin)
      cpg.typeDecl.nameExact("reordered_t").aliasTypeFullName.l shouldBe List("int")
      cpg.typeDecl.nameExact("const_first_t").aliasTypeFullName.l shouldBe List("int")
      cpg.local.nameExact("reordered_t").l shouldBe empty
      cpg.local.nameExact("const_first_t").l shouldBe empty
    }

    "be recognized for a class member" in {
      val cpg = code(
        """
          |struct S {
          |  typedef int plain_t;
          |  [[deprecated]] typedef int attr_t;
          |};
          |""".stripMargin,
        "test.cpp"
      )
      cpg.typeDecl.nameExact("plain_t").aliasTypeFullName.l shouldBe List("int")
      cpg.typeDecl.nameExact("attr_t").aliasTypeFullName.l shouldBe List("int")
      cpg.member.nameExact("attr_t").l shouldBe empty
    }

    "be recognized when the typedef keyword comes from a macro" in {
      val cpg = code("""
          |#define TYPEDEF_INT(n) typedef int n
          |TYPEDEF_INT(macro_t);
          |""".stripMargin)
      cpg.typeDecl.nameExact("macro_t").aliasTypeFullName.l shouldBe List("int")
      cpg.local.nameExact("macro_t").l shouldBe empty
    }

    "not be assumed for a variable whose type name starts with `typedef`" in {
      val cpg = code("""
          |typedef int typedef_t;
          |typedef_t v;
          |""".stripMargin)
      cpg.local.nameExact("v").typeFullName.l shouldBe List("typedef_t")
      cpg.typeDecl.nameExact("v").l shouldBe empty
    }
  }

  "The static modifier" should {

    "be set when a C++ attribute precedes the static keyword" in {
      val cpg = code(
        """
          |struct S {
          |  static int plain() { return 1; }
          |  [[nodiscard]] static int attr() { return 2; }
          |};
          |""".stripMargin,
        "test.cpp"
      )
      cpg.method.nameExact("plain").isStatic.size shouldBe 1
      cpg.method.nameExact("attr").isStatic.size shouldBe 1
      // a static member function has no implicit `this`
      cpg.method.nameExact("attr").parameter.l shouldBe empty
    }

    "be set when another specifier precedes the static keyword" in {
      val cpg = code(
        """
          |struct S {
          |  constexpr static int cexpr() { return 1; }
          |  inline static int inl() { return 2; }
          |};
          |""".stripMargin,
        "test.cpp"
      )
      cpg.method.nameExact("cexpr").isStatic.size shouldBe 1
      cpg.method.nameExact("inl").isStatic.size shouldBe 1
      cpg.method.nameExact("cexpr").parameter.l shouldBe empty
      cpg.method.nameExact("inl").parameter.l shouldBe empty
    }

    "be set when a GNU attribute precedes the static keyword in C" in {
      val cpg = code("""
          |__attribute__((unused)) static void attrDef() {}
          |__attribute__((unused)) static void attrDecl(void);
          |inline static void inlineDef() {}
          |""".stripMargin)
      cpg.method.nameExact("attrDef").isStatic.size shouldBe 1
      cpg.method.nameExact("attrDecl").isStatic.size shouldBe 1
      cpg.method.nameExact("inlineDef").isStatic.size shouldBe 1
    }

    "be set when the static keyword comes from a macro" in {
      val cpg = code("""
          |#define PRIVATE static
          |PRIVATE void macroStatic(void) {}
          |""".stripMargin)
      cpg.method.nameExact("macroStatic").isStatic.size shouldBe 1
    }
  }

}
