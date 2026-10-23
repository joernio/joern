package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.NodeTypes
import io.shiftleft.semanticcpg.language.*

class FunctionLocalTypeTests extends C2CpgSuite {

  "a class defined inside a function body" should {
    val cpg = code(
      """
        |void memcpy(char* dst, const char* src, int n);
        |
        |struct AtFileScope {
        |  char buf[8];
        |  void copyIn(const char* src, int n) { memcpy(buf, src, n); }
        |};
        |
        |void f(const char* src, int n) {
        |  struct Local {
        |    char buf[8];
        |    void copyIn(const char* s, int len) { memcpy(buf, s, len); }
        |  };
        |  enum LocalEnum { A, B };
        |  union LocalUnion { int i; char c[4]; };
        |  Local l;
        |  l.copyIn(src, n);
        |  LocalEnum e = A;
        |}
        |""".stripMargin,
      "test.cpp"
    )

    "be the one internal TYPE_DECL for that type" in {
      inside(cpg.typeDecl.nameExact("Local").l) { case List(local) =>
        local.isExternal shouldBe false
        local.fullName shouldBe "Local"
        local.filename shouldBe "test.cpp"
      }
    }

    "carry its members" in {
      cpg.typeDecl.nameExact("Local").member.name.l shouldBe List("buf")
    }

    "carry its member function, with a body" in {
      inside(cpg.typeDecl.nameExact("Local").method.fullNameExact("Local.copyIn:void(char*,int)").l) {
        case List(copyIn) =>
          copyIn.isExternal shouldBe false
          copyIn.body.astChildren.size should be > 0
          copyIn.local.nameExact("buf").size shouldBe 1
      }
    }

    "make the code inside that member function body reachable" in {
      cpg.call.nameExact("memcpy").method.fullName.l.sorted shouldBe
        List("AtFileScope.copyIn:void(char*,int)", "Local.copyIn:void(char*,int)")
    }

    "hang the TYPE_DECL off the enclosing function body, just like a function-local enum" in {
      val body = cpg.method.nameExact("f").body.loneElement
      cpg.typeDecl.nameExact("Local").astParent.l shouldBe List(body)
      cpg.typeDecl.nameExact("LocalEnum").astParent.l shouldBe List(body)
      cpg.typeDecl.nameExact("LocalUnion").astParent.l shouldBe List(body)
    }

    "model a function-local union the same way" in {
      inside(cpg.typeDecl.nameExact("LocalUnion").l) { case List(localUnion) =>
        localUnion.isExternal shouldBe false
        localUnion.member.name.l shouldBe List("i", "c")
      }
    }

    "not change the function-local enum" in {
      inside(cpg.typeDecl.nameExact("LocalEnum").l) { case List(localEnum) =>
        localEnum.isExternal shouldBe false
        localEnum.filename shouldBe "test.cpp"
        localEnum.member.name.l shouldBe List("A", "B")
      }
    }

    "not change a file-scope class of the same shape" in {
      inside(cpg.typeDecl.nameExact("AtFileScope").l) { case List(atFileScope) =>
        atFileScope.isExternal shouldBe false
        atFileScope.filename shouldBe "test.cpp"
        atFileScope.member.name.l shouldBe List("buf")
        atFileScope.method.fullName.l should contain("AtFileScope.copyIn:void(char*,int)")
        atFileScope.astParent.label shouldBe NodeTypes.METHOD
      }
    }
  }

  "a struct defined inside a function body in C" should {
    val cpg = code("""
        |struct AtFileScope { int a; };
        |void f(void) {
        |  struct Local { int b; };
        |  enum LocalEnum { A };
        |  struct Local l;
        |  enum LocalEnum e = A;
        |  l.b = 1;
        |}
        |""".stripMargin)

    "be modelled with its members, scoped exactly like a function-local enum" in {
      val body = cpg.method.nameExact("f").body.loneElement
      inside(cpg.typeDecl.internal.nameExact("Local").l) { case List(local) =>
        local.fullName shouldBe "f.Local"
        local.member.name.l shouldBe List("b")
        local.astParent shouldBe body
      }
      inside(cpg.typeDecl.internal.nameExact("LocalEnum").l) { case List(localEnum) =>
        localEnum.fullName shouldBe "f.LocalEnum"
        localEnum.member.name.l shouldBe List("A")
        localEnum.astParent shouldBe body
      }
    }

    "not change a file-scope struct" in {
      inside(cpg.typeDecl.nameExact("AtFileScope").l) { case List(atFileScope) =>
        atFileScope.isExternal shouldBe false
        atFileScope.member.name.l shouldBe List("a")
        atFileScope.astParent.label shouldBe NodeTypes.METHOD
      }
    }
  }

}
