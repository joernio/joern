package io.joern.c2cpg.passes.types

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class TypeNodePassTests extends C2CpgSuite {

  "TypeNodePass" should {

    "be correct for top level type definitions" in {
      val cpg = code("""
          |typedef const char * foo;
          |typedef foo * bar;
          |""".stripMargin)
      val List(foo) = cpg.typeDecl.nameExact("foo").l
      val List(bar) = cpg.typeDecl.nameExact("bar").l
      foo.aliasTypeFullName shouldBe Option("char*")
      bar.aliasTypeFullName shouldBe Option("char**")
    }

    "be correct for reference to type" in {
      val cpg = code(
        """
          |typedef const char (&TwoChars)[2];
          |""".stripMargin,
        "twochars.cpp"
      )
      val List(bar) = cpg.typeDecl.nameExact("TwoChars").l
      bar.fullName shouldBe "TwoChars"
      bar.aliasTypeFullName shouldBe Option("char(&)[2]")
    }

    "be correct for static decl assignment" in {
      val cpg = code("""
          |void method() {
          |  static int local = 1;
          |}
          |""".stripMargin)
      inside(cpg.method.name("method").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        call.name shouldBe Operators.assignment
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "be correct for const decl assignment" in {
      val cpg = code("""
        |void method() {
        |  const int local = 1;
        |}
        |""".stripMargin)
      inside(cpg.method.name("method").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        call.name shouldBe Operators.assignment
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "be correct for static const decl assignment" in {
      val cpg = code("""
        |void method() {
        |  static const int local = 1;
        |}
        |""".stripMargin)
      inside(cpg.method.name("method").block.astChildren.l) { case List(local: Local, call: Call) =>
        local.name shouldBe "local"
        local.typeFullName shouldBe "int"
        call.name shouldBe Operators.assignment
        inside(call.astChildren.l) { case List(identifier: Identifier, literal: Literal) =>
          identifier.name shouldBe "local"
          identifier.typeFullName shouldBe "int"
          identifier.order shouldBe 1
          identifier.argumentIndex shouldBe 1
          literal.code shouldBe "1"
          literal.typeFullName shouldBe "int"
          literal.order shouldBe 2
          literal.argumentIndex shouldBe 2
        }
      }
    }

    "create TYPE nodes for used types" in {
      val cpg = code("""
        |int main() {
        |  int x;
        |}""".stripMargin)
      cpg.typ.name.toSetMutable shouldBe Set("int", "void", "ANY")
    }

    "create correct types for locals" in {
      val cpg = code("""
       |int main() {
       |  char test[1024];
       |}""".stripMargin)
      inside(cpg.local.l) { case List(test) =>
        test.typeFullName shouldBe "char[1024]"
        test.evalType.l shouldBe List("char[1024]")
        inside(test.typ.l) { case List(t) =>
          t.name shouldBe "char[1024]"
          t.fullName shouldBe "char[1024]"
          t.typeDeclFullName shouldBe "char[1024]"
        }
      }
    }

    "create correct types for structs" in {
      val cpg = code("""
        |struct test {
        |  int a;
        |};
        |
        |void free_struct() {
        |  struct test *ptr;
        |  ptr = kzalloc(sizeof(struct test), GFP_KERNEL);
        |  free(ptr);
        |}
        |""".stripMargin)
      inside(cpg.call("free").argument(1).l) { case List(arg) =>
        arg.evalType.l shouldBe List("test*")
        arg.code shouldBe "ptr"
        inside(arg.typ.referencedTypeDecl.l) { case List(tpe) =>
          tpe.fullName shouldBe "test*"
          tpe.name shouldBe "test*"
        }
        inside(cpg.local.l) { case List(ptr) =>
          ptr.name shouldBe "ptr"
          ptr.typeFullName shouldBe "test*"
          ptr.code shouldBe "struct test* ptr"
        }
        inside(cpg.local.typ.referencedTypeDecl.l) { case List(tpe) =>
          tpe.name shouldBe "test*"
          tpe.fullName shouldBe "test*"
        }
      }
    }

    "create correct types for arrays" in {
      val cpg = code("""
        |void bad1(size_t a) {
        |  uint8_t src[1], dst[1];
        |  memcpy(dst, src, a);
        |}
        |""".stripMargin)
      inside(cpg.call("memcpy").argument(1).evalType.l) { case List(tpe) =>
        tpe shouldBe "uint8_t[1]"
      }
    }

    "create correct types for locals with struct type" in {
      val cpg = code("""
        |struct Foo {
        |  int x;
        |};
        |
        |int foo() {
        | struct Foo *ptr;
        |}
        |""".stripMargin)
      inside(cpg.local.typ.referencedTypeDecl.l) { case List(tpe) =>
        tpe.fullName shouldBe "Foo*"
      }
    }

    "create correct types for identifiers" in {
      val cpg = code("""
       |void test_func() {
       |  char * badChar = malloc(0x100);
       |  free(badChar);
       |  return;
       |}""".stripMargin)
      inside(cpg.call("free").argument(1).isIdentifier.l) { case List(badChar) =>
        badChar.name shouldBe "badChar"
        badChar.typeFullName shouldBe "char*"
        inside(badChar.typ.l) { case List(tpe) =>
          tpe.fullName shouldBe "char*"
          tpe.name shouldBe "char*"
        }
        inside(cpg.method("test_func").ast.isLocal.name(badChar.name).code(".*\\*.*").l) { case List(ptr) =>
          ptr.name shouldBe "badChar"
          ptr.typeFullName shouldBe "char*"
          ptr.code shouldBe "char* badChar"
        }
      }
    }

    "be correct for volatile types" in {
      val cpg = code("""
          |void func(void) {
          |  static volatile int **ipp;
          |  static int *ip;
          |  static volatile int i = 0;
          |
          |  ipp = &ip;
          |  ipp = (int**) &ip;
          |  *ipp = &i;
          |  if (*ip != 0) {}
          |}""".stripMargin)
      cpg.identifier.nameExact("ipp").typeFullName.distinct.l shouldBe List("volatile int**")
      cpg.identifier.nameExact("ip").typeFullName.distinct.l shouldBe List("int*")
      cpg.identifier.nameExact("i").typeFullName.distinct.l shouldBe List("volatile int")
      cpg.local.nameExact("ipp").typeFullName.l shouldBe List("volatile int**")
      cpg.local.nameExact("ip").typeFullName.l shouldBe List("int*")
      cpg.local.nameExact("i").typeFullName.l shouldBe List("volatile int")
    }

    "be correct for referenced types from locals" in {
      val cpg = code("""
          |struct flex {
          |  int a;
          |  char b[];
          |};
          |void foo() {
          |  struct flex *ptr = malloc(sizeof(struct flex));
          |  struct flex value = {0};
          |}""".stripMargin)
      val List(value) = cpg.typeDecl.fullNameExact("flex").referencingType.fullNameExact("flex").localOfType.l
      value.name shouldBe "value"
      value.typeFullName shouldBe "flex"
      value.code shouldBe "struct flex value"
    }
  }

}
