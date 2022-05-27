package io.joern.c2cpg.passes

import io.joern.c2cpg.fixtures.CpgTypeNodeFixture
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class TypeNodePassTests extends AnyWordSpec with Matchers with Inside with CpgTypeNodeFixture {

  "TypeNodePass" should {
    "create TYPE nodes for used types" in CpgTypeNodeFixture("""
        |int main() {
        |  int x;
        |}""".stripMargin) { cpg =>
      cpg.typ.name.toSetMutable shouldBe Set("int", "void", "ANY")
    }

    "create correct types for locals" in CpgTypeNodeFixture("""
       |int main() {
       |  char test[1024];
       |}""".stripMargin) { cpg =>
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

    "create correct types for structs" in CpgTypeNodeFixture("""
        |struct test {
        |  int a;
        |};
        |
        |void free_struct() {
        |  struct test *ptr;
        |  ptr = kzalloc(sizeof(struct test), GFP_KERNEL);
        |  free(ptr);
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.call("free").argument(1).l) { case List(arg) =>
        arg.evalType.l shouldBe List("test")
        arg.code shouldBe "ptr"
        inside(arg.typ.referencedTypeDecl.l) { case List(tpe) =>
          tpe.fullName shouldBe "test"
          tpe.name shouldBe "test"
          tpe.code should startWith("struct test")
        }
        inside(cpg.local.l) { case List(ptr) =>
          ptr.name shouldBe "ptr"
          ptr.typeFullName shouldBe "test"
          ptr.code shouldBe "struct test* ptr"
        }
        inside(cpg.local.typ.referencedTypeDecl.l) { case List(tpe) =>
          tpe.fullName shouldBe "test"
          tpe.name shouldBe "test"
          tpe.code should startWith("struct test")
        }
      }
    }

    "create correct types for arrays" in CpgTypeNodeFixture("""
        |void bad1(size_t a) {
        |  uint8_t src[1], dst[1];
        |  memcpy(dst, src, a);
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.call("memcpy").argument(1).evalType.l) { case List(tpe) =>
        tpe shouldBe "uint8_t[1]"
      }
    }

    "create correct types for locals with struct type" in CpgTypeNodeFixture("""
        |struct Foo {
        |  int x;
        |};
        |
        |int foo() {
        | struct Foo *ptr;
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.local.typ.referencedTypeDecl.l) { case List(tpe) =>
        tpe.fullName shouldBe "Foo"
      }
    }

    "create correct types for identifiers" in CpgTypeNodeFixture("""
       |void test_func() {
       |  char * badChar = malloc(0x100);
       |  free(badChar);
       |  return;
       |}""".stripMargin) { cpg =>
      inside(cpg.call("free").argument(1).isIdentifier.l) { case List(badChar) =>
        badChar.name shouldBe "badChar"
        badChar.typeFullName shouldBe "char"
        inside(badChar.typ.l) { case List(tpe) =>
          tpe.fullName shouldBe "char"
          tpe.name shouldBe "char"
        }
        inside(cpg.method("test_func").ast.isLocal.name(badChar.name).code(".*\\*.*").l) { case List(ptr) =>
          ptr.name shouldBe "badChar"
          ptr.typeFullName shouldBe "char"
          ptr.code shouldBe "char* badChar"
        }
      }
    }
  }

}
