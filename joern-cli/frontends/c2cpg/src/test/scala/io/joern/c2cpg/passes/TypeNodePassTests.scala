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
      cpg.typ.name.toSet shouldBe Set("int", "void", "ANY")
    }

    "create correct types for locals" in CpgTypeNodeFixture("""
       |int main() {
       |  char test[1024];
       |}""".stripMargin) { cpg =>
      inside(cpg.local.l) {
        case List(test) =>
          test.typeFullName shouldBe "char[1024]"
          test.evalType.l shouldBe List("char[1024]")
          inside(test.typ.l) {
            case List(t) =>
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
      inside(cpg.call("free").argument(1).evalType.l) {
        case List(tpe) => tpe shouldBe "struct test*"
      }
    }

    "create correct types for arrays" in CpgTypeNodeFixture("""
        |void bad1(size_t a) {
        |  uint8_t src[1], dst[1];
        |  memcpy(dst, src, a);
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.call("memcpy").argument(1).evalType.l) {
        case List(tpe) => tpe shouldBe "uint8_t[1]"
      }
    }
  }

}
