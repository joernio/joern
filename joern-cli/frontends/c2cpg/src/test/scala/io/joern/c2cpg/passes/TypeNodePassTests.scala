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
  }

}
