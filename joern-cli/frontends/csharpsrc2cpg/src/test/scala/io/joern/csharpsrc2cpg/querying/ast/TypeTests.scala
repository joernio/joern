package io.joern.csharpsrc2cpg.querying.ast

import io.joern.csharpsrc2cpg.testfixtures.CSharpCode2CpgFixture
import io.shiftleft.semanticcpg.language.*

class TypeTests extends CSharpCode2CpgFixture {
  "type resolution for nullable types" should {

    "resolve types for primitive type identifiers" in {
      val cpg = code(basicBoilerplate("""
          |int? a = 10;
          |string? b = "Foo";
          |""".stripMargin))

      inside(cpg.identifier.nameExact("a").l) {
        case a :: Nil =>
          a.typeFullName shouldBe "System.Int32"
        case _ => fail("Identifier named `a` not found")
      }

      inside(cpg.identifier.nameExact("b").l) {
        case a :: Nil =>
          a.typeFullName shouldBe "System.String"
        case _ => fail("Identifier named `b` not found")
      }
    }

    "resolve types for custom type identifiers" in {
      val cpg = code("""
          |namespace Foo {
          | public class Bar {}
          | public class Baz {
          |   static void mBaz() {
          |     Bar? iBar = new Bar();
          |   }
          | }
          |}
          |""".stripMargin)

      inside(cpg.identifier.nameExact("iBar").l) {
        case iBar :: Nil =>
          iBar.typeFullName shouldBe "Foo.Bar"
        case _ => fail("Identifier named `iBar` not found")
      }
    }

  }
}
