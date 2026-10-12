package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, TypeRef}
import io.shiftleft.semanticcpg.language.*
import org.scalactic.source.Position

class QualifiedNameReceiverTests extends C2CpgSuite {

  "qualified-ids with a namespace or typedef qualifier" should {
    val cpg = code(
      """
        |namespace app { int limit = 1; namespace in { int deep = 2; } }
        |namespace fs = app::in;
        |namespace fs2 = fs;
        |struct Cfg { static int limit; };
        |typedef Cfg CfgT;
        |using CfgU = Cfg;
        |template<typename T> int tv() { return T::value; }
        |int probe() {
        |  int a = app::limit;
        |  int b = app::in::deep;
        |  int c = CfgT::limit;
        |  int d = CfgU::limit;
        |  int e = ::app::limit;
        |  int f = fs::deep;
        |  int* p = &Cfg::limit;
        |  int g = fs2::deep;
        |  return a + b + c + d + e + f + g + *p;
        |}
        |""".stripMargin,
      "test.cpp"
    )

    def receiver(codeStr: String)(implicit pos: Position): TypeRef = {
      inside(cpg.call.nameExact(Operators.fieldAccess).codeExact(codeStr).l) { case List(fa) =>
        inside(fa.argument.l) { case List(owner: TypeRef, _: FieldIdentifier) => owner }
      }
    }

    "not fabricate locals or identifiers for the qualifier" in {
      cpg.method.nameExact("probe").local.name.l.sorted shouldBe List("a", "b", "c", "d", "e", "f", "g", "p")
      cpg.method.nameExact("tv").local.size shouldBe 0
      cpg.method
        .nameExact("probe", "tv")
        .ast
        .isIdentifier
        .nameExact("app", "CfgT", "CfgU", "fs", "fs2", "T", "in")
        .size shouldBe 0
    }

    "use a TYPE_REF for a namespace" in {
      val owner = receiver("app::limit")
      owner.typeFullName shouldBe "app"
      owner.code shouldBe "app"
    }

    "use a TYPE_REF for the innermost namespace of a nested qualifier" in {
      val owner = receiver("app::in::deep")
      owner.typeFullName shouldBe "app.in"
      owner.code shouldBe "app::in"
      cpg.call.nameExact(Operators.fieldAccess).codeExact("app::in").size shouldBe 0
    }

    "use a TYPE_REF for a globally qualified namespace" in {
      receiver("::app::limit").typeFullName shouldBe "app"
    }

    "resolve a namespace alias to its target" in {
      receiver("fs::deep").typeFullName shouldBe "app.in"
    }

    "resolve a chain of namespace aliases to the final target" in {
      receiver("fs2::deep").typeFullName shouldBe "app.in"
    }

    "use a TYPE_REF for a typedef and an alias-declaration" in {
      receiver("CfgT::limit").typeFullName shouldBe "CfgT"
      receiver("CfgU::limit").typeFullName shouldBe "CfgU"
    }

    "use a TYPE_REF for a template type parameter" in {
      receiver("T::value").typeFullName shouldBe "T"
    }

    "keep a TYPE_REF for a class" in {
      receiver("Cfg::limit").typeFullName shouldBe "Cfg"
    }
  }

  "qualified-ids whose qualifier cannot be resolved" should {
    val cpg = code(
      """
        |namespace known { }
        |int probe() {
        |  int a = missing::value;
        |  int b = known::missing::value;
        |  return a + b;
        |}
        |""".stripMargin,
      "test.cpp"
    )

    "not fail and still yield field accesses" in {
      cpg.method.nameExact("probe").call.nameExact(Operators.fieldAccess).codeExact("missing::value").size shouldBe 1
      cpg.method
        .nameExact("probe")
        .call
        .nameExact(Operators.fieldAccess)
        .codeExact("known::missing::value")
        .size shouldBe 1
    }
  }

}
