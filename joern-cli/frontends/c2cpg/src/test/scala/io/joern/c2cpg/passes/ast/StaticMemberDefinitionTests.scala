package io.joern.c2cpg.passes.ast

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Literal, TypeRef}
import io.shiftleft.semanticcpg.language.*

class StaticMemberDefinitionTests extends C2CpgSuite {

  "an out-of-class static data member definition with initializer" should {
    val cpg = code(
      """
        |struct Config { static int limit; };
        |int Config::limit = 4096;
        |int get() { return Config::limit; }
        |""".stripMargin,
      "test.cpp"
    )

    "be an assignment to a field access on a TYPE_REF" in {
      inside(cpg.assignment.codeExact("Config::limit = 4096").l) { case List(assignment) =>
        inside(assignment.argument.l) { case List(target: Call, value: Literal) =>
          value.code shouldBe "4096"
          target.name shouldBe Operators.fieldAccess
          inside(target.argument.l) { case List(owner: TypeRef, member: FieldIdentifier) =>
            owner.typeFullName shouldBe "Config"
            owner.code shouldBe "Config"
            member.canonicalName shouldBe "limit"
          }
        }
      }
    }

    "have the same shape for the definition and a use" in {
      inside(cpg.method("get").call.nameExact(Operators.fieldAccess).l) { case List(use) =>
        inside(use.argument.l) { case List(owner: TypeRef, _: FieldIdentifier) =>
          owner.typeFullName shouldBe "Config"
        }
      }
    }

    "not create locals for the qualifier or the member" in {
      cpg.local.nameExact("limit") shouldBe empty
      cpg.local.nameExact("Config") shouldBe empty
    }
  }

  "all out-of-class forms" should {
    val cpg = code(
      """
        |struct S1 { static int sc; };
        |int S1::sc = 1;
        |struct S2 { static int sa[2]; };
        |int S2::sa[2] = {2, 3};
        |namespace ns { struct S5 { static int nk; }; }
        |int ns::S5::nk = 5;
        |template <class T> struct S6 { static int tk; };
        |template <class T> int S6<T>::tk = 6;
        |""".stripMargin,
      "test.cpp"
    )

    "keep every initializer and use the owning type as TYPE_REF" in {
      cpg.literal.code.l should contain allOf ("1", "2", "3", "5", "6")
      val owners = cpg.assignment.argument(1).isCall.nameExact(Operators.fieldAccess).argument(1).isTypeRef.l
      owners.map(_.typeFullName).sorted shouldBe List("S1", "S2", "S6", "ns.S5")
    }
  }

  "template specializations that share one full name" should {
    "get their own local for a member also referenced at namespace scope" in {
      // The <clinit> methods of all three specializations have the same full name. The reference to `entries` in the
      // static_assert resolves to a namespace-scope variable; the captured local created for each <clinit> must not be shared between them.
      val cpg = code(
        """struct E { int x; };
          |template <unsigned a, unsigned b = 0> struct T { E header; E entries; };
          |template <unsigned a> struct T<a, 0> { E header; E entries; };
          |template <> struct T<0, 0> { E header; E entries; };
          |static_assert(sizeof(T<1>::entries) == 4, "");
          |static_assert(offsetof(T<1>, entries) == 0, "");
          |""".stripMargin,
        "test.cpp"
      )
      val clinits = cpg.method.fullName(".*T.<clinit>.*").l
      clinits.size shouldBe 3
      clinits.local.nameExact("entries").size shouldBe 3
    }
  }

  "out-of-class static member definitions without initializer in templates" should {
    "not produce cross-method refs" in {
      val cpg = code(
        """template <class S, unsigned d> class Geom;
          |template <class S> class Geom<S, 1> {
          |  enum { n = 2 };
          |public:
          |  static void init() { for (unsigned i = 0; i < n; ++i) v_[i] = 0; }
          |  static int v_[n];
          |};
          |template <class S> int Geom<S, 1>::v_[Geom<S, 1>::n] = {1, 2};
          |template <class S> class Geom<S, 2> {
          |  enum { n = 3 };
          |public:
          |  static void init() { for (unsigned i = 0; i < n; ++i) v_[i] = 0; }
          |  static int v_[n];
          |};
          |template <class S> int Geom<S, 2>::v_[Geom<S, 2>::n] = {1, 2, 3};
          |""".stripMargin,
        "test.hh"
      )
      cpg.method.fullName(".*init.*").fullName.sorted shouldBe List(
        "Geom.<clinit>:Geom()",
        "Geom.<clinit>:Geom()<duplicate>0",
        "Geom.<enum>0.<clinit>:Geom.<enum>0()",
        "Geom.<enum>1.<clinit>:Geom.<enum>1()",
        "Geom.init:void()",
        "Geom.init<duplicate>0:void()"
      )
    }
  }
}
