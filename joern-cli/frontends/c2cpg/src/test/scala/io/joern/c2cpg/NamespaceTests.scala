package io.joern.c2cpg

import io.joern.c2cpg.fixtures.CompleteCpgFixture
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Identifier}
import io.shiftleft.semanticcpg.language._
import org.scalatest.Inside
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class NamespaceTests extends AnyWordSpec with Matchers with Inside with CompleteCpgFixture {

  "Namespaces" should {

    "be correct for nested namespaces" in CompleteCpgFixture("""
        |namespace Q {
        |  namespace V {   // V is a member of Q, and is fully defined within Q
        |    class C { int m(); }; // C is a member of V and is fully defined within V
        |                          // C::m is only declared
        |    int f(); // f is a member of V, but is only declared here
        |  }
        | 
        |  int V::f() // definition of V's member f outside of V
        |             // f's enclosing namespaces are still the global namespace, Q, and Q::V
        |  {
        |    extern void h(); // This declares ::Q::V::h
        |    return 0;
        |  }
        | 
        |  int V::C::m() // definition of V::C::m outside of the namespace (and the class body)
        |                // enclosing namespaces are the global namespace, Q, and Q::V
        |  { return 0 ; }
        |}
        |""".stripMargin) { cpg =>
      inside(cpg.method.isNotStub.fullName.l) { case List(f, m) =>
        f shouldBe "Q.V.f"
        m shouldBe "Q.V.C.m"
      }

      inside(cpg.namespaceBlock.l) { case List(_, _, q, v) =>
        q.fullName shouldBe "Q"
        v.fullName shouldBe "Q.V"

        inside(v.typeDecl.l) { case List(c) =>
          c.name shouldBe "C"
          c.fullName shouldBe "Q.V.C"
        }

        inside(q.method.l) { case List(f, m) =>
          f.name shouldBe "f"
          f.fullName shouldBe "Q.V.f"
          m.name shouldBe "m"
          m.fullName shouldBe "Q.V.C.m"
        }
      }

    }

    "be correct for nested namespaces in C++17 style" in CompleteCpgFixture("""
        |namespace Q::V {
        |  class C { int m(); }; // C is a member of V and is fully defined within V
        |                        // C::m is only declared
        |  int f(); // f is a member of V, but is only declared here
        |}
        | 
        |int V::f() // definition of V's member f outside of V
        |           // f's enclosing namespaces are still the global namespace, Q, and Q::V
        |{
        |  extern void h(); // This declares ::Q::V::h
        |  return 0;
        |}
        | 
        |int V::C::m() // definition of V::C::m outside of the namespace (and the class body)
        |              // enclosing namespaces are the global namespace, Q, and Q::V
        |{ return 0; }
        |""".stripMargin) { cpg =>
      inside(cpg.method.fullName.l) { case List(_, m1, f1, f2, h, m2) =>
        // TODO: this looks strange too it first glance. But as Eclipse CDT does not provide any
        //  mapping from definitions outside of namespace into them we cant reconstruct proper full-names.
        m1 shouldBe "Q.V.C.m"
        f1 shouldBe "Q.V.f"
        h shouldBe "V.f.h"
        f2 shouldBe "V.f"
        m2 shouldBe "V.C.m"
      }

      inside(cpg.namespaceBlock.l) { case List(_, _, q, v) =>
        q.fullName shouldBe "Q"
        v.fullName shouldBe "Q.V"

        inside(v.typeDecl.l) { case List(c) =>
          c.name shouldBe "C"
          c.fullName shouldBe "Q.V.C"
        }
      }
    }

    "be correct for unnamed namespaces" in CompleteCpgFixture("""
        |namespace {
        |  int i; // defines ::(unique)::i
        |}
        |void f() {
        |  i++;   // increments ::(unique)::i
        |}
        | 
        |namespace A {
        |  namespace {
        |    int i;        // A::(unique)::i
        |    int j;        // A::(unique)::j
        |  }
        |  void g() { i++; } // A::(unique)::i++
        |}
        | 
        |using namespace A; // introduces all names from A into global namespace
        |
        |void h() {
        |  i++;    // error: ::(unique)::i and ::A::(unique)::i are both in scope
        |  A::i++; // ok, increments ::A::(unique)::i
        |  j++;    // ok, increments ::A::(unique)::j
        |}""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, unnamed1, a, unnamed2) =>
        unnamed1.fullName shouldBe "anonymous_namespace_0"
        a.fullName shouldBe "A"
        unnamed2.fullName shouldBe "A.anonymous_namespace_1"
      }

      inside(cpg.method.internal.fullName.l) { case List(_, f, g, h) =>
        f shouldBe "f"
        g shouldBe "A.g"
        h shouldBe "h"
      }

      inside(cpg.method.nameExact("h").ast.isCall.code.l) { case List(c1, c2, c3, c4) =>
        c1 shouldBe "i++"
        c2 shouldBe "A::i++"
        c3 shouldBe "A::i"
        c4 shouldBe "j++"
      }
    }

    "be correct for namespaces with using" in CompleteCpgFixture("""
        |void f();
        |
        |namespace A {
        |  void g();
        |}
        | 
        |namespace X {
        |  using ::f;        // global f is now visible as ::X::f
        |  using A::g;       // A::g is now visible as ::X::g
        |}
        | 
        |void h() {
        |  X::f(); // calls ::f
        |  X::g(); // calls A::g
        |}""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, a, x) =>
        a.fullName shouldBe "A"
        x.fullName shouldBe "X"
      }

      inside(cpg.method.internal.fullName.l) { case List(_, f, g, h) =>
        f shouldBe "f"
        g shouldBe "A.g"
        h shouldBe "h"
      }

      inside(cpg.call.filterNot(_.name == Operators.fieldAccess).l) { case List(f, g) =>
        f.name shouldBe "X::f"
        f.methodFullName shouldBe "X::f"
        g.name shouldBe "X::g"
        g.methodFullName shouldBe "X::g"
      }
    }

    "be correct for namespaces with using and synonyms" in CompleteCpgFixture("""
        |namespace A {
        |  void f(int);
        |}
        |using A::f; // ::f is now a synonym for A::f(int)
        | 
        |namespace A {     // namespace extension
        |  void f(char); // does not change what ::f means
        |}
        | 
        |void foo() {
        |  f('a'); // calls f(int), even though f(char) exists.
        |}
        | 
        |void bar() {
        |  using A::f; // this f is a synonym for both A::f(int) and A::f(char)
        |  f('a');     // calls f(char)
        |}""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, a1, a2) =>
        // TODO: how to handle namespace extension?
        a1.fullName shouldBe "A"
        a2.fullName shouldBe "A"
      }

      inside(cpg.method.internal.l) { case List(_, f1, f2, foo, bar) =>
        f1.fullName shouldBe "A.f"
        f1.signature shouldBe "void A.f (int)"
        f2.fullName shouldBe "A.f"
        f2.signature shouldBe "void A.f (char)"
        foo.fullName shouldBe "foo"
        bar.fullName shouldBe "bar"
      }

      inside(cpg.call.l) { case List(c1, c2) =>
        c1.name shouldBe "f"
        c1.methodFullName shouldBe "f"
        c2.name shouldBe "f"
        c2.methodFullName shouldBe "f"
      }
    }

    "be correct for namespaces with alias" in CompleteCpgFixture("""
        |namespace foo {
        |  namespace bar {
        |    namespace baz {
        |      int qux = 42;
        |    }
        |  }
        |}
        | 
        |namespace fbz = foo::bar::baz;
        | 
        |int main() {
        |  int x = fbz::qux;
        |}""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, foo, bar, baz, fbz) =>
        foo.name shouldBe "foo"
        foo.fullName shouldBe "foo"
        bar.name shouldBe "bar"
        bar.fullName shouldBe "foo.bar"
        baz.name shouldBe "baz"
        baz.fullName shouldBe "foo.bar.baz"

        inside(baz.ast.isIdentifier.l) { case List(qux) =>
          qux.name shouldBe "qux"
          qux.typeFullName shouldBe "int"
        }

        fbz.name shouldBe "fbz"
        fbz.fullName shouldBe "foo.bar.baz"
      }

      inside(cpg.call.l) { case List(c1, c2, c3) =>
        c1.code shouldBe "qux = 42"
        c2.code shouldBe "x = fbz::qux"
        c3.code shouldBe "fbz::qux"
        inside(c3.ast.l) { case List(call: Call, x: Identifier, a: FieldIdentifier) =>
          call.name shouldBe Operators.fieldAccess
          x.order shouldBe 1
          x.name shouldBe "fbz"
          a.order shouldBe 2
          a.code shouldBe "qux"
        }
      }
    }

    "be correct for namespaces with alias inside of methods" in CompleteCpgFixture("""
       |namespace foo {
       |  namespace bar {};
       |};
       | 
       |int main() {
       |  namespace x = foo::bar;
       |};""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, foo, bar, x) =>
        foo.name shouldBe "foo"
        foo.fullName shouldBe "foo"
        bar.name shouldBe "bar"
        bar.fullName shouldBe "foo.bar"
        x.name shouldBe "x"
        x.fullName shouldBe "foo.bar"
      }
    }

    "be correct for using namespaces inside of methods" in CompleteCpgFixture("""
       |namespace foo {
       |  namespace bar {};
       |};
       | 
       |int main() {
       |  using namespace foo::bar;
       |};""".stripMargin) { cpg =>
      inside(cpg.namespaceBlock.l) { case List(_, _, foo, bar) =>
        foo.name shouldBe "foo"
        foo.fullName shouldBe "foo"
        bar.name shouldBe "bar"
        bar.fullName shouldBe "foo.bar"
      }
    }

    "be correct for derived classes in namespaces" in CompleteCpgFixture("""
        |namespace BaseClasses {
        |
        |class A {
        |};
        |
        |}
        |
        |namespace IntermediateClasses {
        |
        |class B1 : public BaseClasses::A {
        |};
        |
        |class B2 : public BaseClasses::A {
        |};
        |
        |}
        |
        |namespace FinalClasses {
        |
        |class C11 : public IntermediateClasses::B1 {
        |};
        |
        |class C12 : public IntermediateClasses::B1 {
        |};
        |
        |class C21 : public IntermediateClasses::B2 {
        |};
        |
        |class C22 : public IntermediateClasses::B2 {
        |};
        |
        |class C23 : public IntermediateClasses::B2 {
        |};
        |
        |}
        |
        |int main(int argc, char** argv) {
        |  IntermediateClasses::B1* b11 = new FinalClasses::C11();
        |  IntermediateClasses::B1* b12 = new FinalClasses::C12();
        |  IntermediateClasses::B2* b21 = new FinalClasses::C21();
        |  IntermediateClasses::B2* b22 = new FinalClasses::C22();
        |  IntermediateClasses::B2* b23 = new FinalClasses::C23();
        |
        |  BaseClasses::A* a11 = b11;
        |  BaseClasses::A* a12 = b12;
        |  BaseClasses::A* a21 = b21;
        |  BaseClasses::A* a22 = b22;
        |  BaseClasses::A* a23 = b23;
        |
        |  return 0;
        |};
        |""".stripMargin) { cpg =>
      inside(cpg.namespace.nameNot("<global>").sortBy(_.name).toList) {
        case List(baseClasses, finalClasses, interClasses) =>
          baseClasses.name shouldBe "BaseClasses"
          interClasses.name shouldBe "IntermediateClasses"
          finalClasses.name shouldBe "FinalClasses"
      }
      inside(cpg.namespaceBlock.nameNot("<global>").l) { case List(baseClasses, interClasses, finalClasses) =>
        baseClasses.name shouldBe "BaseClasses"
        baseClasses.fullName shouldBe "BaseClasses"
        interClasses.name shouldBe "IntermediateClasses"
        interClasses.fullName shouldBe "IntermediateClasses"
        finalClasses.name shouldBe "FinalClasses"
        finalClasses.fullName shouldBe "FinalClasses"
      }
      inside(cpg.typ.name("A").derivedTypeTransitive.l) { case List(b1, c11, c12, b2, c21, c22, c23) =>
        b1.name shouldBe "B1"
        b1.fullName shouldBe "IntermediateClasses.B1"
        b1.typeDeclFullName shouldBe "IntermediateClasses.B1"

        c11.name shouldBe "C11"
        c11.fullName shouldBe "FinalClasses.C11"
        c11.typeDeclFullName shouldBe "FinalClasses.C11"

        c12.name shouldBe "C12"
        c12.fullName shouldBe "FinalClasses.C12"
        c12.typeDeclFullName shouldBe "FinalClasses.C12"

        b2.name shouldBe "B2"
        b2.fullName shouldBe "IntermediateClasses.B2"
        b2.typeDeclFullName shouldBe "IntermediateClasses.B2"

        c21.name shouldBe "C21"
        c21.fullName shouldBe "FinalClasses.C21"
        c21.typeDeclFullName shouldBe "FinalClasses.C21"

        c22.name shouldBe "C22"
        c22.fullName shouldBe "FinalClasses.C22"
        c22.typeDeclFullName shouldBe "FinalClasses.C22"

        c23.name shouldBe "C23"
        c23.fullName shouldBe "FinalClasses.C23"
        c23.typeDeclFullName shouldBe "FinalClasses.C23"
      }
    }

  }

}
