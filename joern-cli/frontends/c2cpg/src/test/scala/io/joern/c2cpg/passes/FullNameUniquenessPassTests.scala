package io.joern.c2cpg.passes

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal

class FullNameUniquenessPassTests extends C2CpgSuite {

  "the FullNameUniquenessPass" should {
    "create truly unique method fullnames for multiple C functions" in {
      val cpg = code(
        """
          |#include "h1.h"
          |void main() {
          |  foo(1, 2, 3);
          |}
          """.stripMargin,
        "main.c"
      ).moreCode(
        """
          |void foo(int a, int b, int c) {}
          |""".stripMargin,
        "h1.h"
      ).moreCode(
        """
          |void foo(float a, float b, float c) {}
          |""".stripMargin,
        "h2.h"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        "foo",
        "foo<duplicate>0",
        "main"
      )
    }

    "create truly unique method fullnames and bindings for templated C++ functions" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    template<typename Bar, Kind k>
          |    bool foo(Bar x) { return true; }
          |
          |    template<typename Bar>
          |    bool foo(Bar x) { return false; }
          |}
          |""".stripMargin,
        "main.cpp"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot(NamespaceTraversal.globalNamespaceName).fullName.sorted.l shouldBe List(
        "Foo.foo:bool(Bar)",
        "Foo.foo<duplicate>0:bool(Bar)"
      )
      cpg.binding.l.map(b => (b.methodFullName, b.name, b.signature)) shouldBe List(
        ("Foo.foo:bool(Bar)", "foo", "bool(Bar)"),
        ("Foo.foo<duplicate>0:bool(Bar)", "foo<duplicate>0", "bool(Bar)")
      )
    }

    "respect method fullnames for const C++ functions" in {
      val cpg = code(
        """
          |class Foo {
          |  public:
          |    int foo(int a) { return 0; }
          |    int foo(int a) const { return 0; }
          |}
          |
          |void main() {
          |  Foo f1 = new Foo();
          |  const Foo f2 = new Foo();
          |  f1.foo(1);
          |  f2.foo(2);
          |}
          |""".stripMargin,
        "main.cpp"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameExact("foo").fullName.sorted.l shouldBe List("Foo.foo:int(int)", "Foo.foo:int(int)<const>")
      val List(call1, call2) = cpg.call.name("foo").sortBy(_.lineNumber).l
      call1.methodFullName shouldBe "Foo.foo:int(int)"
      call2.methodFullName shouldBe "Foo.foo:int(int)<const>"
    }

    "fix calls to static function in the same file correctly" in {
      val cpg = code(
        """
          |void f(void);
          |static void sf() { }
          |void a() {
          |  f();
          |  sf();
          |}
          |""".stripMargin,
        "a.c"
      ).moreCode(
        """
          |void a(void);
          |void f() {}
          |static void sf() {}
          |void m() {
          |  f();
          |  sf();
          |}
          |void main() {
          |  m();
          |  a();
          |}
          |""".stripMargin,
        "main.c"
      )
      cpg.method.fullName.size shouldBe cpg.method.fullName.toSet.size
      cpg.method.nameNot("<global>").map(m => (m.fullName, m.filename)).sorted.l shouldBe List(
        ("a", "a.c"),
        ("f", "main.c"),
        ("m", "main.c"),
        ("main", "main.c"),
        ("sf", "a.c"),
        ("sf<duplicate>0", "main.c")
      )
      cpg.call
        .map(c =>
          s"${c.name} in ${c.file.map(_.name).head} -> CALL -> ${c.methodFullName} in ${c.callOut.filename.head}"
        )
        .sorted
        .l shouldBe List(
        "a in main.c -> CALL -> a in a.c",
        "f in a.c -> CALL -> f in main.c",
        "f in main.c -> CALL -> f in main.c",
        "m in main.c -> CALL -> m in main.c",
        "sf in a.c -> CALL -> sf in a.c",
        "sf<duplicate>0 in main.c -> CALL -> sf<duplicate>0 in main.c" // fixed call here
      )
    }
  }

}
