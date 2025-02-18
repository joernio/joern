package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.C2CpgSuite
import io.shiftleft.semanticcpg.language.*

/** Language primitives for navigating local variables
  */
class LocalQueryTests extends C2CpgSuite {

  "local query example 1" should {
    "allow to query for the locals" in {
      val cpg = code(
        """
        |void foo() {
        |  static const Foo::Bar bar{};
        |  static extern std::vector<int> vec;
        |}
        |""".stripMargin,
        "test.cpp"
      )
      val List(barLocal) = cpg.method.name("foo").local.nameExact("bar").l
      barLocal.typeFullName shouldBe "Foo.Bar"
      barLocal.code shouldBe "static const Foo::Bar bar"

      val List(vecLocal) = cpg.method.name("foo").local.nameExact("vec").l
      vecLocal.typeFullName shouldBe "std.vector<int>"
      vecLocal.code shouldBe "static extern std::vector<int> vec"
    }
  }

  "local query example 2" should {
    "allow to query for the local" in {
      val cpg = code(
        """
          |class Foo {
          |  static Foo* foo() {
          |    static Foo bar;
          |    return &bar;
          |  }
          |}
          |""".stripMargin,
        "test.cpp"
      )
      val List(barLocal) = cpg.method.name("foo").local.nameExact("bar").l
      barLocal.typeFullName shouldBe "Foo"
      barLocal.code shouldBe "static Foo bar"
    }
  }

  "local query example 3" should {
    val cpg = code("""
        | struct node {
        |   int value;
        |   struct node *next;
        | };
        |
        | void free_list(struct node *head) {
        |   struct node *q;
        |   for (struct node *p = head; p != NULL; p = q) {
        |     q = p->next;
        |     free(p);
        |   }
        | }
        |
        | int flow(int p0) {
        |   int a = p0;
        |   int b = a;
        |   int c = 0x31;
        |   int z = b + c;
        |   z++;
        |   int x = z;
        |   return x;
        | }
        |
        | void test() {
        |   static int a, *b, c[1];
        |   wchar_t *foo;
        |   int d[10], e = 1;
        | }
        | """.stripMargin)

    "should allow to query for all locals" in {
      cpg.local.name.sorted.l shouldBe List("NULL", "a", "a", "b", "b", "c", "c", "d", "e", "foo", "p", "q", "x", "z")
    }

    "should prove correct (name, type) pairs for locals" in {
      inside(cpg.method.name("free_list").local.l) { case List(q, p, nullLocal) =>
        q.name shouldBe "q"
        q.typeFullName shouldBe "node*"
        q.code shouldBe "struct node *q"
        p.name shouldBe "p"
        p.typeFullName shouldBe "node*"
        p.code shouldBe "struct node *p"
        nullLocal.name shouldBe "NULL"
        nullLocal.typeFullName shouldBe "ANY"
        nullLocal.code shouldBe "NULL"
      }
    }

    "should prove correct (name, type, code) pairs for locals" in {
      inside(cpg.method.name("test").local.l) { case List(a, b, c, foo, d, e) =>
        a.name shouldBe "a"
        a.typeFullName shouldBe "int"
        a.code shouldBe "static int a"
        b.name shouldBe "b"
        b.typeFullName shouldBe "int*"
        b.code shouldBe "static int *b"
        c.name shouldBe "c"
        c.typeFullName shouldBe "int[1]"
        c.code shouldBe "static int c[1]"
        foo.name shouldBe "foo"
        foo.typeFullName shouldBe "wchar_t*"
        foo.code shouldBe "wchar_t *foo"
        d.name shouldBe "d"
        d.typeFullName shouldBe "int[10]"
        d.code shouldBe "int d[10]"
        e.name shouldBe "e"
        e.typeFullName shouldBe "int"
        e.code shouldBe "int e"
      }
    }

    "should allow finding filenames by local regex" in {
      val filename = cpg.local.name("a*").file.name.headOption
      filename should not be empty
      filename.head.endsWith(".c") shouldBe true
    }
  }
  "local query example 4" should {
    "be correct for global variable bindings" in {
      val cpg = code(
        """
        |int x = 1;
        |void foo() {
        |  x = 2;
        |}""".stripMargin,
        "Test0.cpp"
      )
      val List(x1, x2) = cpg.local.sortBy(_.lineNumber.get).l
      x1.name shouldBe "x"
      x1.closureBindingId shouldBe None
      val List(methodRef) = cpg.methodRef.methodFullNameExact("foo:void()").l
      methodRef.containsIn.l shouldBe cpg.method.fullNameExact("Test0.cpp:<global>").l
      val List(binding) = x1.closureBinding.l
      binding.closureBindingId shouldBe Some("Test0.cpp:foo:x")
      binding.closureOriginalName shouldBe Some("x")
      binding._captureIn.l shouldBe List(methodRef)
      val List(id1) = x1.referencingIdentifiers.l
      id1.name shouldBe "x"
      id1.typeFullName shouldBe "int"
      id1.lineNumber shouldBe Some(2)

      x2.name shouldBe "x"
      x2.closureBindingId shouldBe Some("Test0.cpp:foo:x")
      x2.closureBinding shouldBe empty
      val List(id2) = x2.referencingIdentifiers.l
      id2.name shouldBe "x"
      id2.typeFullName shouldBe "int"
      id2.lineNumber shouldBe Some(4)
    }
  }
}
