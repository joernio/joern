package io.joern.c2cpg.querying

import io.joern.c2cpg.testfixtures.CCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

/** Language primitives for navigating local variables
  */
class LocalsTests extends CCodeToCpgSuite {

  override val code: String = """
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
    |   static int a, b, c;
    |   wchar_t *foo;
    | }
    | """.stripMargin

  "should allow to query for all locals" in {
    cpg.local.name.toSetMutable shouldBe Set("a", "b", "c", "z", "x", "q", "p")
  }

  "should allow to query for all locals in method `free_list`" in {
    cpg.method.name("free_list").local.name.toSetMutable shouldBe Set("q", "p")
  }

  "should prove correct (name, type) pairs for locals" in {
    inside(cpg.method.name("free_list").local.l) { case List(q, p) =>
      q.name shouldBe "q"
      q.typeFullName shouldBe "node"
      q.code shouldBe "struct node* q"
      p.name shouldBe "p"
      p.typeFullName shouldBe "node"
      p.code shouldBe "struct node* p"
    }
  }

  "should prove correct (name, type, code) pairs for locals" in {
    inside(cpg.method.name("test").local.l) { case List(a, b, c, foo) =>
      a.name shouldBe "a"
      a.typeFullName shouldBe "int"
      a.code shouldBe "static int a"
      b.name shouldBe "b"
      b.typeFullName shouldBe "int"
      b.code shouldBe "static int b"
      c.name shouldBe "c"
      c.typeFullName shouldBe "int"
      c.code shouldBe "static int c"
      foo.name shouldBe "foo"
      foo.typeFullName shouldBe "wchar_t"
      foo.code shouldBe "wchar_t* foo"
    }
  }

  "should allow finding filenames by local regex" in {
    val filename = cpg.local.name("a*").file.name.headOption
    filename should not be empty
    filename.head.endsWith(".c") shouldBe true
  }
}
