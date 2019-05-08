package io.shiftleft.joern

import io.shiftleft.cpgqueryingtests.codepropertygraph._
import org.scalatest.{Matchers, WordSpec}

/**
  * Language primitives for navigating local variables
  * */
class LocalsTests extends WordSpec with Matchers {

  "should allow to query for all locals" in {
    cpg.local.name.toSet shouldBe Set("a", "b", "c", "z", "x", "q", "p")
  }

  "should allow to query for all locals in method `free_list`" in {
    cpg.method.name("free_list")
      .local.name.toSet shouldBe Set("q", "p")
  }

  "should prove correct (name, type) pairs for locals" in {
    cpg.method.name("free_list")
      .local.map(l => (l.name, l.typeFullName)).toSet shouldBe
      Set(("q", "struct node *"), ("p", "struct node *"))
  }

  "should allow finding filenames by local regex" in {
    val filename = cpg.local.name("a*").file.name.headOption()
    filename should not be empty
    filename.head.endsWith(".c") shouldBe true
  }

  val code =
    """| #include <stdlib.h>
       | struct node {
       | int value;
       | struct node *next;
       | };
       |
       | void free_list(struct node *head) {
       | struct node *q;
       | for (struct node *p = head; p != NULL; p = q) {
       |    q = p->next;
       |    free(p);
       |    }
       | }
       | int flow(int p0) {
       |    int a = p0;
       |    int b=a;
       |    int c=0x31;
       |    int z = b + c;
       |    z++;
       |    int x = z;
       |    return x;
       |    }
    """.stripMargin

  val semantics = "src/main/resources/default.semantics"
  val cpgFactory = new CpgFactory(LanguageFrontend.Fuzzyc, semantics)
  val cpg = cpgFactory.buildCpg(code)

}
