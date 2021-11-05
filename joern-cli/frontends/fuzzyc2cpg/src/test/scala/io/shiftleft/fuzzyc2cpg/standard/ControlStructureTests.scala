package io.shiftleft.fuzzyc2cpg.standard

import io.shiftleft.fuzzyc2cpg.testfixtures.FuzzyCCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class ControlStructureTests extends FuzzyCCodeToCpgSuite {

  override val code =
    """
      |void foo(int x, int y) {
      | try {
      |    goto foo;
      |    foo:
      | } catch(exc_t exc) {
      |   // ...
      | }
      |
      | for(int i = 0; i < 10; i++) {
      |     if (x > y) {
      |     continue;
      |    }
      |    while(y++ < x) {
      |     printf("foo\n");
      |   }
      | }
      |
      |switch(y) {
      |  case 1:
      |   printf("bar\n");
      |   break;
      |  default:
      |};
      |
      | int i = 0;
      | do {
      |   i++;
      | } while(i < 11);
      |}
      |
      |""".stripMargin

  "should identify `try` block" in {
    cpg.method("foo").tryBlock.code.l shouldBe List("try")
  }

  "should identify `if` block" in {
    cpg.method("foo").ifBlock.condition.code.l shouldBe List("x > y")
  }

  "should identify `switch` block" in {
    cpg.method("foo").switchBlock.code.l shouldBe List("switch(y)")
  }

  "should identify `for` block" in {
    cpg.method("foo").forBlock.condition.code.l shouldBe List("i < 10")
  }

  "should identify `while` block" in {
    cpg.method("foo").whileBlock.condition.code.l shouldBe List("y++ < x")
  }

  "should identify `do` block" in {
    cpg.method("foo").doBlock.condition.code.l shouldBe List("i < 11")
  }

  "should identify `goto`" in {
    cpg.method("foo").goto.code.l shouldBe List("goto foo;")
  }

  "should identify `break`" in {
    cpg.method("foo").break.code.l shouldBe List("break;")
  }

  "should identify `continue`" in {
    cpg.method("foo").continue.code.l shouldBe List("continue;")
  }

}
