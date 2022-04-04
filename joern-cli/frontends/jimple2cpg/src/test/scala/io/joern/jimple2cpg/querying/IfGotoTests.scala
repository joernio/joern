package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCodeToCpgFixture
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Unknown}
import io.shiftleft.semanticcpg.language._

class IfGotoTests extends JimpleCodeToCpgFixture {

  override val code: String =
    """
      |class Foo {
      |
      |   void foo(int x, int y) {
      |     for(int i = 0; i < 10; i++) {
      |         if (x > y) {
      |             continue;
      |         }
      |         while (y++ < x) {
      |           System.out.println("foo");
      |         }
      |     }
      |
      |     int i = 0;
      |     do {
      |         i++;
      |     } while(i < 11);
      |   }
      |
      |}
      |""".stripMargin

  "should identify `goto` blocks" in {
    cpg.all.collect { case x: Unknown => x }.code.toSetMutable shouldBe Set("goto 9", "goto 5")
  }

  "should contain 4 branching nodes at conditional calls" in {
    cpg.all
      .collect { case x: Call => x }
      .filter { x =>
        x.cfgOut.size > 1
      }
      .code
      .toSetMutable shouldBe Set("i < 11", "$stack6 >= x", "x <= y", "i >= 10")
  }

}
