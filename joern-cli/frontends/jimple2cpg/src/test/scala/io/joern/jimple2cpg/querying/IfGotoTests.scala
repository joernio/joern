package io.joern.jimple2cpg.querying

import io.joern.jimple2cpg.testfixtures.JimpleCode2CpgFixture
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Call, Unknown}
import io.shiftleft.semanticcpg.language.*

class IfGotoTests extends JimpleCode2CpgFixture {

  val cpg: Cpg = code("""
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
      |""".stripMargin).cpg

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
  val cpg2: Cpg = code("""
                         |  class IfTest {
                         |      void foo(int x) {
                         |        try{
                         |          if (x < 33) {
                         |              baz(bar(), 1);
                         |          }else{
                         |              baz(bar(), 2);
                         |          }
                         |        }catch(Exception e){
                         |          baz(bar(), 3);
                         |        }
                         |      }
                         |      Object bar() {
                         |          return new Object();
                         |      }
                         |      void baz(Object o, int x) {
                         |          if (o == null) {
                         |              System.out.println("null");
                         |          }else{
                         |              System.out.println(x);
                         |          }
                         |      }
                         |  }
                         |""".stripMargin).cpg

  "should have possibleTypes in condition calls" in {
    val conditionCall = cpg2.call.code(".*33.*").head
    conditionCall.name shouldBe "<operator>.greaterEqualsThan"
    conditionCall.possibleTypes shouldBe List("Condition")
    cpg2.call.code(".*null.*").head.possibleTypes shouldBe List("Condition")
    val call1             = cpg2.call.code(".*this.baz\\(.*, 1\\).*").head
    val call1ControlledBy = call1.controlledBy.isCall.l
    call1ControlledBy.size shouldBe 2
    call1ControlledBy.filter { (c: Call) => c.possibleTypes.contains("Condition") }.head shouldBe conditionCall
  }
}
