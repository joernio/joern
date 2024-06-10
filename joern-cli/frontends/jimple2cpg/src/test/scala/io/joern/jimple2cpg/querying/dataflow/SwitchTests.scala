package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg

class SwitchTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through `SWITCH`" should {

    lazy implicit val cpg: Cpg = code("""
        |class Foo {
        |    public void test1(int input) {
        |        String s;
        |
        |        switch (input) {
        |            case 0:
        |            case 1:
        |                s = "SAFE";
        |                break;
        |            case 2:
        |                s = "MALICIOUS";
        |                break;
        |            default:
        |                s = "SAFE";
        |        }
        |        System.out.println(s);
        |    }
        |
        |    public void test2(int input) {
        |        String s = "MALICIOUS";
        |
        |        switch(input) {
        |            case 0:
        |                System.out.println(s);
        |                break;
        |            default:
        |                System.out.println("SAFE");
        |        }
        |    }
        |}
        |""".stripMargin)

    "find a path if the source is in a switch" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if the sink is in a switch" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }
  }
}
