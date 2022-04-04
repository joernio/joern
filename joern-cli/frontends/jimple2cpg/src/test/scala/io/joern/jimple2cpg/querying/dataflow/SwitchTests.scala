package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.jimple2cpg.testfixtures.JimpleDataflowFixture

class SwitchTests extends JimpleDataflowFixture {

  behavior of "Dataflow through `SWITCH`"

  override val code: String =
    """
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
      |""".stripMargin

  it should "find a path if the source is in a switch" in {
    val (source, sink) = getConstSourceSink("test1")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if the sink is in a switch" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 1
  }
}
