package io.joern.javasrc2cpg.querying.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.semanticcpg.language._

class DoNotMerge extends JavaDataflowFixture {

  behavior of "Dataflow through objects"

  override val code: String =
    """
      |class Foo {
      |    public String value;
      |
      |    public Foo(String s) {
      |        value = s;
      |    }
      |
      |    public String toString() {
      |        return value;
      |    }
      |
      |    public static void sink(Foo b) {
      |        System.out.println(b.toString());
      |    }
      |
      |    public void test11() {
      |        Foo b = new Foo("MALICIOUS");
      |        sink(b);
      |    }
      |}
      |""".stripMargin

  it should "find a inter-procedural path from object variable" in {
    def source = cpg.method.name("test11").literal.code("\"MALICIOUS\"")
    def sink = cpg.method.name("sink").call.name("println").argument

    sink.reachableBy(source).size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1
  }
}
