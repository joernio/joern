package io.joern.javasrc2cpg.querying.dataflow

import io.joern.dataflowengineoss.language._
import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.semanticcpg.language._

class DoNotMerge2 extends JavaDataflowFixture {

  behavior of "Dataflow through objects"

  override val code: String =
    """
      |class Bar {
      |    public String value;
      |
      |    public static Bar alloc() {
      |        return new Bar();
      |    }
      |
      |    public void init(String s) {
      |        value = s;
      |    }
      |
      |    public Bar initWithReturn(String s) {
      |        value = s;
      |        return this;
      |    }
      |
      |    public static Bar withValue(Bar b, String s) {
      |        b.value = s;
      |        return b;
      |    }
      |
      |    public String toString() {
      |        return value;
      |    }
      |
      |    public static void sink(Bar b) {
      |        System.out.println(b.toString());
      |    }
      |
      |    public static void test1() {
      |        Bar b = alloc();
      |        b.init("MALICIOUS");
      |        sink(b);
      |    }
      |
      |    public static void test2() {
      |        Bar b = alloc();
      |        b.undefinedInit("MALICIOUS");
      |        sink(b);
      |    }
      |
      |    public static void test3() {
      |        Bar b = alloc();
      |        b.initWithReturn("MALICIOUS");
      |        sink(b);
      |    }
      |
      |    public static void test4() {
      |        Bar b = alloc();
      |        b = b.initWithReturn("MALICIOUS");
      |        sink(b);
      |    }
      |}
      |""".stripMargin

  it should "find a flow through a defined <init> WITHOUT semantics" in {
    def source = cpg.method.name("test1").literal.code("\"MALICIOUS\"")
    def sink = cpg.method.name("sink").call.name("println").argument

    sink.reachableBy(source).size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1 // <- This is 0
  }

  it should "find a flow through an undefined <init> (overtaint)" in {
    def source = cpg.method.name("test2").literal.code("\"MALICIOUS\"")
    def sink = cpg.method.name("sink").call.name("println").argument

    sink.reachableBy(source).size shouldBe 1 // <- This is 0
    sink.reachableByFlows(source).size shouldBe 1
  }

  it should "find a flow through an <init> WITH a return" in {
    def source = cpg.method.name("test3").literal.code("\"MALICIOUS\"")
    def sink = cpg.method.name("sink").call.name("println").argument

    sink.reachableBy(source).size shouldBe 1
    sink.reachableByFlows(source).size shouldBe 1 // <- This is 0
  }

  it should "find a flow through a reassignment using <init> WITH a return" in {
    def source = cpg.method.name("test4").literal.code("\"MALICIOUS\"")
    def sink = cpg.method.name("sink").call.name("println").argument

    sink.reachableBy(source).size shouldBe 2 // Why are there 2?
    sink.reachableByFlows(source).size shouldBe 1
  }
}
