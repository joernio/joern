package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.joern.dataflowengineoss.language._
import io.joern.dataflowengineoss.queryengine.{EngineContext, EngineConfig}

import scala.jdk.CollectionConverters._

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic

class SemanticTests
    extends JavaDataflowFixture(extraFlows =
      List(
        FlowSemantic("Test.sanitize:java.lang.String(java.lang.String)", List((0, 0), (1, 1))),
        FlowSemantic("^ext\\.Library\\.taintReturn:.*", List((0, 0), (1, -1)), regex = true),
        FlowSemantic("^ext\\.Library\\.taintNone:.*", List((0, 0), (1, 1)), regex = true),
        FlowSemantic("^ext\\.Library\\.taint1to2:.*", List((1, 2)), regex = true)
      )
    ) {
  behavior of "Dataflow through custom semantics"

  // new Parser().parse("""
  //  "Test.sanitize:java.lang.String(java.lang.String)" 0->0 1->1
  //  "ext.Library.taintReturn:<unresolvedSignature>(1)" 0->0 1->-1
  //  "ext.Library.taintNone:<unresolvedSignature>(1)" 0->0 1->1
  //  "ext.Library.taint1to2:<unresolvedSignature>(2)" 0->0 1->1 2->2 1->2
  //  """)

  override val code: String =
    """
      |import ext.Library;
      |
      |public class Test {
      | public void test1() {
      |   String s = "MALICIOUS";
      |   String b = taint(s);
      |   System.out.println(b);
      | }
      |
      | public void test2() {
      |   String s = "MALICIOUS";
      |   String b = taint(s);
      |   String c = sanitize(b);
      |   System.out.println(c);
      | }
      |
      | public void test3() {
      |   String s = "MALICIOUS";
      |   String b = Library.foo(s);
      |   System.out.println(b);
      | }
      |
      | public void test4() {
      |   String s = "MALICIOUS";
      |   String b = Library.taintReturn(s);
      |   System.out.println(b);
      | }
      |
      | public void test5() {
      |   String s = "MALICIOUS";
      |   String b = Library.taintReturn(s);
      |   System.out.println(s);
      | }
      |
      | public void test6() {
      |   String s = "MALICIOUS";
      |   String b = Library.taintNone(s);
      |   System.out.println(b);
      | }
      |
      | public void test7() {
      |   String s = "MALICIOUS";
      |   StringBuilder sb = new StringBuilder();
      |   Library.taint1to2(s, sb);
      |   String b = sb.toString();
      |   System.out.println(b);
      | }
      |
      | public String taint(String s) {
      |     return s + ".taint";
      | }
      |
      | public String sanitize(String s) {
      |     if (s.contains("..")) {
      |         return s.replace("..", "");
      |     }
      |     return s;
      | }
      |}""".stripMargin

  it should "find a path" in {
    val (source, sink) = getConstSourceSink("test1")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "be kill in sanitizer" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "continue in external method" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "continue in custom external method" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "be killed if semantic does not specify that it taints itself" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 1 // XXX: not killed
  }

  it should "be killed in custom semantic" in {
    val (source, sink) = getConstSourceSink("test6")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "taint param2" in {
    val (source, sink) = getConstSourceSink("test7")
    sink.reachableBy(source).size shouldBe 1
  }
}
