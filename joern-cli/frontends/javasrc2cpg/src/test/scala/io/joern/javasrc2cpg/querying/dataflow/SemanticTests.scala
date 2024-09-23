package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.joern.dataflowengineoss.language.*
import io.joern.dataflowengineoss.queryengine.{EngineContext, EngineConfig}

import scala.jdk.CollectionConverters.*

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.x2cpg.Defines

class SemanticTests
    extends JavaDataflowFixture(semantics =
      DefaultSemantics().plus(
        List(
          FlowSemantic.from("Test.sanitize:java.lang.String(java.lang.String)", List((0, 0), (1, 1))),
          FlowSemantic.from(s"ext.Library.killParam:${Defines.UnresolvedSignature}(1)", List.empty),
          FlowSemantic.from("^ext\\.Library\\.taintNone:.*", List((0, 0), (1, 1)), regex = true),
          FlowSemantic.from("^ext\\.Library\\.taint1to2:.*", List((1, 2)), regex = true)
        )
      )
    ) {
  behavior of "Dataflow through custom semantics"

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
      |   StringBuilder sb = new StringBuilder(s);
      |   Library.killParam(sb);
      |   String c = sb.toString();
      |   System.out.println(c);
      | }
      |
      | public void test5() {
      |   String s = "MALICIOUS";
      |   String b = Library.taintNone(s);
      |   System.out.println(b);
      | }
      |
      | public void test6() {
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

  it should "taints return for unresolved method by default" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "be killed if semantic does not specify that it taints itself" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "be killed in custom semantic" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "taint param2" in {
    val (source, sink) = getConstSourceSink("test6")
    sink.reachableBy(source).size shouldBe 1
  }
}
