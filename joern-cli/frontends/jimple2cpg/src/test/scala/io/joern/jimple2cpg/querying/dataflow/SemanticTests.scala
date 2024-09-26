package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.language.*
import io.joern.jimple2cpg.testfixtures.{JimpleDataFlowCodeToCpgSuite, JimpleDataflowTestCpg}
import io.joern.dataflowengineoss.semanticsloader.FlowSemantic
import io.joern.x2cpg.Defines

class SemanticTests
    extends JimpleDataFlowCodeToCpgSuite(semantics =
      DefaultSemantics().plus(
        List(
          FlowSemantic.from("Test.sanitize:java.lang.String(java.lang.String)", List((0, 0), (1, 1))),
          FlowSemantic.from("java.nio.file.Paths.get:.*\\(java.lang.String,.*\\)", List.empty, regex = true)
        )
      )
    ) {

  "Dataflow through custom semantics" should {
    lazy implicit val cpg: JimpleDataflowTestCpg = code(
      """
      |import java.nio.file.Paths;
      |import java.net.URI;
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
      |   String b = Paths.get(URI.create(s)).toString();
      |   System.out.println(b);
      | }
      |
      | public void test4() {
      |   String s = "MALICIOUS";
      |   String b = Paths.get("/tmp", s).toString();
      |   System.out.println(b);
      | }
      |
      | public void test5() {
      |   String s = "MALICIOUS";
      |   byte[] dst = new byte[10];
      |   System.arraycopy(s.getBytes(), 0, dst, 0, 9);
      |   String b = new String(dst);
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
      |}""".stripMargin,
      "Test.java"
    )

    "find a path" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "be kill in sanitizer" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 0
    }

    "taints return" in {
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 1
    }

    "be killed" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 0
    }

    "follow taint rules" in {
      val (source, sink) = getConstSourceSink("test5")
      sink.reachableBy(source).size shouldBe 1
    }

  }

}
