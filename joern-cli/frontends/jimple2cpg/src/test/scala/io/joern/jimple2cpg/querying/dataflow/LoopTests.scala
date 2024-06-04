package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg

class LoopTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through loop structures" should {

    lazy implicit val cpg: Cpg = code("""
        |class Foo {
        |    public void test1(boolean b) {
        |        String s = "MALICIOUS";
        |        while (b) {
        |            s = "SAFE";
        |            b = !b;
        |        }
        |        System.out.println(s);
        |    }
        |
        |    public void test2(boolean b) {
        |        String s = "SAFE";
        |        while (b) {
        |            s = "MALICIOUS";
        |            b = !b;
        |        }
        |        System.out.println(s);
        |    }
        |
        |    public void test3(int maxLen) {
        |        for (String s = "SAFE"; s.length() < maxLen; s += "MALICIOUS") {
        |            System.out.println(s);
        |        }
        |    }
        |
        |    public void test4(int maxLen) {
        |        for (String s = "MALICIOUS"; s.length() < maxLen; s += "MALICIOUS") {
        |            s = "SAFE";
        |            System.out.println(s);
        |        }
        |    }
        |
        |    public void test5(int maxLen) {
        |        for (String s = "MALICIOUS"; s.length() < maxLen; s += "SAFE") {
        |            System.out.println(s);
        |        }
        |    }
        |
        |    public void test6(int maxLen) {
        |        String s = "MALICIOUS";
        |        do {
        |            s += "SAFE";
        |            System.out.println(s);
        |        } while (s.length() < maxLen);
        |    }
        |
        |    public void test7(String[] prefixes) {
        |        String s = "MALICIOUS";
        |        for (String prefix : prefixes) {
        |            String output = prefix + s;
        |            System.out.println(output);
        |        }
        |    }
        |
        |
        |    public void test10(int count) {
        |        String s = "";
        |        for (int i = 0; i < count; i++) {
        |            if (i == 6) {
        |                s += "MALICIOUS";
        |            } else {
        |                s += "SAFE";
        |            }
        |        }
        |        System.out.println(s);
        |    }
        |
        |    public void test11(String[] prefixes) {
        |        String s = "MALICIOUS";
        |        for (String prefix: prefixes) {
        |            String output = prefix + s;
        |            System.out.println(output);
        |        }
        |    }
        |
        |    public void test12(boolean b) {
        |        String s = "MALICIOUS";
        |        do {
        |            s = "SAFE";
        |            b = !b;
        |        } while (b);
        |        System.out.println(s);
        |    }
        |}
        |""".stripMargin)

    "find a path if `MALICIOUS` is possibly reassigned in a `WHILE" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is possibly assigned in a `WHILE`" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is added in `FOR` update" in {
      val (source, sink) = getConstSourceSink("test3", sourceCode = ".*MALICIOUS.*")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if `MALICIOUS` is always reassigned before sink in loop" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 0
    }

    "find a path if `MALICIOUS` is assigned in `FOR` init" in {
      val (source, sink) = getConstSourceSink("test5")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if sink is in a `DO` loop" in {
      val (source, sink) = getConstSourceSink("test6")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if sink is in `FOREACH` loop" in {
      val (source, sink) = getConstSourceSink("test7")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is possibly appended in `FOR`" in {
      val (source, sink) = getConstSourceSink("test10", sourceCode = ".*MALICIOUS.*")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is appended to prefix in `FOREACH`" in {
      val (source, sink) = getConstSourceSink("test11")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if `MALICIOUS` is reassigned before sink in `DO`" in {
      val (source, sink) = getConstSourceSink("test12")
      sink.reachableBy(source).size shouldBe 0
    }
  }
}
