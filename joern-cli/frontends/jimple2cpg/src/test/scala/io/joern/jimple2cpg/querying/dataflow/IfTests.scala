package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg

class IfTests extends JimpleDataFlowCodeToCpgSuite {
  "dataflow through IF structures" should {

    lazy implicit val cpg: Cpg = code("""
    |class Foo {
    |    public void test1(boolean b) {
    |        String s = "MALICIOUS";
    |        if (b) {
    |            s = "SAFE";
    |        }
    |        System.out.println(s);
    |    }
    |
    |    public void test2(boolean b) {
    |        String s = "SAFE";
    |        if (b) {
    |            s = "MALICIOUS";
    |        }
    |        System.out.println(s);
    |    }
    |
    |    public void test3(boolean b) {
    |        String s;
    |
    |        if (b) {
    |            s = "SAFE";
    |        } else {
    |            s = "MALICIOUS";
    |        }
    |
    |        System.out.println(s);
    |    }
    |
    |    public void test4(int input) {
    |        String s;
    |
    |        if (input < 10) {
    |            s = "SAFE";
    |        } else if (input > 20) {
    |            s = "MALICIOUS";
    |        } else {
    |            s = "ALSO SAFE";
    |        }
    |
    |        System.out.println(s);
    |    }
    |
    |    public void test5(boolean b) {
    |        String s = "MALICIOUS";
    |
    |        if (b) {
    |            s = "SAFE";
    |        }
    |        else {
    |            s = "ALSO SAFE";
    |        }
    |
    |        System.out.println(s);
    |    }
    |}
    |""".stripMargin)

    "find a path if `MALICIOUS` is reassigned to `SAFE` in only one path of an if" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `SAFE` is reassigned to `MALICIOUS` in at least one path of an if" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is assigned in at least one path of an if" in {
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is assigned in a chained if" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if `MALICIOUS` is reassigned in all paths of an if" in {
      val (source, sink) = getConstSourceSink("test5")
      sink.reachableBy(source).size shouldBe 0
    }
  }
}
