package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg

class OperatorTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through operators" should {

    lazy implicit val cpg: Cpg = code("""
        |class Foo {
        |
        |  private String safe = "";
        |
        |  public void test1() {
        |    String s = "MALICIOUS";
        |    System.out.println(s);
        |  }
        |
        |  public void test2() {
        |    int x = 42;
        |    int y = x;
        |    int z = y;
        |    System.out.println(z);
        |  }
        |
        |  public void test3(String suffix) {
        |    String s = "MALICIOUS";
        |    String t = s + safe;
        |    System.out.println(t);
        |  }
        |
        |  public void test4(boolean shouldToggle) {
        |    String bad = "MALICIOUS";
        |    String s = shouldToggle ? "SAFE" : bad;
        |
        |    System.out.println(s);
        |  }
        |
        |  public void test5() {
        |    int bad = 42;
        |    int good = 0;
        |    int veryGood = 11;
        |
        |    int maybeBad = good + (veryGood + bad);
        |    System.out.println(maybeBad);
        |  }
        |
        |  public void test6() {
        |    String s = "MALICIOUS";
        |    s = "SAFE";
        |    System.out.println(s);
        |  }
        |
        |  public void test7() {
        |    String s = "SAFE";
        |    s += "MALICIOUS";
        |    System.out.println(s);
        |  }
        |
        |  public void test8() {
        |     String s = "MALICIOUS";
        |     s += "SAFE";
        |     System.out.println(s);
        |  }
        |
        |  public void test9() {
        |    String s = "SAFE";
        |    s = "MALICIOUS";
        |    System.out.println(s);
        |  }
        |}
        |""".stripMargin)

    "track dataflow through direct assignment" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "track dataflow through multiple assignments" in {
      val (source, sink) = getConstSourceSink("test2", sourceCode = "42")
      sink.reachableBy(source).size shouldBe 1
    }

    "track dataflow through a binary operation" in {
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 1
    }

    "track dataflow through a conditional expression" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 1
    }

    "track dataflow through nested operations" in {
      val (source, sink) = getConstSourceSink("test5", sourceCode = "42")
      sink.reachableBy(source).size shouldBe 1
    }

    "not track dataflow through a reassignment" in {
      val (source, sink) = getConstSourceSink("test6")
      sink.reachableBy(source).size shouldBe 0
    }

    "track dataflow through += where malicious input is added" in {
      // Due to String concats being handled by Soot dummy methods we need to add wildcards to the source
      val (source, sink) = getConstSourceSink("test7", ".*MALICIOUS.*")
      sink.reachableBy(source).size shouldBe 1
    }

    "track dataflow through += where safe input is added" in {
      val (source, sink) = getConstSourceSink("test8")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if safe is reassigned to malicious" in {
      val (source, sink) = getConstSourceSink("test9")
      sink.reachableBy(source).size shouldBe 1
    }
  }
}
