package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.dataflowengineoss.language._

class ObjectTests extends JavaDataflowFixture {

  behavior of "Dataflow through objects"

  override val code: String =
    """
      |class Bar {
      |    public String s;
      |    public String t = "SAFE";
      |
      |    public Bar(String s) {
      |        this.s = s;
      |    }
      |
      |    public void setS(String s) {
      |        this.s = s;
      |    }
      |
      |    public void setT(String t) {
      |        this.t = t;
      |    }
      |
      |    public void printS() {
      |        System.out.println(s);
      |    }
      |
      |    public void printT() {
      |        System.out.println(t);
      |    }
      |
      |    public String getS() {
      |        return s;
      |    }
      |
      |    public String getT() {
      |        return t;
      |    }
      |}
      |
      |public class Foo {
      |
      |    public void test1() {
      |        Bar b = new Bar("MALICIOUS");
      |        System.out.println(b.s);
      |    }
      |
      |    public void test2() {
      |        Bar b = new Bar("MALICIOUS");
      |        System.out.println(b.t);
      |    }
      |
      |    public void test3() {
      |        Bar b = new Bar("SAFE");
      |        b.s = "MALICIOUS";
      |        System.out.println(b.s);
      |    }
      |
      |    public void test4() {
      |        Bar b = new Bar("MALICIOUS");
      |        String s = b.getS();
      |        System.out.println(s);
      |    }
      |
      |    public void test5() {
      |        Bar b = new Bar("MALICIOUS");
      |        String s = b.getT();
      |        System.out.println(s);
      |    }
      |
      |    public void test6() {
      |        Bar b = new Bar("MALICIOUS");
      |        b.printS();
      |    }
      |
      |    public void test7() {
      |        Bar b = new Bar("MALICIOUS");
      |        b.printT();
      |    }
      |
      |    public void test8() {
      |        Bar b = new Bar("MALICIOUS");
      |        b.setS("SAFE");
      |        String s = b.s;
      |        System.out.println(s);
      |    }
      |
      |    public void test9() {
      |        Bar b1 = new Bar("MALICIOUS");
      |        Bar b2 = b1;
      |        String s = b2.s;
      |        System.out.println(s);
      |    }
      |
      |    public void test10() {
      |        Bar b1 = new Bar("SAFE");
      |        Bar b2 = b1;
      |        b2.s = "MALICIOUS";
      |        System.out.println(b1.s);
      |    }
      |}
      |
      |""".stripMargin

  it should "find a path through the constructor and field of an object" in {
    val (source, sink) = getConstSourceSink("test1")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if a safe field is accessed (approximation)" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if a field is directly reassigned to `MALICIOUS`" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path for malicious input via a getter" in {
    // TODO: This should find a path, but the current result is on par with c2cpg.
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path when accessing a safe field via a getter" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path to a void printer via a field" in {
    // TODO: This should find a path, but the current result is on par with c2cpg.
    val (source, sink) = getMultiFnSourceSink("test6", "printS")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path to a void printer via a safe field" in {
    val (source, sink) = getMultiFnSourceSink("test7", "printT")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path if `MALICIOUS` is overwritten via a setter" in {
    val (source, sink) = getConstSourceSink("test8")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path via an alias" in {
    val (source, sink) = getConstSourceSink("test9")
    // TODO: This should find a path, but the current result is on par with c2cpg.
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path if a field is reassigned to `MALICIOUS` via an alias" in {
    val (source, sink) = getConstSourceSink("test10")
    // TODO: This should find a path, but the current result is on par with c2cpg.
    sink.reachableBy(source).size shouldBe 0
  }
}
