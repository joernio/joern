package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.toExtendedCfgNode
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg

class ObjectTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through objects" should {

    lazy implicit val cpg: Cpg = code("""
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
        |class Foo {
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
        |class Baz {
        |    public String value;
        |
        |    public Baz(String s) {
        |        value = s;
        |    }
        |
        |    public String toString() {
        |        return value;
        |    }
        |
        |    public static void sink(Baz b) {
        |        System.out.println(b.toString());
        |    }
        |
        |    public void test11() {
        |        Baz b = new Baz("MALICIOUS");
        |        sink(b);
        |    }
        |
        |    public void test12() {
        |        sink(new Baz("MALICIOUS"));
        |    }
        |}
        |""".stripMargin)

    "find a path through the constructor and field of an object" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if a safe field is accessed (approximation)" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if a field is directly reassigned to `MALICIOUS`" in {
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path for malicious input via a getter" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path when accessing a safe field via a getter" in {
      val (source, sink) = getConstSourceSink("test5")
      // TODO: This should not find a path, but does due to over-tainting.
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path to a void printer via a field" in {
      val (source, sink) = getMultiFnSourceSink("test6", "printS")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path to a void printer via a safe field" in {
      val (source, sink) = getMultiFnSourceSink("test7", "printT")
      // TODO: The data flow appears to be object-field insensitive and taints the whole object instance
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if `MALICIOUS` is overwritten via a setter" in {
      val (source, sink) = getConstSourceSink("test8")
      // TODO: The data flow appears to be object-field insensitive and taints the whole object instance
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path via an alias" in {
      val (source, sink) = getConstSourceSink("test9")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if a field is reassigned to `MALICIOUS` via an alias" in {
      val (source, sink) = getConstSourceSink("test10")
      // TODO: The data flow appears to be alias insensitive and taints the whole object instance
      sink.reachableBy(source).size shouldBe 0
    }

    // TODO this isn't supported yet
    //  "find a inter-procedural path from object variable" in {
    //    def source = cpg.method.name("test11").literal.code("\"MALICIOUS\"")
    //    def sink   = cpg.method.name("sink").call.name("println").argument
    //    // This matches two since both the variable holding System.out and MALICIOUS are considered tainted at this point
    //    sink.reachableBy(source).size shouldBe 2
    //    sink.reachableByFlows(source).size shouldBe 2
    //  }
    //
    //  "find a inter-procedural path from object instantiation in call argument" in {
    //    def source = cpg.method.name("test12").literal.code("\"MALICIOUS\"")
    //    def sink   = cpg.method.name("sink").call.name("println").argument
    //    // This matches two since both the variable holding System.out and MALICIOUS are considered tainted at this point
    //    sink.reachableBy(source).size shouldBe 2
    //    sink.reachableByFlows(source).size shouldBe 2
    //  }
  }
}
