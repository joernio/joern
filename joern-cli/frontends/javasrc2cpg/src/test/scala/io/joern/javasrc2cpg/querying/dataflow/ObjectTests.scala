package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.{JavaDataflowFixture, JavaSrcCode2CpgFixture}
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class NewObjectTests extends JavaSrcCode2CpgFixture(withOssDataflow = true) {

  "static field passed as an argument inside a same-class static method whilst being referenced by its simple name" in {
    val cpg = code("""
        |class Bar {
        | static String CONST = "<const>";
        | static void run() {
        |   System.out.println(CONST);
        | }
        |}""".stripMargin)
    val sink   = cpg.call("println").argument(1)
    val source = cpg.literal
    sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List(
      List(("String Bar.CONST = \"<const>\"", Some(3)), ("System.out.println(CONST)", Some(5)))
    )
  }

  "static field passed as an argument inside a same-class static method whilst being referenced by its qualified name" in {
    val cpg = code("""
        |class Bar {
        | static String CONST = "<const>";
        | static void run() {
        |   System.out.println(Bar.CONST);
        | }
        |}""".stripMargin)
    val sink   = cpg.call("println").argument(1)
    val source = cpg.literal
    sink.reachableByFlows(source).map(flowToResultPairs).l shouldBe List(
      List(("String Bar.CONST = \"<const>\"", Some(3)), ("System.out.println(Bar.CONST)", Some(5)))
    )
  }
}

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
      |""".stripMargin

  it should "find a path through the constructor and field of an object" in {
    val source = cpg.method("test1").literal
    val sink   = cpg.method("test1").ast.isCall.name("println").argument(1).l
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if a safe field is accessed (approximation)" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if a field is directly reassigned to `MALICIOUS`" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path for malicious input via a getter" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "not find a path when accessing a safe field via a getter" in {
    val (source, sink) = getConstSourceSink("test5")
    // TODO: This should not find a path, but does due to over-tainting.
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path to a void printer via a field" in {
    val (source, sink) = getMultiFnSourceSink("test6", "printS")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "not find a path to a void printer via a safe field" in {
    val (source, sink) = getMultiFnSourceSink("test7", "printT")
    // TODO: This should not find a path, but does due to over-tainting.
    sink.reachableBy(source).size shouldBe 2
  }

  it should "not find a path if `MALICIOUS` is overwritten via a setter" in {
    val (source, sink) = getConstSourceSink("test8")
    pendingUntilFixed(sink.reachableBy(source).size shouldBe 0)
  }

  it should "find a path via an alias" in {
    val (source, sink) = getConstSourceSink("test9")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if a field is reassigned to `MALICIOUS` via an alias" in {
    val (source, sink) = getConstSourceSink("test10")
    // TODO: This should find a path, but the current result is on par with c2cpg.
    sink.reachableBy(source).size shouldBe 0
  }

  // TODO this isn't supported yet
//  it should "find a inter-procedural path from object variable" in {
//    def source = cpg.method.name("test11").literal.code("\"MALICIOUS\"")
//    def sink   = cpg.method.name("sink").call.name("println").argument
//
//    sink.reachableBy(source).size shouldBe 2
//    sink.reachableByFlows(source).size shouldBe 2
//  }

  it should "not create Baz method with ANY type in signature" in {
    cpg.method.fullNameExact("Baz.sink:void(ANY)").size shouldBe 0
  }

  // TODO this isn't supported yet
//  it should "find a inter-procedural path from object instantiation in call argument" in {
//    def source = cpg.method.name("test12").literal.code("\"MALICIOUS\"")
//    def sink   = cpg.method.name("sink").call.name("println").argument
//
//    sink.reachableBy(source).size shouldBe 2
//    sink.reachableByFlows(source).size shouldBe 2
//  }
}
