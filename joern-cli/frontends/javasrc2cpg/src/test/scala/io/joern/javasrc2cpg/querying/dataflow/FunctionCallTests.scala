package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.{JavaDataflowFixture, JavaSrcCode2CpgFixture}
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class NewFunctionCallTests extends JavaSrcCode2CpgFixture(withOssDataflow = true) {
  "Dataflow through function calls" should {

    "allow traversing through a method multiple times" in {
      val cpg = code("""
          |class Foo{
          |    String name;
          |    String getName() {
          |        return name;
          |    }
          |}
          |class Main{
          |    public void bar(Foo foo) {
          |        check(foo.getName());
          |        sink(foo.getName());
          |    }
          |}
          |""".stripMargin)

      def source = cpg.method("bar").parameter.name("foo")
      def sink   = cpg.method("sink").parameter.index(1)
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path directly via a function argument" in {
      val cpg = code("""
          |class Foo {
          |  public static void printSimpleString(String s) {
          |    System.out.println(s);
          |  }
          |
          |  public static void test() {
          |    printSimpleString("MALICIOUS");
          |  }
          |}
          |""".stripMargin)

      val (source, sink) = getMultiFnSourceSink(cpg, "test", "printSimpleString")
      sink.reachableBy(source).size shouldBe 1
    }

    "find paths through calls with varargs" when {
      val cpg = code("""
          |public class Foo {
          |  public static void sink(String s) {}
          |
          |  public static void processAll(String item, String... moreItems) {
          |    sink(item);
          |    for (int i = 0; i < moreItems.length; i++) {
          |      sink(moreItems[i]);
          |    }
          |  }
          |
          |  public static void test(String item0, String item1, String item2) {
          |    processAll(item0, item1, item2);
          |  }
          |}
          |""".stripMargin)

      def sink = cpg.method.name("sink").parameter.l

      "the source is not passed as a vararg argument" in {
        def source = cpg.method.name("test").parameter.name("item0")

        sink.reachableBy(source).size shouldBe 1
        sink.reachableByFlows(source).size shouldBe 1
      }

      "the source is the first vararg argument" in {
        def source = cpg.method.name("test").parameter.name("item1")

        sink.reachableBy(source).size shouldBe 1
        sink.reachableByFlows(source).size shouldBe 1
      }

      "the source is the second vararg argument" in {
        def source = cpg.method.name("test").parameter.name("item2")

        sink.reachableBy(source).size shouldBe 1
        sink.reachableByFlows(source).size shouldBe 1
      }
    }
  }
}

class FunctionCallTests extends JavaDataflowFixture {

  behavior of "Dataflow through function calls"

  override val code: String =
    """
      |public class Foo {
      |    public static void printSimpleString(String s) {
      |        System.out.println(s);
      |    }
      |
      |    public static void printStringReassign(String s) {
      |        String t = s;
      |        System.out.println(t);
      |    }
      |
      |    public static void printStringPrefix(String s) {
      |        String prefix = "SAFE";
      |        String output = prefix + s;
      |        System.out.println(output);
      |    }
      |
      |    public static void cat(String s, String t) {
      |        String output = s + t;
      |        System.out.println(output);
      |    }
      |
      |    public static void first(String s, String t) {
      |        System.out.println(s);
      |    }
      |
      |    public static void second(String s, String t) {
      |        System.out.println(t);
      |    }
      |
      |    public static String getMalicious() {
      |        return "MALICIOUS";
      |    }
      |
      |    public static String join(String s, String t) {
      |        return s + t;
      |    }
      |
      |    public static void depth1(String s) {
      |        depth2(s);
      |    }
      |
      |    public static void depth2(String s) {
      |        printSimpleString(s);
      |    }
      |
      |    public static void overwrite(String s) {
      |        s = "SAFE";
      |        System.out.println(s);
      |    }
      |
      |    public static String safeReturn(String s) {
      |        return "SAFE";
      |    }
      |
      |    public static void test1() {
      |        printSimpleString("MALICIOUS");
      |    }
      |
      |    public static void test2() {
      |        String s = "MALICIOUS";
      |        printSimpleString(s);
      |    }
      |
      |    public static void test3(String prefix) {
      |        String s = "MALICIOUS";
      |        printSimpleString(prefix + s);
      |    }
      |
      |    public static void test4() {
      |        String s = "MALICIOUS";
      |        printStringReassign(s);
      |    }
      |
      |    public static void test5() {
      |        String s = "MALICIOUS";
      |        printStringPrefix(s);
      |    }
      |
      |    public static void test6() {
      |        String s = "MALICIOUS";
      |        depth1(s);
      |    }
      |
      |    public static void test7() {
      |        cat("SAFE", "MALICIOUS");
      |    }
      |
      |    public static void test8() {
      |        cat("MALICIOUS", "SAFE");
      |    }
      |
      |    public static void test9() {
      |        first("MALICIOUS", "SAFE");
      |    }
      |
      |    public static void test10() {
      |        first("SAFE", "MALICIOUS");
      |    }
      |
      |    public static void test11() {
      |        second("MALICIOUS", "SAFE");
      |    }
      |
      |    public static void test12() {
      |        second("SAFE", "MALICIOUS");
      |    }
      |
      |    public static void test13() {
      |        String s = getMalicious();
      |        System.out.println(s);
      |    }
      |
      |    public static void test14() {
      |        String bad = "MALICIOUS";
      |        String s = join(bad, "SAFE");
      |        System.out.println(s);
      |    }
      |
      |    public static void test15() {
      |        String s = "MALICIOUS";
      |        overwrite(s);
      |    }
      |
      |    public static void test16() {
      |        String s = "MALICIOUS";
      |        String t = safeReturn(s);
      |        System.out.println(t);
      |    }
      |
      |    public static void test17(Object o) {
      |        String s = (String) o;
      |        System.out.println(s);
      |    }
      |}
      |""".stripMargin

  it should "find a path directly via a function argument" in {
    val (source, sink) = getMultiFnSourceSink("test1", "printSimpleString")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path via a variable and function argument" in {
    val (source, sink) = getMultiFnSourceSink("test2", "printSimpleString")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path with an operation as an argument" in {
    val (source, sink) = getMultiFnSourceSink("test3", "printSimpleString")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path when the parameter is reassigned" in {
    val (source, sink) = getMultiFnSourceSink("test4", "printStringReassign")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path when a prefix is prepended to the parameter" in {
    val (source, sink) = getMultiFnSourceSink("test5", "printStringPrefix")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path of depth 3" in {
    val (source, sink) = getMultiFnSourceSink("test6", "printSimpleString")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where a `MALICIOUS` second parameter is cat'd with a `SAFE` first" in {
    val (source, sink) = getMultiFnSourceSink("test7", "cat")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where a `MALICIOUS` first parameter is cat'd with a `SAFE` second" in {
    val (source, sink) = getMultiFnSourceSink("test8", "cat")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where the `MALICIOUS` first parameter is printed" in {
    val (source, sink) = getMultiFnSourceSink("test9", "first")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where the `MALICIOUS` second parameter is not printed" in {
    val (source, sink) = getMultiFnSourceSink("test10", "first")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path where the `MALICIOUS` first parameter is not printed" in {
    val (source, sink) = getMultiFnSourceSink("test11", "second")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path where the `MALICIOUS` second parameter is printed" in {
    val (source, sink) = getMultiFnSourceSink("test12", "second")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where `MALICIOUS` is returned directly from a called function" in {
    val (source, sink) = getMultiFnSourceSink("getMalicious", "test13")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path where `MALICIOUS` is added to safe input via a called function" in {
    val (source, sink) = getConstSourceSink("test14")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "not find a path where the `MALICIOUS` arg is overwritten before the sink" in {
    val (source, sink) = getMultiFnSourceSink("test15", "overwrite")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path where `MALICIOUS` arg is not included in return" in {
    val (source, sink) = getConstSourceSink("test16")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path through a cast expression" in {
    def source = cpg.method.name("test17").parameter.index(1)
    def sink   = cpg.method.name("test17").methodReturn
    sink.reachableBy(source).size shouldBe 1
  }
}
