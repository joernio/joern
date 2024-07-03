package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.joern.dataflowengineoss.language.*

class TryTests extends JavaDataflowFixture {

  behavior of "Dataflow through TRY/CATCH"

  override val code: String =
    """
      |public class Foo {
      |    public static void foo() {
      |        throw new Exception();
      |    }
      |
      |    public void test1() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            System.out.println(s);
      |        } catch (Exception e) {
      |            System.out.println("SAFE");
      |        }
      |    }
      |
      |    public void test2() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            System.out.println("SAFE");
      |            foo();
      |        } catch (Exception e) {
      |            System.out.println(s);
      |        }
      |    }
      |
      |    public void test3() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            System.out.println("SAFE");
      |        } catch (Exception e) {
      |            System.out.println("ALSO_SAFE");
      |        } finally {
      |            System.out.println(s);
      |        }
      |    }
      |
      |    public void test4() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            throw new Exception(s);
      |        } catch (Exception e) {
      |            System.out.println(e);
      |        }
      |    }
      |
      |    public void test5() {
      |        String s = "SAFE";
      |
      |        try {
      |            s = "MALICIOUS";
      |        } catch (Exception e) {
      |            // Do nothing
      |        }
      |
      |        System.out.println(s);
      |    }
      |
      |    public void test6() {
      |        String s = "SAFE";
      |
      |        try {
      |            foo();
      |        } catch (Exception e) {
      |            s = "MALICIOUS";
      |        }
      |
      |        System.out.println(s);
      |    }
      |
      |    public void test7() {
      |        String s = "SAFE";
      |
      |        try {
      |            foo();
      |        } catch (Exception e) {
      |            // Do nothing
      |        } finally {
      |            s = "MALICIOUS";
      |        }
      |
      |        System.out.println(s);
      |    }
      |
      |    public void test8() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            s = "SAFE";
      |        } catch (Exception e) {
      |            s = "ALSO SAFE";
      |        }
      |
      |        System.out.println(s);
      |    }
      |
      |    public void test9() {
      |        String s = "MALICIOUS";
      |
      |        try {
      |            s = "MALICIOUS";
      |        } catch (Exception e) {
      |            s = "MALICIOUS";
      |        } finally {
      |            s = "SAFE";
      |        }
      |
      |        System.out.println(s);
      |    }
      |
      |    public static int tryWithExplicitReturn(String args) {
      |        try {
      |            var x = 1;
      |            System.out.println("in begin");
      |            return x;
      |         } catch (Exception e) {
      |            System.out.println("Something went wrong." + args);
      |         }
      |         return 1;
      |    }
      |
      |    public static void test10(String[] args) {
      |       String s = "MALICIOUS";
      |       tryWithExplicitReturn(s);
      |    }
      |
      |}
      |""".stripMargin

  it should "find a path if the sink is in a `TRY`" in {
    val (source, sink) = getConstSourceSink("test1")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if the sink is in a `CATCH`" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if the sink is in a `FINALLY`" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 1
  }

  // TODO: This is a very optimistic test. We expect the path to be missing for now.
  it should "find a path if `MALICIOUS` is contained in thrown string with sink in catch" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path if `MALICIOUS` is assigned in `TRY`" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is assigned in `CATCH`" in {
    val (source, sink) = getConstSourceSink("test6")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is assigned in `FINALLY`" in {
    val (source, sink) = getConstSourceSink("test7")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "not find a path if `MALICIOUS` is reassigned in both TRY/CATCH" in {
    val (source, sink) = getConstSourceSink("test8")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path if `MALICIOUS` is reassigned in FINALLY" in {
    val (source, sink) = getConstSourceSink("test9")
    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path if `MALICIOUS` is given to a call in CATCH" in {
    val (source, sink) = getMultiFnSourceSink("test10", "tryWithExplicitReturn")
    sink.reachableBy(source).size shouldBe 2
  }
}
