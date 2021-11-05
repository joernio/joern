package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.dataflowengineoss.language._

class IfTests extends JavaDataflowFixture {
  behavior of "Dataflow through IF structures"

  override val code: String = """
    |public class Foo {
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
    |""".stripMargin

  it should "find a path if `MALICIOUS` is reassigned to `SAFE` in only one path of an if" in {
    val (source, sink) = getConstSourceSink("test1")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `SAFE` is reassigned to `MALICIOUS` in at least one path of an if" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is assigned in at least one path of an if" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is assigned in a chained if" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "not find a path if `MALICIOUS` is reassigned in all paths of an if" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 0
  }
}
