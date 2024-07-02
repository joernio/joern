package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.joern.dataflowengineoss.language.*

import scala.jdk.CollectionConverters.*
class ArrayTests extends JavaDataflowFixture {

  behavior of "Dataflow through arrays"

  override val code: String =
    """
      |public class Foo {
      |    public void test1() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        System.out.println(vals[2]);
      |    }
      |
      |    public void test2() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        System.out.println(vals[0]);
      |    }
      |
      |    public void test3() {
      |        String[] vals = new String[]{"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        System.out.println(vals[2]);
      |    }
      |
      |    public void test4() {
      |        String[] vals = new String[2];
      |        vals[0] = "SAFE";
      |        vals[1] = "MALICIOUS";
      |        System.out.println(vals[0]);
      |    }
      |
      |    public void test5() {
      |        String[] vals = new String[2];
      |        vals[0] = "SAFE";
      |        vals[1] = "MALICIOUS";
      |        System.out.println(vals[1]);
      |    }
      |
      |    public void test6() {
      |        String[] vals = {"SAFE", "MALICIOUS"};
      |        vals[0] = "ALSO SAFE";
      |        System.out.println(vals[1]);
      |    }
      |
      |    public void test7() {
      |        String[] vals = {"SAFE", "MALICIOUS"};
      |        vals[1] = "ALSO SAFE";
      |        System.out.println(vals[1]);
      |    }
      |
      |    public void test8() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        for (int i = 0; i < vals.length; i++) {
      |            String val = vals[i];
      |            System.out.println(val);
      |        }
      |    }
      |
      |    public void test9() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        for (String val : vals) {
      |            System.out.println(val);
      |        }
      |    }
      |
      |    public void test10() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        String acc = "";
      |        for (String val : vals) {
      |            acc += val;
      |        }
      |        System.out.println(acc);
      |    }
      |
      |    public void test11() {
      |        String[] vals = {"SAFE", "STILL SAFE", "ALSO SAFE"};
      |        vals[1] = "MALICIOUS";
      |        System.out.println(vals[1]);
      |    }
      |
      |    public void test12() {
      |        String[] vals = {"SAFE", "STILL SAFE", "ALSO SAFE"};
      |        vals[1] = "MALICIOUS";
      |        System.out.println(vals[0]);
      |    }
      |
      |    public void test13() {
      |        String[] vals = {"SAFE", "SAFE", "MALICIOUS", "SAFE"};
      |        String[] alias = vals;
      |        System.out.println(alias[2]);
      |    }
      |   }
      |""".stripMargin

  it should "find a path if the `MALICIOUS` array entry is printed" in {
    val (source, sink) = getConstSourceSink("test1")
    // This is 2 due to how the sink is constructed (it finds both path to the indexAccess call
    // and the vals identifier. The same applies to following tests.
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if an entry in the `MALICIOUS` array is printed (approximation)" in {
    val (source, sink) = getConstSourceSink("test2")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path for alternative array initializer syntax" in {
    val (source, sink) = getConstSourceSink("test3")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if array element is assigned to `MALICIOUS` (approximation)" in {
    val (source, sink) = getConstSourceSink("test4")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if array element is assigned to `MALICIOUS`" in {
    val (source, sink) = getConstSourceSink("test5")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if a different array element is overwritten" in {
    val (source, sink) = getConstSourceSink("test6")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if the `MALICIOUS` array element is overwritten (approximation)" in {
    val (source, sink) = getConstSourceSink("test7")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if sink is in a `FOR` loop" in {
    val (source, sink) = getConstSourceSink("test8")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if sink is in a `FOREACH` loop over `MALICIOUS` array" in {
    val (source, sink) = getConstSourceSink("test9")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is added to an accumulator in a loop" in {
    val (source, sink) = getConstSourceSink("test10")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path if `MALICIOUS` is assigned to safe array and printed" in {
    val (source, sink) = getConstSourceSink("test11")
    sink.reachableBy(source).size shouldBe 2
  }

  it should "find a path if `MALICIOUS` is assigned to safe array and not printed (approximation)" in {
    val (source, sink) = getConstSourceSink("test12")
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find a path through an array alias" in {
    val (source, sink) = getConstSourceSink("test13")
    sink.reachableBy(source).size shouldBe 2
  }
}
