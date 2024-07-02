package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.jimple2cpg.testfixtures.{JimpleDataFlowCodeToCpgSuite, JimpleDataflowTestCpg}

class ArrayTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through arrays" should {
    lazy implicit val cpg: JimpleDataflowTestCpg = code("""
        |class Foo {
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
        |""".stripMargin)

    "find a path if the `MALICIOUS` array entry is printed" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if an entry in the `MALICIOUS` array is printed (approximation)" in {
      // The reason this false positive occurs is because Jimple makes an alias in here unlike in test4
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path for alternative array initializer syntax" in {
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if an unrelated array element not assigned to `MALICIOUS` reaches the sink" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 0
    }

    "find a path if array element is assigned to `MALICIOUS`" in {
      val (source, sink) = getConstSourceSink("test5")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if a different array element is overwritten" in {
      val (source, sink) = getConstSourceSink("test6")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if the `MALICIOUS` array element is overwritten (approximation)" in {
      // Similarly to test2, this false positive occurs because Jimple makes an alias
      val (source, sink) = getConstSourceSink("test7")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if sink is in a `FOR` loop" in {
      val (source, sink) = getConstSourceSink("test8")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if sink is in a `FOREACH` loop over `MALICIOUS` array" in {
      val (source, sink) = getConstSourceSink("test9")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is added to an accumulator in a loop" in {
      val (source, sink) = getConstSourceSink("test10")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is assigned to safe array and printed" in {
      val (source, sink) = getConstSourceSink("test11")
      sink.reachableBy(source).size shouldBe 1
    }

    "not find a path if `MALICIOUS` is assigned to safe array and not printed" in {
      val (source, sink) = getConstSourceSink("test12")
      sink.reachableBy(source).size shouldBe 0
    }

    "find a path through an array alias" in {
      val (source, sink) = getConstSourceSink("test13")
      sink.reachableBy(source).size shouldBe 1
    }
  }

}
