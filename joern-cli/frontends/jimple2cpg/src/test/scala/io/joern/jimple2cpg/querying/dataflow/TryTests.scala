package io.joern.jimple2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.jimple2cpg.testfixtures.JimpleDataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class TryTests extends JimpleDataFlowCodeToCpgSuite {

  "dataflow through TRY/CATCH" should {

    lazy implicit val cpg: Cpg = code("""
        |class Foo {
        |    public static void foo() throws Exception {
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
        |    public void spooky() throws Exception {
        |    }
        |
        |    public void test8() {
        |        String s = "MALICIOUS";
        |
        |        try {
        |            s = "SAFE";
        |            spooky();
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
        |    public void test10() {
        |        String s = "MALICIOUS";
        |
        |        try {
        |            spooky();
        |            s = "SAFE";
        |            try {
        |               s = "MALICIOUS";
        |            } finally {
        |               s = "SAFE";
        |            }
        |        } catch (Exception e) {
        |            s = "SAFE";
        |        }
        |
        |        System.out.println(s);
        |    }
        |
        |    public void test11() {
        |        String s = "MALICIOUS";
        |
        |        try {
        |            spooky();
        |            s = "SAFE";
        |            try {
        |               s = "MALICIOUS";
        |            } finally {
        |               s = "SAFE";
        |            }
        |        } catch (Exception e) {
        |        }
        |
        |        System.out.println(s);
        |    }
        |}
        |""".stripMargin)

    "find a path if the sink is in a `TRY`" in {
      val (source, sink) = getConstSourceSink("test1")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if the sink is in a `CATCH`" in {
      val (source, sink) = getConstSourceSink("test2")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if the sink is in a `FINALLY`" in {
      // Jimple's flat AST evaluates multiple paths that will end at the FINALLY sink but it is a conservative result
      val (source, sink) = getConstSourceSink("test3")
      sink.reachableBy(source).size shouldBe 3
    }

    // TODO: This is a very optimistic test. We expect the path to be missing for now.
    "find a path if `MALICIOUS` is contained in thrown string with sink in catch" in {
      val (source, sink) = getConstSourceSink("test4")
      sink.reachableBy(source).size shouldBe 0
    }

    "find a path if `MALICIOUS` is assigned in `TRY`" in {
      val (source, sink) = getConstSourceSink("test5")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is assigned in `CATCH`" in {
      val (source, sink) = getConstSourceSink("test6")
      sink.reachableBy(source).size shouldBe 1
    }

    "find a path if `MALICIOUS` is assigned in `FINALLY`" in {
      // Jimple's flat AST evaluates multiple paths that will end at the FINALLY sink but it is a conservative result
      val (source, sink) = getConstSourceSink("test7")
      sink.reachableBy(source).size shouldBe 3
    }

    "not find a path if `MALICIOUS` is reassigned in both TRY/CATCH" in {
      val (source, sink) = getConstSourceSink("test8")
      sink.reachableBy(source).size shouldBe 0
    }

    "not find a path if `MALICIOUS` is reassigned in FINALLY" in {
      val (source, sink) = getConstSourceSink("test9")
      sink.reachableBy(source).size shouldBe 0
    }
    "not find a path if `MALICIOUS` has `SAFE` assigned in all paths of nested try-catch" in {
      val (source, sink) = getConstSourceSink("test10")
      sink.reachableBy(source).size shouldBe 0
    }
    "find a path if `MALICIOUS` untouched in outer handler of nested try-catch" in {
      val (source, sink) = getConstSourceSink("test11")
      sink.reachableBy(source).size shouldBe 1
    }
  }
}
