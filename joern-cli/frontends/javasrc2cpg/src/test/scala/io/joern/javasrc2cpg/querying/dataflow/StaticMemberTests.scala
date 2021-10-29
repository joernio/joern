package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal

/**
  * These tests are added as a wishlist for static member accesses. These
  * results are consistent with static members in C++ using c2cgp, however.
  * For practical reasons, only handling `final` static members is probably
  * the way to go, so at least the first 2 tests should pass.
  *
  * TODO: Fix dataflow from static members, treating them as final.
  */
class StaticMemberTests extends JavaDataflowFixture {

  behavior of "Dataflow from static members"

  override val code: String =
    """
      |class Bar {
      |    public static String bad = "MALICIOUS";
      |    public static String good = "SAFE";
      |
      |}
      |
      |public class Foo {
      |    public static String good = "MALICIOUS";
      |    public static String bad = "SAFE";
      |
      |    public void test1() {
      |        String s = Bar.bad;
      |        System.out.println(s);
      |    }
      |
      |    public void test2() {
      |        System.out.println(Bar.bad);
      |    }
      |
      |    public void test3() {
      |        System.out.println(Bar.good);
      |    }
      |
      |    public void test4() {
      |        System.out.println(Foo.good);
      |    }
      |
      |    public void test5() {
      |        System.out.println(Foo.bad);
      |    }
      |
      |    public void test6() {
      |        Bar.bad = "SAFE";
      |        System.out.println(Bar.bad);
      |    }
      |
      |    public void test7() {
      |        Bar.good = "MALICIOUS";
      |        System.out.println(Bar.good);
      |    }
      |}
      |""".stripMargin

  private def getSources = {
    val sources = cpg.literal.code("\"MALICIOUS\"").l
    if (sources.size <= 0) {
      fail("Could not find any sources")
    }
    Traversal.from(sources)
  }

  it should "find a path for `MALICIOUS` data from different class via a variable" in {
    val source = getSources
    val sink = cpg.method(".*test1.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path for `MALICIOUS` data from a different class directly" in {
    val source = getSources
    val sink = cpg.method(".*test2.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path for `SAFE` data directly" in {
    val source = getSources
    val sink = cpg.method(".*test3.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path for `MALICIOUS` data from the same class" in {
    val source = getSources
    val sink = cpg.method(".*test4.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path for `SAFE` data in the same class" in {
    val source = getSources
    val sink = cpg.method(".*test5.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "not find a path for overwritten `MALICIOUS` data" in {
    val source = getSources
    val sink = cpg.method(".*test6.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 0
  }

  it should "find a path for overwritten `SAFE` data" in {
    val source = getSources
    val sink = cpg.method(".*test7.*").call.name(".*println.*").argument

    sink.reachableBy(source).size shouldBe 1
  }
}
