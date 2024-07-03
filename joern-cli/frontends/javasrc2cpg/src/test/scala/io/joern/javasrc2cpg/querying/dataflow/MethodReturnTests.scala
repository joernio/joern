package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.semanticcpg.language.*
import io.joern.dataflowengineoss.language.*

class MethodReturnTests extends JavaDataflowFixture {

  behavior of "Dataflow to method return"

  override val code: String = """
      |public class Foo {
      | public void foo(int y) {
      |   int x = 10;
      | }
      |
      | public void bar() {
      |   bar(foo(1));
      | }
      |
      | public void woo() {
      |   int x = 20;
      |   System.out.println(1, x);
      |   sink(x);
      | }
      |
      |}
      |""".stripMargin

  it should "find flow from x to METHOD_RETURN (exit node)" in {
    val src = cpg.identifier.name("x")
    val snk = cpg.method("foo").methodReturn
    snk.reachableBy(src).size shouldBe 1
  }

  it should "not find a flow from x to bar's argument" in {
    val src = cpg.identifier("x")
    val snk = cpg.method("bar").parameter.index(1)
    snk.reachableBy(src).size shouldBe 0
  }

  it should "not find a flow from y to bar's argument" in {
    val src = cpg.parameter("y")
    val snk = cpg.method("bar").parameter.index(1)
    snk.reachableBy(src).size shouldBe 0
  }

  it should "find a flow passed an external method with semantic" in {
    val src = cpg.literal.code("20")
    val snk = cpg.method("sink").parameter.index(1)
    snk.reachableBy(src).size shouldBe 1
  }

}
