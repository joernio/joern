package io.joern.javasrc2cpg.querying.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.shiftleft.semanticcpg.language.*

class ReturnTests extends JavaDataflowFixture {

  behavior of "Dataflow to return statements"

  override val code: String =
    """
      |public class Foo {
      | public int case1() {
      |   int x = 42;
      |   return x;
      | }
      |
      | public Baz case2() {
      |   int x = 42;
      |   return new Baz(x);
      | }
      |
      | public void case2_sink() {
      |   sink(case2());
      | }
      |}
      |""".stripMargin

  it should "find a flow when returning a single variable" in {
    def src = cpg.method("case1").literal
    def snk = cpg.method("case1").methodReturn.toReturn
    snk.reachableBy(src).size shouldBe 1
  }

  it should "find a flow when returning an object instantiation 1" in {
    def src = cpg.method("case2").literal
    def snk = cpg.method("case2").methodReturn.toReturn
    snk.reachableByFlows(src).size shouldBe 1
  }

  it should "find a flow when returning an object instantiation 2" in {
    def src = cpg.method("case2").literal
    def snk = cpg.call("sink")
    snk.reachableByFlows(src).size shouldBe 1
  }
}
