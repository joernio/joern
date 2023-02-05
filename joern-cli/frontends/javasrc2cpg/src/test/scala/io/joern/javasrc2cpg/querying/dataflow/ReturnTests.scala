package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class ReturnTests extends JavaDataflowFixture {

  behavior of "Dataflow to return statements"

  override val code: String =
    """
      |public class Foo {
      | public void bar() {
      |   int x = 42;
      |   return x;
      | }
      |}
      |""".stripMargin

  it should "find a flow from x in return statement to 42" in {
    val src = cpg.literal("42")
    val snk = cpg.identifier("x").lineNumber(5)
    snk.reachableBy(src).size shouldBe 1
  }
}
