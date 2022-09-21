package io.joern.javasrc2cpg.querying.dataflow

import io.joern.javasrc2cpg.testfixtures.JavaDataflowFixture
import overflowdb.traversal._
import io.joern.dataflowengineoss.language._
import io.shiftleft.semanticcpg.language._

class MemberTests extends JavaDataflowFixture {

  behavior of "Dataflow from non-static members"

  override val code: String =
    """
      | class Foo {
      |
      |   int x = "abc";
      |
      |   public void foo() {
      |     sink(x);
      |   }
      | }
      |""".stripMargin

  it should "find flow from member to sink" in {
    val sink   = cpg.call("sink").argument(1).l
    val source = cpg.member.name("x").l
    sink.size shouldBe 1
    source.size shouldBe 1
    sink.reachableBy(source).size shouldBe 1
  }

  it should "find flow from literal to sink" in {
    val sink   = cpg.call("sink").argument(1).l
    val source = cpg.literal.code("\"abc\"").l
    sink.size shouldBe 1
    source.size shouldBe 1
    sink.reachableBy(source).size shouldBe 1
  }

}
