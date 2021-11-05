package io.shiftleft.fuzzyc2cpg.querying

import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.fuzzyc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class NewCDataFlowTests1 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   int x = source();
      |   sink(x);
      | }
      |""".stripMargin

  "should find flow via assignment" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
    )
  }
}

class NewCDataFlowTests2 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   sink(source());
      | }
      |""".stripMargin

  "should find flow of call in call" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(3)), ("sink(source())", Some(3)))
    )
  }
}

class NewCDataFlowTests3 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   woo(x);
      | }
      |""".stripMargin

  "should find flow of call return value to exit node" in {
    val source = cpg.call("woo")
    val sink = cpg.method("foo").methodReturn
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("woo(x)", Some(3)), ("void", Some(2)))
    )
  }
}

class NewCDataFlowTests4 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   x = source();
      |   sink(x);
      | }
      |""".stripMargin

  "should find flow via assignment for global" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink")
    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(3)), ("x = source()", Some(3)), ("sink(x)", Some(4)))
    )
  }
}

class NewCDataFlowTests5 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   int x = source();
      |   x = y;
      |   sink(x);
      | }
      |""".stripMargin

  "should find that flow is blocked by assignment" in {
    val source = cpg.call("source").l
    val assignment = cpg.assignment.codeExact("x = y")
    val sink = cpg.call("sink").l

    val flows = sink.reachableByFlows(source).l
    flows.size shouldBe 0
    val flows2 = sink.reachableByFlows(assignment.target).l
    flows2.size shouldBe 1
    flows2.map(flowToResultPairs).toSet shouldBe Set(
      List(("x = y", Some(4)), ("sink(x)", Some(5)))
    )
  }
}

class NewCDataFlowTests6 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   x.y = source();
      |   sink(x.y);
      | }
      |""".stripMargin

  "should find via assignment with field access" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(3)), ("x.y = source()", Some(3)), ("sink(x.y)", Some(4)))
    )
  }
}

class NewCDataFlowTests7 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   x->y = source();
      |   sink(x->y);
      | }
      |""".stripMargin

  "should find flow via assignment with indirect field access" in {
    val source = cpg.call("source")
    val sink = cpg.call("sink")
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 1
    flows.map(flowToResultPairs).toSet shouldBe Set(
      List(("source()", Some(3)), ("x->y = source()", Some(3)), ("sink(x->y)", Some(4)))
    )
  }
}

class NewCDataFlowTests8 extends DataFlowCodeToCpgSuite {
  override val code =
    """
      | void foo() {
      |   int x.y = source();
      |   x.y = z;
      |   sink(x);
      | }
      |""".stripMargin

  "should find that flow is blocked by assignment" in {
    val source = cpg.call("source").l
    val assignment = cpg.assignment.codeExact("x.y = z")
    val sink = cpg.call("sink").l
    val flows = sink.reachableByFlows(source).l

    flows.size shouldBe 0
    val flows2 = sink.reachableByFlows(assignment.target).l
    flows2.size shouldBe 1
    flows2.map(flowToResultPairs).toSet shouldBe Set(
      List(("x.y = z", Some(4)), ("sink(x)", Some(5)))
    )
  }
}
