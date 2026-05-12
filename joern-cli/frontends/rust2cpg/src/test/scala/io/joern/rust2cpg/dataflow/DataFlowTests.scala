package io.joern.rust2cpg.dataflow

import io.joern.dataflowengineoss.language.*
import io.joern.rust2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language.*

class DataFlowTests extends DataFlowCodeToCpgSuite(noSysRoot = true) {

  "flow from function parameter to method return" in {
    val cpg = code("""
        |fn add_one(x: i32) -> i32 {
        |  let y = x + 1;
        |  y
        |}
        |""".stripMargin)

    val source = cpg.method.name("add_one").parameter.name("x")
    val sink   = cpg.method.name("add_one").methodReturn
    val flows  = sink.reachableByFlows(source).map(flowToResultPairs).l

    flows shouldBe List(List(("add_one(x: i32)", 2), ("x + 1", 3), ("let y = x + 1;", 3), ("y", 4), ("RET", 2)))
  }

  "flow from literal through `let` to call argument" in {
    val cpg = code("""
        |fn main() {
        |  let x = 42;
        |  sink(x);
        |}
        |""".stripMargin)

    val source = cpg.literal("42")
    val sink   = cpg.call.name("sink").argument
    val flows  = sink.reachableByFlows(source).map(flowToResultPairs).l

    flows shouldBe List(List(("let x = 42;", 3), ("sink(x)", 4)))
  }

  "flow through both `if/else` branches reaches the sink" in {
    val cpg = code("""
        |fn main(cond: bool) {
        |  let mut x = 0;
        |  if cond {
        |    x = 1;
        |  } else {
        |    x = 2;
        |  }
        |  sink(x);
        |}
        |""".stripMargin)

    val source = cpg.literal.filter(l => l.code == "1" || l.code == "2")
    val sink   = cpg.call.name("sink").argument
    val flows  = sink.reachableByFlows(source).map(flowToResultPairs).toSet

    flows shouldBe Set(List(("x = 1", 5), ("sink(x)", 9)), List(("x = 2", 7), ("sink(x)", 9)))
  }

  "flow from function parameter through same-crate call to sink" in {
    val cpg = code("""
        |fn forward(p: i32) -> i32 { p }
        |
        |fn main(tainted: i32) {
        |  let x = forward(tainted);
        |  sink(x);
        |}
        |""".stripMargin)

    val source = cpg.method.name("main").parameter.name("tainted")
    val sink   = cpg.call.name("sink").argument
    val flows  = sink.reachableByFlows(source).map(flowToResultPairs).l

    flows shouldBe List(
      List(
        ("main(tainted: i32)", 4),
        ("forward(tainted)", 5),
        ("forward(p: i32)", 2),
        ("p", 2),
        ("RET", 2),
        ("forward(tainted)", 5),
        ("let x = forward(tainted);", 5),
        ("sink(x)", 6)
      )
    )
  }
}
