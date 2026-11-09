package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class ConditionalOperatorDataflowTests extends DataFlowCodeToCpgSuite {

  "conditional operator condition use" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |  int source = 0;
        |  int source_copy = source;
        |  int sink_1 = source_copy;
        |  int status = source_copy ? 1 : 0;
        |  int sink_2 = source_copy;
        |  return 0;
        |}""".stripMargin)

    "still find flow to sink_1" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_1")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source_copy = source", 4), ("sink_1 = source_copy", 5)),
        List(("source = 0", 3), ("source_copy = source", 4), ("sink_1 = source_copy", 5))
      )
    }

    "find flow to sink_2 after ternary condition use" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_2")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source_copy = source", 4), ("sink_2 = source_copy", 7)),
        List(("source = 0", 3), ("source_copy = source", 4), ("sink_2 = source_copy", 7))
      )
    }
  }

  "Statement-form ternary without assignment" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |  int source = 0;
        |  int source_copy = source;
        |  int sink_1 = source_copy;
        |  source_copy ? 1 : 0;
        |  int sink_2 = source_copy;
        |  return 0;
        |}""".stripMargin)

    "find flow to sink_2 after statement ternary" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_2")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source = 0", 3), ("source_copy = source", 4), ("sink_2 = source_copy", 7)),
        List(("source_copy = source", 4), ("sink_2 = source_copy", 7))
      )
    }
  }

  "Logical AND short-circuit condition use" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |  int source = 0;
        |  int source_copy = source;
        |  int sink_1 = source_copy;
        |  int guard = source_copy && 1;
        |  int sink_2 = source_copy;
        |  return 0;
        |}""".stripMargin)

    "find flow to sink_2 after logical and condition use" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_2")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source = 0", 3), ("source_copy = source", 4), ("sink_2 = source_copy", 7)),
        List(("source_copy = source", 4), ("sink_2 = source_copy", 7))
      )
    }
  }

  "Logical OR short-circuit condition use" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |  int source = 0;
        |  int source_copy = source;
        |  int sink_1 = source_copy;
        |  int guard = source_copy || 0;
        |  int sink_2 = source_copy;
        |  return 0;
        |}""".stripMargin)

    "find flow to sink_2 after logical or condition use" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_2")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source = 0", 3), ("source_copy = source", 4), ("sink_2 = source_copy", 7)),
        List(("source_copy = source", 4), ("sink_2 = source_copy", 7))
      )
    }
  }

  "Nested assignments in ternary branches" should {
    val cpg = code("""
        |int main(int argc, char **argv) {
        |  int source = 1;
        |  int x = 0;
        |  int y = 0;
        |  int cond = 1;
        |  cond ? (x = source) : (y = source);
        |  int sink_x = x;
        |  int sink_y = y;
        |  return 0;
        |}""".stripMargin)

    "flow into x through then-branch assignment" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_x")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source = 1", 3), ("x = source", 7), ("sink_x = x", 8)),
        List(("x = source", 7), ("sink_x = x", 8))
      )
    }

    "flow into y through else-branch assignment" in {
      val source = cpg.identifier("source")
      val sink   = cpg.identifier("sink_y")
      sink.reachableByFlows(source).map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("source = 1", 3), ("y = source", 7), ("sink_y = y", 9)),
        List(("y = source", 7), ("sink_y = y", 9))
      )
    }
  }
}
