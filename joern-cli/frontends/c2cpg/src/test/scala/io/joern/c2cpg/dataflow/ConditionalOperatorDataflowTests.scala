package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class ConditionalOperatorDataflowTests extends DataFlowCodeToCpgSuite {

  "Issue5667 conditional operator condition use" should {
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
      cpg.identifier("sink_1").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
    }

    "find flow to sink_2 after ternary condition use" in {
      cpg.identifier("sink_2").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
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
      cpg.identifier("sink_2").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
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
      cpg.identifier("sink_2").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
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
      cpg.identifier("sink_2").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
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
      cpg.identifier("sink_x").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
    }

    "flow into y through else-branch assignment" in {
      cpg.identifier("sink_y").reachableByFlows(cpg.identifier("source")).nonEmpty shouldBe true
    }
  }
}
