package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.semanticcpg.language.*

class TernaryPointerCallTests extends DataFlowCodeToCpgSuite {

  "ternary function designators invoked via pointer call" should {
    val cpg = code("""
        |#include <stdio.h>
        |#include <stdbool.h>
        |
        |void open_file_1(char *arg) {
        |  printf(arg);
        |}
        |
        |void open_file_2(char *arg) {
        |  printf(arg);
        |}
        |
        |int main(int argc, char **argv) {
        |  bool cond = true;
        |  char *source = "source";
        |  ((cond ? open_file_1 : open_file_2)(source));
        |  return 0;
        |}
      """.stripMargin)

    "not treat bare designators as direct calls" in {
      cpg.call("open_file_1").size shouldBe 0
      cpg.call("open_file_2").size shouldBe 0
    }

    "flow source into printf via either branch callee" in {
      val source = cpg.identifier.name("source")
      val sink   = cpg.call("printf").argument
      val flows  = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("source = \"source\"", 15),
          ("(cond ? open_file_1 : open_file_2)(source)", 16),
          ("open_file_1(char *arg)", 5),
          ("printf(arg)", 6)
        ),
        List(
          ("source = \"source\"", 15),
          ("(cond ? open_file_1 : open_file_2)(source)", 16),
          ("open_file_2(char *arg)", 9),
          ("printf(arg)", 10)
        ),
        List(("(cond ? open_file_1 : open_file_2)(source)", 16), ("open_file_1(char *arg)", 5), ("printf(arg)", 6)),
        List(("(cond ? open_file_1 : open_file_2)(source)", 16), ("open_file_2(char *arg)", 9), ("printf(arg)", 10))
      )
    }

    "not flow unrelated identifiers into printf" in {
      val unrelated = cpg.identifier.name("cond")
      val sink      = cpg.call("printf").argument
      sink.reachableByFlows(unrelated).size shouldBe 0
    }
  }
}
