package io.shiftleft.fuzzyc2cpg.querying

import io.shiftleft.dataflowengineoss.language._
import io.shiftleft.fuzzyc2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.semanticcpg.language._

class DataFlowTests extends DataFlowCodeToCpgSuite {
  override val code =
    """| #include <stdlib.h>
       | struct node {
       | int value;
       | struct node *next;
       | };
       |
       | void free_list(struct node *head) {
       | struct node *q;
       | for (struct node *p = head; p != NULL; p = q) {
       |    q = p->next;
       |    free(p);
       |    }
       | }
       | int flow(int p0) {
       |    int a = p0;
       |    int b=a;
       |    int c=0x31;
       |    int z = b + c;
       |    z++;
       |    int x = z;
       |    return x;
       |    }
             """.stripMargin

  "should identify all calls to `free`" in {
    cpg.call.name("free").code.toSet shouldBe Set("free(p)")
  }

  "should find flows to arguments of `free`" in {
    implicit val callResolver = NoResolve
    val source = cpg.identifier
    val sink = cpg.method.name("free").parameter.argument
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
  }

  "should find flows to `free`" in {
    val source = cpg.identifier
    val sink = cpg.call.name("free")
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 5
  }

  "should find flows from identifiers to return values of `flow`" in {
    val source = cpg.identifier
    val sink = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).l.map(flowToResultPairs).distinct.size shouldBe 8
  }

  "find flows from z to method returns of flow" in {
    val source = cpg.identifier.name("z")
    val sink = cpg.method.name("flow").methodReturn
    sink.reachableByFlows(source).l.size shouldBe 3
  }
}
