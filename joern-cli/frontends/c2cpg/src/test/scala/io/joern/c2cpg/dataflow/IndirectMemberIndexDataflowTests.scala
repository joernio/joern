package io.joern.c2cpg.dataflow

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.language.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.semanticcpg.language.*

class IndirectMemberIndexDataflowTests extends DataFlowCodeToCpgSuite {

  "direct indirect field access from parameter" should {
    val cpg = code("""
        |struct xxx_t {
        |  int *attrs;
        |};
        |
        |void sink(int x);
        |
        |void direct_member(void *a1, struct xxx_t *info) {
        |  sink(info->attrs);
        |}""".stripMargin)

    "flow from info to attrs sink" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.method.name("direct_member").parameter.name("info")
      val sink                                  = cpg.call.codeExact("sink(info->attrs)").argument(1)
      val flows                                 = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("direct_member(void *a1, struct xxx_t *info)", 8), ("sink(info->attrs)", 9))
      )
    }
  }

  "nested indirect field and index access from parameter" should {
    val cpg = code("""
        |struct xxx_t {
        |  int *attrs;
        |};
        |
        |void sink(int x);
        |
        |void nested_member_index(void *a1, struct xxx_t *info) {
        |  sink(info->attrs[1]);
        |  sink(info[1]);
        |}""".stripMargin)

    "flow from info to nested attrs index sink" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.method.name("nested_member_index").parameter.name("info")
      val sink  = cpg.method.name("nested_member_index").call.code("sink\\(info->attrs\\[1\\]\\)").argument(1)
      val flows = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(("nested_member_index(void *a1, struct xxx_t *info)", 8), ("sink(info->attrs[1])", 9))
      )
    }

    "not flow from a1 to nested attrs index sink" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.method.name("nested_member_index").parameter.name("a1")
      val sink = cpg.method.name("nested_member_index").call.code("sink\\(info->attrs\\[1\\]\\)").argument(1)
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

  "field access as the direct base of an index access" should {
    val cpg = code("""
        |struct S { char **arr; };
        |void sink(char *p);
        |
        |void nested(struct S *s, int i) { sink(s->arr[i]); }
        |void via_local(struct S *s, int i) { char **a = s->arr; sink(a[i]); }
        |""".stripMargin)

    "flow from the field access to the index access sink when nested" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source = cpg.method.nameExact("nested").ast.isCall.nameExact(Operators.indirectFieldAccess)
      val sink   = cpg.method.nameExact("nested").ast.isCall.nameExact("sink").argument
      val flows  = sink.reachableByFlows(source).l
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("sink(s->arr[i])", 5)))
    }

    "flow from the field access to the index access sink via a local" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source = cpg.method.nameExact("via_local").ast.isCall.nameExact(Operators.indirectFieldAccess)
      val sink   = cpg.method.nameExact("via_local").ast.isCall.nameExact("sink").argument
      val flows  = sink.reachableByFlows(source).l
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(List(("a = s->arr", 6), ("sink(a[i])", 6)))
    }
  }

  "dereference of assigned pointer from buffer parameter" should {
    val cpg = code("""
        |void sink(int x);
        |
        |void deref_assigned_pointer(int a1, int *array, unsigned char *data) {
        |  int *int_ptr = (int *)(data + 8);
        |  int idx = *int_ptr;
        |  sink(idx);
        |}""".stripMargin)

    "flow from data through int_ptr deref to sink" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.method.name("deref_assigned_pointer").parameter.name("data")
      val sink                                  = cpg.method.name("deref_assigned_pointer").call("sink").argument(1)
      val flows                                 = sink.reachableByFlows(source)
      flows.map(flowToResultPairs).toSetMutable shouldBe Set(
        List(
          ("deref_assigned_pointer(int a1, int *array, unsigned char *data)", 4),
          ("idx = *int_ptr", 6),
          ("sink(idx)", 7)
        )
      )
    }

    "not flow from a1 to sink argument" in {
      implicit val callResolver: NoResolve.type = NoResolve
      val source                                = cpg.method.name("deref_assigned_pointer").parameter.name("a1")
      val sink                                  = cpg.method.name("deref_assigned_pointer").call("sink").argument(1)
      sink.reachableByFlows(source).size shouldBe 0
    }
  }

}
