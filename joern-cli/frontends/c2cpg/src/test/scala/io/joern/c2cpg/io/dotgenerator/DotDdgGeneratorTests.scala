package io.joern.c2cpg.io.dotgenerator

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.dotgenerator.DdgGenerator
import io.joern.dataflowengineoss.language._
import io.shiftleft.codepropertygraph.generated.nodes.Call
import io.shiftleft.semanticcpg.language._

class DotDdgGeneratorTests extends DataFlowCodeToCpgSuite {

  "DotDdgGeneratorTest1" should {
    val cpg = code("""
      |int foo(int param1, char *param2) {
      |   int i = 0;
      |   while(i < 10) {
      |     char *boo = moo("%d\n", i + bar(i));
      |     printf(boo);
      |     i++;
      |   }
      |   return 0;
      |}
      |""".stripMargin)

    "create correct dot graph" in {
      inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
        val lines = elem.split("\n")
        lines.head should startWith("digraph \"foo\"")
        lines.count(x => x.contains("->")) shouldBe 31
        lines.last should startWith("}")
      }
    }
  }

  "DotDdgGeneratorTest2" should {
    val cpg = code("""
      |int foo() {
      |int x = 42;
      |woo(x);
      |baz(x);
      |}
      |""".stripMargin)

    "A DdgDotGenerator" should {
      "create correct dot graph" in {
        inside(cpg.method.name("foo").dotDdg.l) { case List(elem) =>
          val lines = elem.split("\n")
          lines.count(x => x.contains("->") && x.contains("\"x\"")) shouldBe 3
        }
      }
    }
  }

  "include DDG relationship between two calls" in {
    val cpg = code("""
      |static int mwifiex_pcie_delete_cmdrsp_buf(struct mwifiex_adapter *adapter)
      |{
      |    struct pcie_service_card *card;
      |    card = adapter->card;
      |
      |    if (card && card->cmdrsp_buf) {
      |        // the following two lines share the access to 'card->cmdrsp_buf' and should therefor appear in the DDG
      |        dev_kfree_skb_any(card->cmdrsp_buf);
      |        card->cmdrsp_buf = NULL;
      |    }
      |
      |    return 0;
      |}
      |""".stripMargin)


    val ddgGenerator = new DdgGenerator()
    val method = cpg.method.name("mwifiex_pcie_delete_cmdrsp_buf").head
    val ddg = ddgGenerator.generate(method)
    val call1 = cpg.call.code("dev_kfree_skb_any\\(card->cmdrsp_buf\\)").head
    val call2 = cpg.call.code("card->cmdrsp_buf = NULL").head

    ddg.vertices should contain(method)
    ddg.vertices should contain(call1)
    ddg.vertices should contain(call2)

    // TODO remove debug code
    // according to our discussion on discord, the following suggests that we _should_ see that link
    call1.ddgIn().cast[Call].foreach(call => println(call.code)) // 'card->cmdrsp_buf'
    // however, I'm not so sure, since there's no ddg edge to the `card->cmdrsp_buf = NULL` call...
    // TODO debug code end
    
    val call1ToCall2Edges = ddg.edges.filter { edge => edge.src == call1 && edge.dst == call2 }
    val call2ToCall1Edges = ddg.edges.filter { edge => edge.src == call2 && edge.dst == call1 }

    // WIP: one of the following should be true, at least ccording to our discussion on discord...
    call1ToCall2Edges.nonEmpty shouldBe true
    call2ToCall1Edges.nonEmpty shouldBe true
  }

}
