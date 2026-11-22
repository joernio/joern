package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class EdgeValidatorTests extends DataFlowCodeToCpgSuite {

  private implicit val semantics: Semantics = DefaultSemantics()

  private val cpg = code("""
      |struct row { int a; int b; };
      |static struct row table[] = { {1, 2}, {3, 4}, {5, 6} };
      |void set(int *p) { *p = 1; }
      |int f(int x, int *q) {
      |  int y = x + 1;
      |  set(&y);
      |  free(q);
      |  y = unknown(x, y, q);
      |  return y;
      |}
      |""".stripMargin)

  /** The decisions of one validator kept for all pairs of `method`'s nodes, after checking each against a fresh one. */
  private def decisions(method: Method): Seq[Boolean] = {
    val nodes  = List(method) ++ method.parameter.l ++ method.cfgNode.l
    val shared = new CachedEdgeValidator
    for { child <- nodes; parent <- nodes } yield {
      val decision = shared.isValidEdge(child, parent)
      decision shouldBe EdgeValidator.isValidEdge(child, parent)
      decision
    }
  }

  "A validator kept for all the edges of a method" should {
    "decide as a fresh validator does, for every pair of nodes" in {
      semantics.initialize(cpg)
      val methods = cpg.method.nameExact("f", "<global>").filter(_.filename.endsWith(".c")).l
      methods.map(_.name).sorted shouldBe List("<global>", "f")
      methods.flatMap(decisions).toSet shouldBe Set(true, false)
    }
  }
}
