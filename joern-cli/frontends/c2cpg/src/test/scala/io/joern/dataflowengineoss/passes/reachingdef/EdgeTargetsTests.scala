package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class EdgeTargetsTests extends DataFlowCodeToCpgSuite {

  private implicit val semantics: Semantics = DefaultSemantics()

  private val cpg = code("""
      |struct row { int a; int b; };
      |static struct row table[] = { {1, 2}, {3, 4}, {5, 6}, {7, 8} };
      |int f(char *dst, char *src, int n) {
      |  strncpy(dst, src, n);
      |  int y = n + 1;
      |  return unknown(dst, src, y);
      |}
      |""".stripMargin)

  /** Checks the arguments of every call in `method` against their candidates from `edgeTargets`, and also against all
    * the definitions the call generates, to ensure that both accept the same definitions and the order is unchanged.
    */
  private def accepted(method: Method): Map[String, Seq[String]] = {
    val problem   = ReachingDefProblem.create(method)
    val gen       = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen
    val nodes     = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode
    val validator = new CachedEdgeValidator
    method.call.l.flatMap { call =>
      val defs    = gen(call).iterator.map(nodes).toArray
      val targets = validator.edgeTargets(defs)
      call.argument.l.map { use =>
        val all = defs.filter(definition => definition != use && validator.isValidEdge(definition, use)).toSeq
        targets
          .candidates(use)
          .filter(definition => definition != use && validator.isValidEdge(definition, use))
          .toSeq shouldBe all
        s"${call.code} / ${use.code}" -> all.map(_.code)
      }
    }.toMap
  }

  "The candidates of a call's uses" should {
    "hold every definition the validator accepts, in order" in {
      semantics.initialize(cpg)
      val pairs = cpg.method.nameExact("f", "<global>").filter(_.filename.endsWith(".c")).l.map(accepted).reduce(_ ++ _)
      pairs("{ {1, 2}, {3, 4}, {5, 6}, {7, 8} } / {1, 2}") shouldBe Seq("{ {1, 2}, {3, 4}, {5, 6}, {7, 8} }")
      pairs("unknown(dst, src, y) / dst") shouldBe Seq("src", "y", "unknown(dst, src, y)")
    }
  }
}
