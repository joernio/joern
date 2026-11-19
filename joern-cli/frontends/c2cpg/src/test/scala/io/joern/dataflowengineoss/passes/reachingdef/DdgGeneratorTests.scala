package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.Semantics
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, GraphSchema}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

class DdgGeneratorTests extends DataFlowCodeToCpgSuite {

  private implicit val semantics: Semantics = DefaultSemantics()

  private val cpg = code("""
      |int counter = 1;
      |static int add(int x) { return x + counter; }
      |int (*handler)(int) = add;
      |int f(int y) {
      |  counter = 2;
      |  return handler(y) + add(3);
      |}
      |""".stripMargin)

  /** The number of edges `generator` writes for `method`. */
  private def edges(generator: DdgGenerator, method: Method): Int = {
    val problem  = ReachingDefProblem.create(method)
    val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val builder  = new DiffGraphBuilder(GraphSchema)
    generator.addReachingDefEdges(builder, method, problem, solution)
    builder.size
  }

  "A graph without module methods" should {
    "get the same edges without looking for globals assigned from literals" in {
      semantics.initialize(cpg)
      cpg.method.isModule shouldBe empty
      cpg.closureBinding should not be empty
      val counts = cpg.method.filter(_.filename.endsWith(".c")).l.map { method =>
        val count = edges(new DdgGenerator(semantics), method)
        edges(new DdgGenerator(semantics, false), method) shouldBe count
        count
      }
      counts.sum should be > 0
    }
  }
}
