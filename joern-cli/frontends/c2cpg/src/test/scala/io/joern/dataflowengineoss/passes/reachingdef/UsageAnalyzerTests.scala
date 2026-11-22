package io.joern.dataflowengineoss.passes.reachingdef

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*

/** One case per condition `isUsing` matches under, each with a method that exercises it. */
class UsageAnalyzerTests extends DataFlowCodeToCpgSuite {

  /** Returns the (use, definition) code pairs the index finds in `method`, after checking at every node that the index
    * finds what filtering all of `in(node)` with `isUsing` finds. Every test runs these checks:
    *   - on the solution's `in` sets;
    *   - on the views of [[CompactReachingDefSets.solveIn]] with one and with two words per chunk, every candidate list
    *     read by its words, by one analyzer, so that a list laid out for one solution is laid out again for the next;
    *   - and `usedIncomingDefs` on views, which also chooses between reading the candidates and filtering all of `in`.
    */
  private def usedPairs(method: Method): Set[(String, String)] = {
    val problem      = ReachingDefProblem.create(method)
    val solution     = new DataFlowSolver().calculateMopSolutionForwards(problem)
    val analyzer     = new UsageAnalyzer(problem, solution.in)
    val numberToNode = analyzer.numberToNode
    def expected(on: UsageAnalyzer, use: CfgNode, incoming: scala.collection.Set[Int]) =
      incoming.filter(definition => on.isUsing(use, numberToNode(definition)))
    val pairs = solution.in.toSeq.flatMap { case (node, incoming) =>
      analyzer.uses(node).toSeq.flatMap { use =>
        val used = analyzer.usedByIndex(use, incoming)
        used shouldBe expected(analyzer, use, incoming)
        used.toSeq.map(definition => use.code -> numberToNode(definition).code)
      }
    }.toSet
    val byWords = new UsageAnalyzer(problem, solution.in, longListLength = 1)
    Seq(0, 1, 0).foreach { chunkShift =>
      CompactReachingDefSets.solveIn(problem, chunkShift).foreach { case (node, incoming) =>
        byWords.uses(node).foreach(use => byWords.usedByIndex(use, incoming) shouldBe expected(byWords, use, incoming))
      }
    }
    val views   = CompactReachingDefSets.solveIn(problem, chunkShift = 0)
    val onViews = new UsageAnalyzer(problem, views, longListLength = 1)
    views.foreach { case (node, incoming) =>
      onViews.uses(node).foreach(use => onViews.usedIncomingDefs(node)(use) shouldBe expected(onViews, use, incoming))
    }
    pairs
  }

  "A value written many times in a row" should {
    val cpg = code("""
        |void f(unsigned *r, int n) {
        |  r[0] = (unsigned)1 << 16;
        |  r[1] = (unsigned)1 << 16;
        |  if (n) r[2] = (unsigned)1 << 16;
        |  r[3] = (unsigned)1 << 16;
        |  for (int i = 0; i < n; i++) r[4] = (unsigned)1 << 16;
        |  r[5] = (unsigned)1 << 16;
        |  g(r);
        |}
        |""".stripMargin)

    "have each use's definitions found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        usedPairs(method) should contain("(unsigned)1" -> "(unsigned)1")
      }
    }
  }

  "A method whose candidate lists span many chunks" should {
    val writes = (0 until 120).map(i => s"  r[$i] = (unsigned)1 << ${i % 5};").mkString("\n")
    val cpg    = code(s"""
        |void f(unsigned *r, int n) {
        |$writes
        |  if (n) r[0] = r[1];
        |  g(r);
        |}
        |""".stripMargin)

    "have each use's definitions found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        ReachingDefProblem.create(method).flowGraph.asInstanceOf[ReachingDefFlowGraph].numberToNode.size should be > 256
        usedPairs(method) should contain("r[1]" -> "r[1]")
      }
    }
  }

  "A use of the same variable" should {
    val cpg = code("""
        |int f(int a, int *p) {
        |  *p = a;
        |  return g(p);
        |}
        |""".stripMargin)

    "be found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        val pairs = usedPairs(method)
        pairs should contain("a" -> "int a")
        pairs should contain("p" -> "*p")
      }
    }
  }

  "A use of a container whose field was defined" should {
    val cpg = code("""
        |struct s { int x; };
        |int f(struct s t) {
        |  t.x = 1;
        |  return g(t);
        |}
        |""".stripMargin)

    "be found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        usedPairs(method) should contain("t" -> "t.x")
      }
    }
  }

  "A use of a part of a defined variable" should {
    val cpg = code("""
        |struct s { int x; };
        |int f(struct s t, struct s u) {
        |  t = u;
        |  return g(t.x);
        |}
        |""".stripMargin)

    "be found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        usedPairs(method) should contain("t.x" -> "t")
      }
    }
  }

  "A use through another spelling of the same access path" should {
    val cpg = code("""
        |struct s { int a; };
        |int f(struct s *p) {
        |  p->a = 1;
        |  return g((*p).a);
        |}
        |""".stripMargin)

    "be found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        usedPairs(method) should contain("(*p).a" -> "p->a")
      }
    }
  }

  "A use of a field through a pointer to the struct" should {
    val cpg = code("""
        |struct s { int a; int b; };
        |int f(int x) {
        |  struct s v;
        |  struct s *p = &v;
        |  p->a = x;
        |  v.b = x;
        |  return g(v.a) + g(p->b);
        |}
        |""".stripMargin)

    "be found by the usage index" in {
      inside(cpg.method.nameExact("f").l) { case List(method) =>
        val pairs = usedPairs(method)
        pairs should contain("v.a" -> "p->a")
        pairs should contain("p->b" -> "v.b")
      }
    }
  }
}
