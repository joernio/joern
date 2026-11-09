package io.joern.dataflowengineoss.passes.reachingdef

import flatgraph.DNodeOrNode
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, GraphSchema}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** Solving with [[CompactReachingDefSets]] must reach the fixpoint [[DataFlowSolver]]'s work list reaches: the same
  * `in` at every node, read through its views, in the same order, and the same edges written from it, in the same
  * order. Each program is solved with the default chunks and with chunks of one word, so that its sets span several
  * chunks. The solution [[DataFlowSolver]] returns for a problem of [[ReachingDefProblem.create]] must hold the work
  * list's `in` and `out` at every node and lead to the same edges.
  */
class CompactReachingDefSetsTests extends DataFlowCodeToCpgSuite {

  private val programs = Map(
    "straight line and a branch" -> """
        |int f(int c) {
        |  int x = 1;
        |  if (c) x = 2;
        |  g(x);
        |  x = x + c;
        |  return x;
        |}
        |""".stripMargin,
    "nested loops with continue and break" -> """
        |int f(int n, int *a) {
        |  int s = 0;
        |  for (int i = 0; i < n; i++) {
        |    if (a[i] < 0) continue;
        |    for (int j = i; j < n; j++) {
        |      if (a[j] == 0) break;
        |      s = s + a[j];
        |    }
        |    a[i] = s;
        |  }
        |  return s;
        |}
        |""".stripMargin,
    "goto, switch fall-through and unreachable code" -> """
        |int f(int c, int d) {
        |  int x = 0;
        |again:
        |  switch (c) {
        |    case 0: x = 1;
        |    case 1: x = x + d; break;
        |    default: x = 3;
        |  }
        |  if (x < 10) { c = c - 1; goto again; }
        |  return x;
        |  x = 4;
        |  g(x);
        |}
        |""".stripMargin,
    "field accesses, pointers and a loop at the end" -> """
        |struct s { int a; int b; };
        |int f(struct s *p, int c) {
        |  p->a = c;
        |  p->b = p->a + 1;
        |  if (c) p->a = p->b;
        |  int *q = &p->a;
        |  *q = 5;
        |  x = c ? p->a : p->b;
        |  do { p->b = p->b - 1; } while (p->b > 0);
        |}
        |""".stripMargin,
    "a switch whose cases all define the variable a call uses" -> """
        |int f(int c) {
        |  int x = 0;
        |  switch (c) {
        |    case 0: x = 1; break;
        |    case 1: x = 2; break;
        |    case 2: x = 3; break;
        |    case 3: x = 4; break;
        |    case 4: x = 5; break;
        |    case 5: x = 6; break;
        |    case 6: x = 7; break;
        |  }
        |  g(x);
        |  return 0;
        |  h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x);
        |  h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x); h(x);
        |}
        |""".stripMargin,
    "a loop on one branch of an if, its definitions in several words" -> s"""
        |int f(int c, int n) {
        |  int x = 0, y = 0, z = 0;
        |  if (c) {
        |    while (n > 0) {
        |${(1 to 30).map(i => s"      y = z + $i; z = y + $i;").mkString("\n")}
        |      x = x + 1;
        |      n = n - 1;
        |    }
        |  } else {
        |    g(x);
        |  }
        |  return x + y + z;
        |}
        |""".stripMargin,
    "a loop whose definitions fill more than two words" -> s"""
        |int f(int n) {
        |  int x = 0, y = 0, z = 0;
        |  while (n > 0) {
        |${(1 to 50).map(i => s"    x = y + $i; y = z + $i; z = x + $i;").mkString("\n")}
        |    if (n == 7) continue;
        |    n = n - 1;
        |  }
        |  return x + y + z;
        |}
        |""".stripMargin
  )

  /** `problem` as a problem of no particular kind, which [[DataFlowSolver]] solves with its work list. */
  private def general(problem: DataFlowProblem[CfgNode, mutable.BitSet]): DataFlowProblem[CfgNode, mutable.BitSet] =
    new DataFlowProblem(
      problem.flowGraph,
      problem.transferFunction,
      problem.meet,
      problem.inOutInit,
      problem.forward,
      problem.empty
    )

  /** The edges `add` writes, in the order it writes them. */
  private def edgesWritten(add: DiffGraphBuilder => Unit): List[(DNodeOrNode, DNodeOrNode, String, Any)] = {
    val edges   = mutable.ArrayBuffer.empty[(DNodeOrNode, DNodeOrNode, String, Any)]
    val builder = new DiffGraphBuilder(GraphSchema) {
      override def addEdge(src: DNodeOrNode, dst: DNodeOrNode, edgeLabel: String, property: Any): this.type = {
        edges += ((src, dst, edgeLabel, property))
        this
      }
    }
    add(builder)
    edges.toList
  }

  "the compact dense sets of DataFlowSolver" should {
    "be tested on a method with more than two words of definitions" in {
      val method      = code(programs("a loop whose definitions fill more than two words")).method.name("f").head
      val transfer    = ReachingDefProblem.create(method).transferFunction.asInstanceOf[ReachingDefTransferFunction]
      val definitions = transfer.gen.valuesIterator.flatten.toSet
      definitions.size should be > 128
    }

    "reject a chunk shift outside 0 to 20" in {
      val problem = ReachingDefProblem.create(code(programs("straight line and a branch")).method.name("f").head)
      an[IllegalArgumentException] should be thrownBy CompactReachingDefSets.solveIn(problem, -1)
      an[IllegalArgumentException] should be thrownBy CompactReachingDefSets.solveIn(problem, 21)
      CompactReachingDefSets.solveIn(problem, 20) should not be empty
    }

    for {
      (name, program) <- programs.toSeq.sortBy(_._1)
      (chunks, shift) <- Seq("default chunks" -> CompactReachingDefSets.defaultChunkShift, "one-word chunks" -> 0)
    } {
      s"reach the fixpoint of DataFlowSolver with $chunks, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem  = ReachingDefProblem.create(method)
          val expected = new DataFlowSolver().calculateMopSolutionForwards(general(problem))
          val views    = CompactReachingDefSets.solveIn(problem, shift)
          problem.flowGraph.allNodesReversePostOrder.foreach { node =>
            withClue(s"${method.name}, at ${node.code}: ") {
              views(node) shouldBe expected.in(node)
              views(node).toList shouldBe expected.in(node).toList
              views(node).size shouldBe expected.in(node).size
              expected.in(node).foreach(definition => views(node).contains(definition) shouldBe true)
            }
          }
        }
      }

      s"lead to the edges of DataFlowSolver's solution, in the same order, with $chunks, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem   = ReachingDefProblem.create(method)
          val generator = new DdgGenerator(context.semantics)
          val expected  = edgesWritten(
            generator
              .addReachingDefEdges(
                _,
                method,
                problem,
                new DataFlowSolver().calculateMopSolutionForwards(general(problem))
              )
          )
          val actual = edgesWritten(
            generator
              .addReachingDefEdges(
                _,
                method,
                problem,
                CompactReachingDefSets.solveIn(problem, shift),
                problem.transferFunction
              )
          )
          withClue(s"${method.name}: ") {
            actual shouldBe expected
          }
        }
      }
    }

    for ((name, program) <- programs.toSeq.sortBy(_._1)) {
      s"be what DataFlowSolver solves a ReachingDefProblem with, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem  = ReachingDefProblem.create(method)
          val expected = new DataFlowSolver().calculateMopSolutionForwards(general(problem))
          val actual   = new DataFlowSolver().calculateMopSolutionForwards(problem)
          withClue(s"${method.name}: ") {
            actual.in shouldBe a[SolvedIn]
            actual.in.toList shouldBe expected.in.toList
            actual.out.toList shouldBe expected.out.toList
            actual.in shouldBe expected.in
            actual.out shouldBe expected.out
            actual.in(method) shouldBe expected.in(method)
            actual.out(method) shouldBe expected.out(method)
            val generator = new DdgGenerator(context.semantics)
            edgesWritten(generator.addReachingDefEdges(_, method, problem, actual)) shouldBe
              edgesWritten(generator.addReachingDefEdges(_, method, problem, expected))
          }
        }
      }
    }

    "give the empty set for a node without a set in the maps built from the solution, as the work list does" in {
      val cpg     = code(programs("straight line and a branch"))
      val method  = cpg.method.name("f").head
      val other   = cpg.method.name("<global>").head
      val node    = method.methodReturn
      val problem = ReachingDefProblem.create(method)
      def built(solution: Solution[CfgNode, mutable.BitSet]): Seq[(String, Map[CfgNode, mutable.BitSet])] =
        Seq("in" -> solution.in, "out" -> solution.out).flatMap { (name, map) =>
          Seq(
            s"$name.updated"   -> map.updated(node, mutable.BitSet(1)),
            s"$name.removed"   -> map.removed(node),
            s"$name.concat"    -> map.concat(Map(node -> mutable.BitSet(1))),
            s"$name.filter"    -> map.filter(_ => true),
            s"$name.filterNot" -> map.filterNot(_._1 == node),
            s"$name.empty"     -> map.empty
          )
        }
      val expected = built(new DataFlowSolver().calculateMopSolutionForwards(general(problem)))
      val actual   = built(new DataFlowSolver().calculateMopSolutionForwards(problem))
      actual.map(_._1) shouldBe expected.map(_._1)
      actual.zip(expected).foreach { case ((name, map), (_, expectedMap)) =>
        withClue(s"$name: ") {
          expectedMap(other) shouldBe mutable.BitSet()
          map(other) shouldBe mutable.BitSet()
          map.toList shouldBe expectedMap.toList
        }
      }
    }
  }
}
