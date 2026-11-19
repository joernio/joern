package io.joern.dataflowengineoss.passes.reachingdef

import flatgraph.DNodeOrNode
import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.shiftleft.codepropertygraph.generated.{DiffGraphBuilder, GraphSchema}
import io.shiftleft.codepropertygraph.generated.nodes.CfgNode
import io.shiftleft.semanticcpg.language.*

import scala.collection.mutable

/** [[CompactReachingDefSets]] must find the reaching definitions of a problem of [[ReachingDefProblem.create]]: at
  * every node, the `in` set its views hold, read in ascending order, must be the set found from the definition of
  * reaching definitions, and the edges written from the views must be those written from that set. Each program is
  * solved with the default chunks and with chunks of one word, so that its sets span several chunks. The solution
  * [[DataFlowSolver]] returns for such a problem must hold the same `in` and `out` sets.
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

  /** The `in` and `out` sets of `problem`, found from the definition of reaching definitions rather than by solving: a
    * definition generated at node `g` reaches node `n` when a path along the flow graph, which leads from each `p` in
    * `pred(n)` to `n`, goes from `g` to `n` and every node strictly between them passes the definition on. A node
    * reached from the method's entry passes on what it generates and what it does not kill; any other node passes on
    * only what it generates, so its `out` is its `gen`. `in` holds the reached nodes, added in reverse post-order;
    * `out` every node of the flow graph.
    */
  private def reachingDefinitions(
    problem: DataFlowProblem[CfgNode, mutable.BitSet]
  ): (Map[CfgNode, mutable.BitSet], Map[CfgNode, mutable.BitSet]) = {
    val flowGraph  = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph]
    val transfer   = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
    val nodes      = flowGraph.numberToNode.toSeq.sortBy(_._1).map(_._2)
    val reached    = flowGraph.allNodesReversePostOrder.toSet
    val successors = nodes.map(node => node -> mutable.ArrayBuffer.empty[CfgNode]).toMap
    flowGraph.allNodesReversePostOrder.foreach { node =>
      flowGraph.pred(node).iterator.filter(successors.contains).foreach(pred => successors(pred) += node)
    }
    def passesOn(node: CfgNode, definition: Definition): Boolean =
      transfer.gen(node).contains(definition) || (reached(node) && !transfer.kill(node).contains(definition))
    val in = nodes.map(node => node -> mutable.BitSet()).toMap
    for {
      generator  <- nodes
      definition <- transfer.gen(generator)
    } {
      val visited = mutable.Set.empty[CfgNode]
      val next    = mutable.Queue.from(successors(generator))
      while (next.nonEmpty) {
        val node = next.dequeue()
        if (visited.add(node)) {
          in(node) += definition
          if (passesOn(node, definition)) next ++= successors(node)
        }
      }
    }
    val out = nodes.map { node =>
      node -> (if (reached(node)) transfer.gen(node) | (in(node) &~ transfer.kill(node))
               else transfer.gen(node).clone())
    }.toMap
    (flowGraph.allNodesReversePostOrder.map(node => node -> in(node)).toMap, out)
  }

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

  /** The edges [[DdgGenerator]] writes for `method` from `in`. */
  private def edgesFrom(
    method: io.shiftleft.codepropertygraph.generated.nodes.Method,
    problem: DataFlowProblem[CfgNode, mutable.BitSet],
    in: Map[CfgNode, scala.collection.Set[Definition]]
  ): List[(DNodeOrNode, DNodeOrNode, String, Any)] =
    edgesWritten(
      new DdgGenerator(context.semantics)
        .addReachingDefEdges(_, method, problem, in.withDefaultValue(Set.empty), problem.transferFunction)
    )

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

    "let the last definition of a variable in a loop reach its next use, through the loop's back edge" in {
      val method     = code(programs("a loop whose definitions fill more than two words")).method.name("f").head
      val problem    = ReachingDefProblem.create(method)
      val numberOf   = problem.flowGraph.asInstanceOf[ReachingDefFlowGraph].nodeToNumber
      val transfer   = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction]
      val views      = CompactReachingDefSets.solveIn(problem)
      val last       = method.call.codeExact("y = z + 50").head
      val definition = numberOf(last.argument(1))
      transfer.gen(last) should contain(definition)
      Seq("n > 0" -> true, "y + 1" -> true, "x + y" -> true, "x = 0" -> false).foreach { (at, reaches) =>
        withClue(s"at $at: ") {
          views(method.call.codeExact(at).head).contains(definition) shouldBe reaches
        }
      }
    }

    for {
      (name, program) <- programs.toSeq.sortBy(_._1)
      (chunks, shift) <- Seq("default chunks" -> CompactReachingDefSets.defaultChunkShift, "one-word chunks" -> 0)
    } {
      s"hold the reaching definitions at every node with $chunks, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem       = ReachingDefProblem.create(method)
          val (expected, _) = reachingDefinitions(problem)
          val definitions = problem.transferFunction.asInstanceOf[ReachingDefTransferFunction].gen.values.flatten.toSet
          val views       = CompactReachingDefSets.solveIn(problem, shift)
          views.keySet shouldBe expected.keySet
          expected.foreach { (node, set) =>
            withClue(s"${method.name}, at ${node.code}: ") {
              views(node) shouldBe set
              views(node).toList shouldBe set.toList
              views(node).size shouldBe set.size
              definitions.foreach(definition => views(node).contains(definition) shouldBe set.contains(definition))
            }
          }
        }
      }

      s"lead to the edges of the reaching definitions, in the same order, with $chunks, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem       = ReachingDefProblem.create(method)
          val (expected, _) = reachingDefinitions(problem)
          withClue(s"${method.name}: ") {
            edgesFrom(method, problem, CompactReachingDefSets.solveIn(problem, shift)) shouldBe
              edgesFrom(method, problem, expected)
          }
        }
      }
    }

    for ((name, program) <- programs.toSeq.sortBy(_._1)) {
      s"be what DataFlowSolver solves a ReachingDefProblem with, in: $name" in {
        code(program).method.isExternal(false).l.foreach { method =>
          val problem                   = ReachingDefProblem.create(method)
          val (expectedIn, expectedOut) = reachingDefinitions(problem)
          val actual                    = new DataFlowSolver().calculateMopSolutionForwards(problem)
          withClue(s"${method.name}: ") {
            actual.in shouldBe a[SolvedIn]
            actual.in.keySet shouldBe expectedIn.keySet
            expectedOut.keys.foreach { node =>
              withClue(s"at ${node.code}: ") {
                actual.in(node) shouldBe expectedIn.getOrElse(node, mutable.BitSet())
                actual.out(node) shouldBe expectedOut(node)
              }
            }
            val generator = new DdgGenerator(context.semantics)
            edgesWritten(generator.addReachingDefEdges(_, method, problem, actual)) shouldBe
              edgesFrom(method, problem, expectedIn)
          }
        }
      }
    }

    "give the empty set for a node without a set in the maps built from the solution" in {
      val cpg      = code(programs("straight line and a branch"))
      val method   = cpg.method.name("f").head
      val other    = cpg.method.name("<global>").head
      val node     = method.methodReturn
      val problem  = ReachingDefProblem.create(method)
      val solution = new DataFlowSolver().calculateMopSolutionForwards(problem)
      Seq("in" -> solution.in, "out" -> solution.out).foreach { (name, map) =>
        val plain = Map.from(map)
        Seq(
          s"$name.updated" -> (map.updated(node, mutable.BitSet(1)), plain.updated(node, mutable.BitSet(1))),
          s"$name.removed" -> (map.removed(node), plain.removed(node)),
          s"$name.concat" -> (map.concat(Map(node -> mutable.BitSet(1))), plain.concat(Map(node -> mutable.BitSet(1)))),
          s"$name.filter" -> (map.filter(_ => true), plain),
          s"$name.filterNot" -> (map.filterNot(_._1 == node), plain.filterNot(_._1 == node)),
          s"$name.empty"     -> (map.empty, Map.empty[CfgNode, mutable.BitSet])
        ).foreach { case (built, (actual, expected)) =>
          withClue(s"$built: ") {
            actual(other) shouldBe mutable.BitSet()
            actual shouldBe expected
          }
        }
      }
    }
  }
}
