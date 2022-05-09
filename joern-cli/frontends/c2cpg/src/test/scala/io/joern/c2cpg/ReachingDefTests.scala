package io.joern.c2cpg

import io.joern.c2cpg.testfixtures.DataFlowCodeToCpgSuite
import io.joern.dataflowengineoss.passes.reachingdef.ReachingDefFlowGraph
import io.joern.x2cpg.X2Cpg.applyDefaultOverlays
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, Method, Return}
import io.shiftleft.semanticcpg.language._

/*
 The right place for these tests would be `dataflowengineoss`, but we only have access
 to MockCpgs there and they currently don't have control flow edges. Instead, let's at least
 test the integration of the C frontend with reaching definition calculation
 */

class ReachingDefCodeToCpgSuite extends DataFlowCodeToCpgSuite {

  var fooMethod: Method               = _
  var flowGraph: ReachingDefFlowGraph = _

  override def beforeAll(): Unit = {
    super.beforeAll()
    fooMethod = cpg.method("foo").head
    flowGraph = new ReachingDefFlowGraph(fooMethod)
  }

  override def passes(cpg: Cpg): Unit = applyDefaultOverlays(cpg)
}

class ReachingDefFlowGraphTest1 extends ReachingDefCodeToCpgSuite {

  override val code: String =
    """
      |int foo() {
      | int y = x + 1;
      | return y;
      |}
      |""".stripMargin

  "should create ReachingDefFlowGraph with correct entry/exit" in {
    flowGraph.entryNode shouldBe fooMethod
    flowGraph.exitNode shouldBe fooMethod.methodReturn
  }

  "should create ReachingDefFlowGraph with correct successors" in {
    val List(id: Identifier) = flowGraph.succ(fooMethod)
    id.name shouldBe "y"
    flowGraph.succ(fooMethod.methodReturn) shouldBe List()
    val List(ret: Return) = cpg.method("foo").ast.isReturn.l
    flowGraph.succ(ret) shouldBe List(fooMethod.methodReturn)
  }

}

class ReachingDefFlowGraphTest2 extends ReachingDefCodeToCpgSuite {

  override val code: String =
    """
      |int foo(int x, int y) {
      | y = x + 1;
      | return y;
      |}
      |""".stripMargin

  "should create ReachingDefFlowGraph with correct successors" in {
    val param1 = fooMethod.parameter.index(1).head
    val param2 = fooMethod.parameter.index(2).head
    flowGraph.succ(param1) shouldBe List(param2)
    val List(id: Identifier) = flowGraph.succ(param2)
    id.name shouldBe "y"
    val List(ret: Return) = cpg.method("foo").ast.isReturn.l
    val paramOut1         = param1.asOutput.head
    val paramOut2         = param2.asOutput.head

    flowGraph.succ(ret).toSet shouldBe Set(paramOut1)
    flowGraph.succ(paramOut1) shouldBe List(paramOut2)
    flowGraph.succ(paramOut2) shouldBe List(fooMethod.methodReturn)
  }

}

class ReachingDefFlowGraphTest3 extends ReachingDefCodeToCpgSuite {

  override val code: String =
    """
      |void foo(int x, int y) {
      | y = x + 1;
      |}
      |""".stripMargin

  "should create ReachingDefFlowGraph with correct successors" in {
    val param1 = fooMethod.parameter.index(1).head
    val param2 = fooMethod.parameter.index(2).head
    flowGraph.succ(param1) shouldBe List(param2)
    val List(id: Identifier) = flowGraph.succ(param2)
    id.name shouldBe "y"
    val paramOut1 = param1.asOutput.head
    val paramOut2 = param2.asOutput.head
    flowGraph.succ(cpg.call.codeExact("y = x + 1").head) shouldBe List(paramOut1)
    flowGraph.succ(paramOut1) shouldBe List(paramOut2)
    flowGraph.succ(paramOut2) shouldBe List(fooMethod.methodReturn)
  }

}
