package io.joern.x2cpg.passes

import flatgraph.misc.TestUtils.*
import io.joern.x2cpg.passes.controlflow.cfgcreation.Cfg.*
import io.joern.x2cpg.passes.controlflow.cfgcreation.{Cfg, CfgCreator}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, Cpg, DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.semanticcpg.language.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CfgCreatorTypedEdgeTests extends AnyWordSpec with Matchers {

  private class Fixture {
    val cpg    = Cpg.empty
    val graph  = cpg.graph
    val method = graph.addNode(NewMethod().name("test").fullName("test").code("method"))
    val block  = graph.addNode(NewBlock().code("body").order(1))
    val exit   = graph.addNode(NewMethodReturn().typeFullName("void").code("exit").order(2))
    ast(method, block, exit)

    def ast(parent: AstNode, children: AstNode*): Unit =
      graph.applyDiff { builder => children.foreach(child => builder.addEdge(parent, child, EdgeTypes.AST)) }

    def edge(src: AstNode, dst: AstNode, label: String): Unit =
      graph.applyDiff(_.addEdge(src, dst, label))

    def call(code: String, order: Int = 1): Call =
      graph.addNode(NewCall().name(code).code(code).order(order))

    def literal(code: String, order: Int = 1): Literal =
      graph.addNode(NewLiteral().code(code).order(order))

    def control(kind: String, order: Int = 1): ControlStructure =
      graph.addNode(NewControlStructure().controlStructureType(kind).code(kind).order(order))

    def body(parent: ControlStructure, children: AstNode*): Block = {
      val body = graph.addNode(NewBlock().code("loopBody").order(2))
      ast(parent, body)
      ast(body, children*)
      edge(parent, body, EdgeTypes.TRUE_BODY)
      body
    }

    def condition(parent: ControlStructure, value: Expression): Unit = {
      ast(parent, value)
      edge(parent, value, EdgeTypes.CONDITION)
    }

    def cfg(forExecution: Boolean = false): Cfg = new CfgCreator(method, Cpg.newDiffGraphBuilder, forExecution).build()

    def edges: Set[(String, String, CfgEdgeType)] =
      cfg().edges.map(edge => (edge.src.code, edge.dst.code, edge.edgeType)).toSet
  }

  private def withFixture(test: Fixture => Unit): Unit = {
    val fixture = new Fixture
    try test(fixture)
    finally fixture.cpg.close()
  }

  "CfgCreator.build" should {
    "expose conditional branch types without persisting edges" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.IF)
      val condition = f.literal("condition")
      val yes       = f.call("yes", 2)
      val no        = f.call("no", 3)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.ast(statement, yes, no)
      f.edge(statement, yes, EdgeTypes.TRUE_BODY)
      f.edge(statement, no, EdgeTypes.FALSE_BODY)

      f.edges should contain allOf (
        ("condition", "yes", TrueEdge),
        ("condition", "no", FalseEdge),
        ("yes", "after", AlwaysEdge),
        ("no", "after", AlwaysEdge)
      )
      f.method.out(EdgeTypes.CFG).toList shouldBe empty
    }

    "preserve the condition on an empty if branch" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.IF)
      val condition = f.literal("condition")
      val yes       = f.call("yes", 2)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.ast(statement, yes)
      f.edge(statement, yes, EdgeTypes.TRUE_BODY)

      f.edges should contain allOf (
        ("condition", "yes", TrueEdge),
        ("condition", "after", FalseEdge),
        ("yes", "after", AlwaysEdge)
      )
    }

    "mark both outcomes of a while condition" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.WHILE)
      val condition = f.literal("condition")
      val body      = f.call("body")
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.body(statement, body)

      f.edges should contain allOf (
        ("condition", "body", TrueEdge),
        ("body", "condition", AlwaysEdge),
        ("condition", "after", FalseEdge)
      )
    }

    "retain an empty while body's true self loop" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.WHILE)
      val condition = f.literal("condition")
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.body(statement)

      f.edges should contain allOf (("condition", "condition", TrueEdge), ("condition", "after", FalseEdge))
    }

    "enter a while else block only when the condition is false" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.WHILE)
      val condition = f.literal("condition")
      val body      = f.call("body")
      val leave     = f.control(ControlStructureTypes.BREAK, 2)
      val otherwise = f.call("otherwise", 3)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.body(statement, body, leave)
      f.ast(statement, otherwise)
      f.edge(statement, otherwise, EdgeTypes.FALSE_BODY)

      f.edges should contain allOf (
        ("condition", "body", TrueEdge),
        ("condition", "otherwise", FalseEdge),
        ("BREAK", "after", AlwaysEdge),
        ("otherwise", "after", AlwaysEdge)
      )
      f.edges should not contain (("body", "otherwise", AlwaysEdge))
      f.edges should not contain (("BREAK", "otherwise", AlwaysEdge))
      f.edges should not contain (("condition", "after", FalseEdge))
    }

    "mark logical short circuit and evaluated right operands separately" in {
      List((Operators.logicalAnd, TrueEdge, FalseEdge), (Operators.logicalOr, FalseEdge, TrueEdge)).foreach {
        case (operator, evaluateRight, bypassRight) =>
          withFixture { f =>
            val expression = f.graph.addNode(NewCall().name(operator).code("expression").order(1))
            val left       = f.graph.addNode(NewCall().name("left").code("left").order(1).argumentIndex(1))
            val right      = f.graph.addNode(NewCall().name("right").code("right").order(2).argumentIndex(2))
            f.ast(f.block, expression)
            f.ast(expression, left, right)
            f.edge(expression, left, EdgeTypes.ARGUMENT)
            f.edge(expression, right, EdgeTypes.ARGUMENT)

            f.edges should contain allOf (
              ("left", "right", evaluateRight),
              ("left", "expression", bypassRight),
              ("right", "expression", AlwaysEdge)
            )
          }
      }
    }

    "retain typed alternatives in conditional expressions" in withFixture { f =>
      val expression = f.graph.addNode(NewCall().name(Operators.conditional).code("conditional").order(1))
      val condition  = f.graph.addNode(NewLiteral().code("condition").order(1).argumentIndex(1))
      val yes        = f.graph.addNode(NewCall().name("yes").code("yes").order(2).argumentIndex(2))
      val no         = f.graph.addNode(NewCall().name("no").code("no").order(3).argumentIndex(3))
      f.ast(f.block, expression)
      f.ast(expression, condition, yes, no)
      List(condition, yes, no).foreach(f.edge(expression, _, EdgeTypes.ARGUMENT))

      f.edges should contain allOf (
        ("condition", "yes", TrueEdge),
        ("condition", "no", FalseEdge),
        ("yes", "conditional", AlwaysEdge),
        ("no", "conditional", AlwaysEdge)
      )
    }

    "retain switch case dispatch, fallthrough and default" in withFixture { f =>
      val statement   = f.control(ControlStructureTypes.SWITCH)
      val condition   = f.literal("condition")
      val first       = f.graph.addNode(NewJumpTarget().name("case 1").code("case 1").order(1))
      val firstBody   = f.call("first", 2)
      val default     = f.graph.addNode(NewJumpTarget().name("default").code("default").order(3))
      val defaultBody = f.call("defaultBody", 4)
      val after       = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.body(statement, first, firstBody, default, defaultBody)

      f.edges should contain allOf (
        ("condition", "case 1", CaseEdge),
        ("condition", "default", CaseEdge),
        ("first", "default", AlwaysEdge),
        ("defaultBody", "after", AlwaysEdge)
      )
      f.edges should not contain (("condition", "after", FalseEdge))
    }

    "retain the unmatched outcome of a switch without default" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.SWITCH)
      val condition = f.literal("condition")
      val first     = f.graph.addNode(NewJumpTarget().name("case 1").code("case 1").order(1))
      val firstBody = f.call("first", 2)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)
      f.body(statement, first, firstBody)

      f.edges should contain allOf (("condition", "case 1", CaseEdge), ("condition", "after", FalseEdge))
    }

    "preserve for condition and update edges" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.FOR)
      val init      = f.call("init", 1)
      val condition = f.literal("condition", 2)
      val update    = f.call("update", 3)
      val body      = f.call("body", 4)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.ast(statement, init, condition, update, body)
      f.edge(statement, init, EdgeTypes.FOR_INIT)
      f.edge(statement, condition, EdgeTypes.CONDITION)
      f.edge(statement, update, EdgeTypes.FOR_UPDATE)
      f.edge(statement, body, EdgeTypes.FOR_BODY)

      f.edges should contain allOf (
        ("init", "condition", AlwaysEdge),
        ("condition", "body", TrueEdge),
        ("body", "update", AlwaysEdge),
        ("update", "condition", AlwaysEdge),
        ("condition", "after", FalseEdge)
      )
    }

    "retain an empty for body's true self loop" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.FOR)
      val condition = f.literal("condition", 2)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.condition(statement, condition)

      f.edges should contain allOf (("condition", "condition", TrueEdge), ("condition", "after", FalseEdge))
    }

    "retain infinite empty loops in the execution CFG" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.FOR)
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)

      f.edges should contain(("method", "after", AlwaysEdge))
      val executionEdges = f.cfg(forExecution = true).edges.map(edge => (edge.src.code, edge.dst.code, edge.edgeType))
      executionEdges should contain allOf (("method", "FOR", AlwaysEdge), ("FOR", "FOR", AlwaysEdge))
      executionEdges should not contain (("FOR", "after", AlwaysEdge))
      executionEdges should not contain (("method", "after", AlwaysEdge))
    }

    "retain an empty do body's true self loop" in withFixture { f =>
      val statement = f.control(ControlStructureTypes.DO)
      val condition = f.literal("condition", 2)
      val body      = f.graph.addNode(NewBlock().code("doBody").order(1))
      val after     = f.call("after", 2)
      f.ast(f.block, statement, after)
      f.ast(statement, body)
      f.edge(statement, body, EdgeTypes.DO_BODY)
      f.condition(statement, condition)

      f.edges should contain allOf (("condition", "condition", TrueEdge), ("condition", "after", FalseEdge))
    }

    "execute macro expansions without the persisted provenance shortcut" in withFixture { f =>
      val macroCall =
        f.graph.addNode(NewCall().name("macro").code("macro").dispatchType(DispatchTypes.INLINED).order(1))
      val argument  = f.graph.addNode(NewLiteral().code("argument").order(1).argumentIndex(1))
      val expansion = f.graph.addNode(NewBlock().code("expansion").order(2))
      val operation = f.call("operation")
      val after     = f.call("after", 2)
      f.ast(f.block, macroCall, after)
      f.ast(macroCall, argument, expansion)
      f.ast(expansion, operation)
      f.edge(macroCall, argument, EdgeTypes.ARGUMENT)

      f.edges should contain allOf (
        ("macro", "operation", AlwaysEdge),
        ("macro", "after", AlwaysEdge),
        ("operation", "after", AlwaysEdge)
      )
      val executionEdges = f.cfg(forExecution = true).edges.map(edge => (edge.src.code, edge.dst.code, edge.edgeType))
      executionEdges should contain allOf (("macro", "operation", AlwaysEdge), ("operation", "after", AlwaysEdge))
      executionEdges should not contain (("macro", "after", AlwaysEdge))
    }

    "execute macro arguments through the expansion without evaluating their clones" in withFixture { f =>
      val macroCall =
        f.graph.addNode(NewCall().name("macro").code("macro").dispatchType(DispatchTypes.INLINED).order(1))
      val argument  = f.graph.addNode(NewCall().name("argumentEffect").code("argumentEffect").order(1).argumentIndex(1))
      val expansion = f.graph.addNode(NewBlock().code("expansion").order(2))
      val operation = f.call("expandedEffect")
      f.ast(f.block, macroCall)
      f.ast(macroCall, argument, expansion)
      f.ast(expansion, operation)
      f.edge(macroCall, argument, EdgeTypes.ARGUMENT)

      f.edges should contain allOf (
        ("method", "argumentEffect", AlwaysEdge),
        ("argumentEffect", "macro", AlwaysEdge),
        ("macro", "expandedEffect", AlwaysEdge)
      )
      val execution = f.cfg(forExecution = true)
      execution.entryNode shouldBe Some(f.method)
      execution.edges.map(edge => (edge.src.code, edge.dst.code, edge.edgeType)) should contain allOf (
        ("method", "macro", AlwaysEdge),
        ("macro", "expandedEffect", AlwaysEdge)
      )
      execution.edges.exists(edge => edge.src == argument || edge.dst == argument) shouldBe false
    }
  }
}
