package io.joern.dataflowengineoss.language

import io.joern.dataflowengineoss.DefaultSemantics
import io.joern.dataflowengineoss.semanticsloader.FlowPath.FlowMapping
import io.joern.dataflowengineoss.semanticsloader.ParameterNode
import io.shiftleft.codepropertygraph.generated.Operators
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/** Issue #5524: arithmetic binary operators must match addition semantics (operands flow to result only). */
class ArithmeticDdgTests extends AnyWordSpec with Matchers {

  private val binaryOperators =
    List(Operators.addition, Operators.subtraction, Operators.multiplication, Operators.division)

  private def operandToResultMappings(op: String): Set[(Int, Int)] =
    DefaultSemantics.operatorFlows
      .find(_.methodFullName == op)
      .getOrElse(fail(s"missing semantics for $op"))
      .mappings
      .collect { case FlowMapping(ParameterNode(src, _), ParameterNode(dst, _)) => (src, dst) }
      .toSet

  "DefaultSemantics binary operators" should {
    "map both operands to the call result only" in {
      binaryOperators.foreach { op =>
        operandToResultMappings(op) shouldBe Set((1, -1), (2, -1))
      }
    }

    "not define cross-operand flows" in {
      binaryOperators.foreach { op =>
        val mappings = DefaultSemantics.operatorFlows
          .find(_.methodFullName == op)
          .get
          .mappings
          .collect { case FlowMapping(ParameterNode(src, _), ParameterNode(dst, _)) => (src, dst) }
        mappings should not contain (1 -> 2)
        mappings should not contain (2 -> 1)
      }
    }
  }
}
