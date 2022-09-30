package io.joern.dataflowengineoss

import io.joern.dataflowengineoss.semanticsloader.{FlowSemantic, Semantics}
import io.shiftleft.codepropertygraph.generated.Operators

object DefaultSemantics {

  def apply(): Semantics = {
    val list = List(
      F(Operators.addition, List((1, -1), (2, -1))),
      F(Operators.addressOf, List((1, -1))),
      F(Operators.assignment, List((2, 1))),
      F(Operators.assignmentAnd, List((2, 1), (1, 1))),
      F(Operators.assignmentArithmeticShiftRight, List((2, 1), (1, 1))),
      F(Operators.assignmentDivision, List((2, 1), (1, 1))),
      F(Operators.assignmentExponentiation, List((2, 1), (1, 1))),
      F(Operators.assignmentLogicalShiftRight, List((2, 1), (1, 1))),
      F(Operators.assignmentMinus, List((2, 1), (1, 1))),
      F(Operators.assignmentModulo, List((2, 1), (1, 1))),
      F(Operators.assignmentMultiplication, List((2, 1), (1, 1))),
      F(Operators.assignmentOr, List((2, 1), (1, 1))),
      F(Operators.assignmentPlus, List((2, 1), (1, 1))),
      F(Operators.assignmentShiftLeft, List((2, 1), (1, 1))),
      F(Operators.assignmentXor, List((2, 1), (1, 1))),
      F(Operators.cast, List((1, -1), (2, -1))),
      F(Operators.computedMemberAccess, List((1, -1))),
      F(Operators.conditional, List((2, -1), (3, -1))),
      F(Operators.elvis, List((1, -1), (2, -1))),
      F(Operators.notNullAssert, List((1, -1))),
      F(Operators.fieldAccess, List((1, -1))),
      F(Operators.getElementPtr, List((1, -1))),

      // TODO does this still exist?
      F("<operator>.incBy", List((1, 1), (2, 1), (3, 1), (4, 1))),
      F(Operators.indexAccess, List((1, -1))),
      F(Operators.indirectComputedMemberAccess, List((1, -1))),
      F(Operators.indirectFieldAccess, List((1, -1))),
      F(Operators.indirectIndexAccess, List((1, -1), (2, 1))),
      F(Operators.indirectMemberAccess, List((1, -1))),
      F(Operators.indirection, List((1, -1))),
      F(Operators.memberAccess, List((1, -1))),
      F(Operators.pointerShift, List((1, -1))),
      F(Operators.postDecrement, List((1, 1))),
      F(Operators.postIncrement, List((1, 1))),
      F(Operators.preDecrement, List((1, 1))),
      F(Operators.preIncrement, List((1, 1))),
      F(Operators.sizeOf, List()),
      F("free", List((1, 1))),
      F("scanf", List((2, 2))),
      F("strcmp", List((1, -1), (2, -1))),
      F("java.io.PrintWriter.println:void(java.lang.String)", List((1, -1))),
      F("java.io.PrintStream.println:void(java.lang.String)", List((1, -1))),

      //  some of those operators have duplicate mappings due to a typo
      //  - see https://github.com/ShiftLeftSecurity/codepropertygraph/pull/1630

      F("<operators>.assignmentExponentiation", List((2, 1), (1, 1))),
      F("<operators>.assignmentModulo", List((2, 1), (1, 1))),
      F("<operators>.assignmentShiftLeft", List((2, 1), (1, 1))),
      F("<operators>.assignmentLogicalShiftRight", List((2, 1), (1, 1))),
      F("<operators>.assignmentArithmeticShiftRight", List((2, 1), (1, 1))),
      F("<operators>.assignmentAnd", List((2, 1), (1, 1))),
      F("<operators>.assignmentOr", List((2, 1), (1, 1))),
      F("<operators>.assignmentXor", List((2, 1), (1, 1)))
    )
    Semantics.fromList(list)
  }

  private def F = FlowSemantic

}
