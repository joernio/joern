package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Operators

package object operatorextension {

  /** All operators that perform both assignments and arithmetic.
    */
  val assignmentAndArithmetic: Set[String] = Set(
    Operators.assignmentDivision,
    Operators.assignmentExponentiation,
    Operators.assignmentPlus,
    Operators.assignmentMinus,
    Operators.assignmentModulo,
    Operators.assignmentMultiplication,
    Operators.preIncrement,
    Operators.preDecrement,
    Operators.postIncrement,
    Operators.postIncrement
  )

  /** All operators that carry out assignments.
    */
  val allAssignmentTypes: Set[String] = Set(
    Operators.assignment,
    Operators.assignmentOr,
    Operators.assignmentAnd,
    Operators.assignmentXor,
    Operators.assignmentArithmeticShiftRight,
    Operators.assignmentLogicalShiftRight,
    Operators.assignmentShiftLeft
  ) ++ assignmentAndArithmetic

  /** All operators representing arithmetic.
    */
  val allArithmeticTypes: Set[String] = Set(
    Operators.addition,
    Operators.subtraction,
    Operators.division,
    Operators.multiplication,
    Operators.exponentiation,
    Operators.modulo
  ) ++ assignmentAndArithmetic

  /** All operators representing array accesses.
    */
  val allArrayAccessTypes: Set[String] = Set(
    Operators.computedMemberAccess,
    Operators.indirectComputedMemberAccess,
    Operators.indexAccess,
    Operators.indirectIndexAccess
  )

  /** All operators representing direct or indirect accesses to fields of data structures
    */
  val allFieldAccessTypes: Set[String] = Set(Operators.fieldAccess, Operators.indirectFieldAccess)

}
