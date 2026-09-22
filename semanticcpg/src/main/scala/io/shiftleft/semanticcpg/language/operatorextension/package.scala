package io.shiftleft.semanticcpg.language

import io.shiftleft.codepropertygraph.generated.Operators
import scala.collection.immutable.ArraySeq

package object operatorextension {

  /** All operators that perform both assignments and arithmetic.
    */
  val assignmentAndArithmetic: Seq[String] = ArraySeq(
    Operators.assignmentDivision,
    Operators.assignmentExponentiation,
    Operators.assignmentPlus,
    Operators.assignmentMinus,
    Operators.assignmentModulo,
    Operators.assignmentMultiplication,
    Operators.preIncrement,
    Operators.preDecrement,
    Operators.postIncrement,
    Operators.postDecrement
  )

  /** All operators that carry out assignments.
    */
  val allAssignmentTypes: Seq[String] = ArraySeq(
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
  val allArithmeticTypes: Seq[String] = ArraySeq(
    Operators.addition,
    Operators.subtraction,
    Operators.division,
    Operators.multiplication,
    Operators.exponentiation,
    Operators.modulo
  ) ++ assignmentAndArithmetic

  /** All operators representing array accesses.
    */
  val allArrayAccessTypes: Seq[String] = ArraySeq(
    Operators.computedMemberAccess,
    Operators.indirectComputedMemberAccess,
    Operators.indexAccess,
    Operators.indirectIndexAccess
  )

  /** All operators representing direct or indirect accesses to fields of data structures
    */
  val allFieldAccessTypes: Seq[String] = ArraySeq(Operators.fieldAccess, Operators.indirectFieldAccess)

}
