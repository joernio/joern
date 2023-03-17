package io.shiftleft.semanticcpg.utils

import io.shiftleft.codepropertygraph.generated.Operators

object MemberAccess {

  /** For a given name, determine whether it is the name of a "member access" operation, e.g.,
    * "<operator>.memberAccess".
    */
  def isGenericMemberAccessName(name: String): Boolean = {
    (name == Operators.memberAccess) ||
    (name == Operators.indirectComputedMemberAccess) ||
    (name == Operators.indirectMemberAccess) ||
    (name == Operators.computedMemberAccess) ||
    (name == Operators.indirection) ||
    (name == Operators.addressOf) ||
    (name == Operators.fieldAccess) ||
    (name == Operators.indirectFieldAccess) ||
    (name == Operators.indexAccess) ||
    (name == Operators.indirectIndexAccess) ||
    (name == Operators.pointerShift) ||
    (name == Operators.getElementPtr)
  }

  def isFieldAccess(name: String): Boolean = {
    (name == Operators.memberAccess) ||
    (name == Operators.indirectComputedMemberAccess) ||
    (name == Operators.indirectMemberAccess) ||
    (name == Operators.computedMemberAccess) ||
    (name == Operators.indirection) ||
    (name == Operators.fieldAccess) ||
    (name == Operators.indirectFieldAccess) ||
    (name == Operators.indexAccess) ||
    (name == Operators.indirectIndexAccess) ||
    (name == Operators.getElementPtr)
  }

}
