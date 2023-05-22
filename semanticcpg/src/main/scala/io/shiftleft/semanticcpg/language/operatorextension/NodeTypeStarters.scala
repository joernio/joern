package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.help.{Doc, TraversalSource}

/** Steps that allow traversing from `cpg` to operators.
  */
@TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  @Doc(info = "All assignments, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def assignment: Traversal[OpNodes.Assignment] =
    callsWithNameIn(allAssignmentTypes)
      .cast[OpNodes.Assignment]

  @Doc(info = "All arithmetic operations, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def arithmetic: Traversal[OpNodes.Arithmetic] =
    callsWithNameIn(allArithmeticTypes)
      .cast[OpNodes.Arithmetic]

  @Doc(info = "All array accesses")
  def arrayAccess: Traversal[OpNodes.ArrayAccess] =
    callsWithNameIn(allArrayAccessTypes)
      .cast[OpNodes.ArrayAccess]

  @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Traversal[OpNodes.FieldAccess] =
    callsWithNameIn(allFieldAccessTypes)
      .cast[OpNodes.FieldAccess]

  private def callsWithNameIn(set: Set[String]) =
    cpg.call.filter(x => set.contains(x.name))

}
