package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help.{Doc, TraversalSource}

/** Steps that allow traversing from `cpg` to operators. */
@TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  @Doc(info = "All assignments, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def assignment: Iterator[OpNodes.Assignment] =
    callsWithNameIn(allAssignmentTypes)
      .cast[OpNodes.Assignment]

  @Doc(info = "All arithmetic operations, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def arithmetic: Iterator[OpNodes.Arithmetic] =
    callsWithNameIn(allArithmeticTypes)
      .cast[OpNodes.Arithmetic]

  @Doc(info = "All array accesses")
  def arrayAccess: Iterator[OpNodes.ArrayAccess] =
    callsWithNameIn(allArrayAccessTypes)
      .cast[OpNodes.ArrayAccess]

  @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    callsWithNameIn(allFieldAccessTypes)
      .cast[OpNodes.FieldAccess]

  private def callsWithNameIn(set: Set[String]) =
    cpg.call.filter(x => set.contains(x.name))

}
