package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal._
import overflowdb.traversal.help.{Doc, TraversalSource}

/** Steps that allow traversing from `cpg` to operators.
  */
@TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  @Doc(info = "All assignments, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def assignment: Traversal[OpNodes.Assignment] =
    callsWithNameIn(allAssignmentTypes)
      .map(new OpNodes.Assignment(_))

  @Doc(info = "All arithmetic operations, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def arithmetic: Traversal[OpNodes.Arithmetic] =
    callsWithNameIn(allArithmeticTypes)
      .map(new OpNodes.Arithmetic(_))

  @Doc(info = "All array accesses")
  def arrayAccess: Traversal[OpNodes.ArrayAccess] =
    callsWithNameIn(allArrayAccessTypes)
      .map(new OpNodes.ArrayAccess(_))

  @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Traversal[OpNodes.FieldAccess] =
    callsWithNameIn(allFieldAccessTypes)
      .map(new OpNodes.FieldAccess(_))

  private def callsWithNameIn(set: Set[String]) =
    cpg.call.filter(x => set.contains(x.name))

}
