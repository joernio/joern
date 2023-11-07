package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.v2.CpgNodeStarters
import io.shiftleft.codepropertygraph.generated.v2.Language.*
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help.{Doc, TraversalSource}

/** Steps that allow traversing from `cpg` to operators.
  */
// TODO bring back:  @TraversalSource
class NodeTypeStarters(cpg: Cpg) extends CpgNodeStarters(cpg) {

  // TODO bring back: @Doc(info = "All assignments, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def assignment: Iterator[OpNodes.Assignment] =
    callsWithNameIn(allAssignmentTypes)
      .map(new OpNodes.Assignment(_))

  // TODO bring back: @Doc(info = "All arithmetic operations, including shorthand assignments that perform arithmetic (e.g., '+=')")
  def arithmetic: Iterator[OpNodes.Arithmetic] =
    callsWithNameIn(allArithmeticTypes)
      .map(new OpNodes.Arithmetic(_))

  // TODO bring back: @Doc(info = "All array accesses")
  def arrayAccess: Iterator[OpNodes.ArrayAccess] =
    callsWithNameIn(allArrayAccessTypes)
      .map(new OpNodes.ArrayAccess(_))

  // TODO bring back: @Doc(info = "Field accesses, both direct and indirect")
  def fieldAccess: Iterator[OpNodes.FieldAccess] =
    callsWithNameIn(allFieldAccessTypes)
      .map(new OpNodes.FieldAccess(_))

  private def callsWithNameIn(set: Set[String]) =
    call.filter(x => set.contains(x.name))

}
