package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.Cpg
import overflowdb.traversal.help.{Doc, TraversalSource}
import io.shiftleft.semanticcpg.language.*

@TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  @Doc(info = "All module-level variables, e.g., variables declared at the root of a file in Python or JavaScript.")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] =
    cpg.method.isModule.local.moduleVariables

}
