package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.Cpg
// TODO bring back help/doc
//import overflowdb.traversal.help.{Doc, TraversalSource}
import io.shiftleft.semanticcpg.language.*

// TODO bring back help/doc
//@TraversalSource
class NodeTypeStarters(cpg: Cpg) {

  // TODO bring back help/doc
//  @Doc(info = "All module-level variables, e.g., variables declared at the root of a file in Python or JavaScript.")
  def moduleVariables: Iterator[OpNodes.ModuleVariable] =
    cpg.method.isModule.local.moduleVariables

}
