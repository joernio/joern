package io.shiftleft.semanticcpg.language.modulevariable.nodemethods

import io.shiftleft.semanticcpg.language.modulevariable.OpNodes
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help.Doc

class ModuleVariableReferenceMethods(node: OpNodes.ModuleVariableReference) extends AnyVal {

  @Doc(info = "The module variable being referenced")
  def moduleVariable: Iterator[OpNodes.ModuleVariable] =
    node.astChildren.headOption.isCall.fieldAccess.referencedMember.map(new OpNodes.ModuleVariable(_))

}
