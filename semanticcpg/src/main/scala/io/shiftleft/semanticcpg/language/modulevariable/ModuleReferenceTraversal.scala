package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.nodes.Local
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import overflowdb.traversal.help.Doc

class ModuleReferenceTraversal(traversal: Iterator[OpNodes.ModuleVariableReference]) extends AnyVal {

  @Doc(info = "The module variable that this references to")
  def moduleVariable: Iterator[OpNodes.ModuleVariable] =
    traversal.flatMap(_.moduleVariable)

  @Doc(info = "All assignments where the module reference in this traversal are the target")
  def definitions: Iterator[Assignment] = {
    val varRefs        = traversal.toList
    val moduleVarNames = varRefs.iterator.moduleVariable.name.distinct.toSeq
    varRefs.iterator.inAssignment
      .where(_.target.isIdentifier.nameExact(moduleVarNames*))
      .dedup
  }

  @Doc(info = "All local variables concerning the module variable")
  def referencingLocals: Iterator[Local] = {
    traversal.inAssignment.target.isIdentifier.refsTo.collectAll[Local]
  }

}
