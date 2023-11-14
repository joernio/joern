package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Identifier}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes.Assignment
import overflowdb.traversal.help.Doc

class ModuleVariableTraversal(traversal: Iterator[OpNodes.ModuleVariable]) extends AnyVal {

  @Doc(info = "All module references where the module variables in this traversal are the target")
  def moduleVariableRefs: Iterator[OpNodes.ModuleVariableReference] =
    traversal.ref.parentBlock.map(new OpNodes.ModuleVariableReference(_))

  @Doc(info = "All assignments where the module variables in this traversal are the target")
  def definitions: Iterator[Assignment] = {
    val sourceMembers = traversal.toList
    sourceMembers.iterator.moduleVariableRefs.inAssignment
      .where(_.target.isIdentifier.nameExact(sourceMembers.name.toSeq*))
      .dedup
  }

  @Doc(info = "All identifiers related to the module variables in this traversal")
  def referencingIdentifiers: Iterator[Identifier] = {
    definitions.target.isIdentifier.refsTo._refIn.collectAll[Identifier]
  }

  @Doc(info = "Calls this module variable invokes")
  def invokingCalls: Iterator[Call] = {
    referencingIdentifiers.argumentIndexLte(1).inCall.dedup.iterator
  }

}
