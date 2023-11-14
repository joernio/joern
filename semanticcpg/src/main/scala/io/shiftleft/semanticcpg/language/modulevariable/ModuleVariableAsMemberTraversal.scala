package io.shiftleft.semanticcpg.language.modulevariable

import io.shiftleft.codepropertygraph.generated.nodes.{Call, Member}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.modulevariable.OpNodes.ModuleVariable
import overflowdb.traversal.help.Doc

class ModuleVariableAsMemberTraversal(traversal: Iterator[Member]) extends AnyVal {

  @Doc(info = "Members representing module variables")
  def moduleVariables: Iterator[ModuleVariable] = {
    val sourceMembers = traversal.toList
    sourceMembers
      .where(
        _.ref.parentBlock.inAssignment.target.isIdentifier
          .where(_.and(_.nameExact(sourceMembers.name.toSeq*), _.method.nameExact("<module>")))
      )
      .map(new ModuleVariable(_))
      .iterator
  }

}
