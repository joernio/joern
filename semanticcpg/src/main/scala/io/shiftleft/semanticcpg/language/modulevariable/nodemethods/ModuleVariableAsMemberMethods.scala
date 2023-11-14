package io.shiftleft.semanticcpg.language.modulevariable.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.Member
import io.shiftleft.semanticcpg.language.*
import overflowdb.traversal.help.Doc

class ModuleVariableAsMemberMethods(node: Member) extends AnyVal {

  @Doc(info = "If this member refers to a module variable")
  def isModuleVariable: Boolean = {
    Iterator(node)
      .where(
        _.ref.parentBlock.inAssignment.target.isIdentifier
          .where(_.and(_.nameExact(node.name), _.method.nameExact("<module>")))
      )
      .nonEmpty
  }

}
