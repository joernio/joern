package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class FieldAccessMethods(val fieldAccess: OpNodes.FieldAccess) extends AnyVal {

  def fieldIdentifier: Iterator[FieldIdentifier] = fieldAccess.arguments(2).isFieldIdentifier

  def member: Option[Member] = fieldAccess.referencedMember.headOption

}
