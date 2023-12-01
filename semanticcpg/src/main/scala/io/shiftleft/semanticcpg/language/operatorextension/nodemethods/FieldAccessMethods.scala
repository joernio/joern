package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes

class FieldAccessMethods(val arrayAccess: OpNodes.FieldAccess) extends AnyVal {

  def typeDecl: Iterator[TypeDecl] = resolveTypeDecl(arrayAccess.argument(1))

  private def resolveTypeDecl(expr: Expression): Iterator[TypeDecl] = {
    expr match {
      case x: Identifier => x.typ.referencedTypeDecl
      case x: Literal    => x.typ.referencedTypeDecl
      case x: Call       => x.fieldAccess.member.typ.referencedTypeDecl
      case _             => Iterator.empty
    }
  }

  def fieldIdentifier: Iterator[FieldIdentifier] = arrayAccess.start.argument(2).isFieldIdentifier

  def member: Option[Member] =
    arrayAccess.referencedMember.headOption

}
