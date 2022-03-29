package io.shiftleft.semanticcpg.language.operatorextension.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{
  Call,
  Expression,
  FieldIdentifier,
  Identifier,
  Literal,
  Member,
  TypeDecl
}
import io.shiftleft.semanticcpg.language._
import io.shiftleft.semanticcpg.language.operatorextension.OpNodes
import overflowdb.traversal.{NodeOps, Traversal, jIteratortoTraversal}

class FieldAccessMethods(val arrayAccess: OpNodes.FieldAccess) extends AnyVal {

  def typeDecl: Traversal[TypeDecl] = resolveTypeDecl(arrayAccess.argument(1))

  private def resolveTypeDecl(expr: Expression): Traversal[TypeDecl] = {
    expr match {
      case x: Identifier => x.typ.referencedTypeDecl
      case x: Literal    => x.typ.referencedTypeDecl
      case x: Call       => x.fieldAccess.member.typ.referencedTypeDecl
      case _             => Traversal()
    }
  }

  def fieldIdentifier: Traversal[FieldIdentifier] = arrayAccess.start.argument(2).isFieldIdentifier

  def member: Option[Member] = arrayAccess._refOut.to(Traversal).collectAll[Member].headOption

}
