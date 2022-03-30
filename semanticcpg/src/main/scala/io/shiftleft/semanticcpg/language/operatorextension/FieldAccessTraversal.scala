package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.nodes.{FieldIdentifier, Member, TypeDecl}
import io.shiftleft.semanticcpg.language._
import overflowdb.traversal.Traversal
import overflowdb.traversal.help.Doc

class FieldAccessTraversal(val traversal: Traversal[OpNodes.FieldAccess]) extends AnyVal {

  @Doc(info = "Attempts to resolve the type declaration for this field access")
  def typeDecl: Traversal[TypeDecl] =
    traversal.flatMap(_.typeDecl)

  // TODO there are cases for the C++ frontend where argument(2) is a CALL or IDENTIFIER,
  // and we are not handling them at the moment

  @Doc(info = "The identifier of the referenced field (right-hand side)")
  def fieldIdentifier: Traversal[FieldIdentifier] =
    traversal.flatMap(_.fieldIdentifier)

  @Doc(info = "Attempts to resolve the member referenced by this field access")
  def member: Traversal[Member] =
    traversal.flatMap(_.member)

}
