package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.v2.nodes.{FieldIdentifier, Member, TypeDecl}
import io.shiftleft.semanticcpg.language.*
// TODO bring back: import overflowdb.traversal.help.Doc

class FieldAccessTraversal(val traversal: Iterator[OpNodes.FieldAccess]) extends AnyVal {

  // TODO bring back: // TODO bring back: @Doc(info = "Attempts to resolve the type declaration for this field access")
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_.typeDecl)

  // TODO there are cases for the C++ frontend where argument(2) is a CALL or IDENTIFIER,
  // and we are not handling them at the moment

  // TODO bring back: // TODO bring back: @Doc(info = "The identifier of the referenced field (right-hand side)")
  def fieldIdentifier: Iterator[FieldIdentifier] =
    traversal.flatMap(_.fieldIdentifier)

  // TODO bring back: // TODO bring back: @Doc(info = "Attempts to resolve the member referenced by this field access")
  def member: Iterator[Member] =
    traversal.flatMap(_.member)

}
