package io.shiftleft.semanticcpg.language.operatorextension

import io.shiftleft.codepropertygraph.generated.help.{Doc, Traversal}
import io.shiftleft.codepropertygraph.generated.nodes.{Call, FieldIdentifier, Member, TypeDecl}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.codepropertygraph.generated.help.Doc

@Traversal(elementType = classOf[Call])
class FieldAccessTraversal(val traversal: Iterator[OpNodes.FieldAccess]) extends AnyVal {

  @Doc(info = "Attempts to resolve the type declaration for this field access")
  def typeDecl: Iterator[TypeDecl] =
    traversal.flatMap(_.typeDecl)

  // TODO there are cases for the C++ frontend where argument(2) is a CALL or IDENTIFIER,
  // and we are not handling them at the moment

  @Doc(info = "The identifier of the referenced field (right-hand side)")
  def fieldIdentifier: Iterator[FieldIdentifier] =
    traversal.flatMap(_.fieldIdentifier)

  @Doc(info = "Attempts to resolve the member referenced by this field access")
  def member: Iterator[Member] =
    traversal.flatMap(_.member)

}
