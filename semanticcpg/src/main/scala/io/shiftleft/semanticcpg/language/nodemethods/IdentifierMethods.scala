package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Identifier, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class IdentifierMethods(val identifier: Identifier) extends AnyVal with NodeExtension with HasLocation with HasLoc {
  @deprecated("Prefer .loc to .location")
  override def location: NewLocation = {
    LocationCreator.defaultCreateLocation(identifier)
  }

  override def loc: Loc = {
    Loc(identifier)
  }

  def isModuleVariable: Boolean = identifier.refOut.collectAll[Declaration].method.isModule.nonEmpty

}
