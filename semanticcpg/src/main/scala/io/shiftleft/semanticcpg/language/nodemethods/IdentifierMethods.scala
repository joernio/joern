package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Identifier, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.{HasLocation, LocationCreator, _}

class IdentifierMethods(val identifier: Identifier) extends AnyVal with NodeExtension with HasLocation {
  override def location: NewLocation = {
    LocationCreator(identifier, identifier.name, identifier.label, identifier.lineNumber, identifier.method)
  }
}
