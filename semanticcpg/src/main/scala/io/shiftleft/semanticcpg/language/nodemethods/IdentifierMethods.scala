package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Declaration, Identifier}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class IdentifierMethods(val identifier: Identifier) extends AnyVal with NodeExtension with HasLocation {
  override def location: LocationInfo = {
    Location(identifier)
  }

  def isModuleVariable: Boolean = identifier.refOut.collectAll[Declaration].method.isModule.nonEmpty

}
