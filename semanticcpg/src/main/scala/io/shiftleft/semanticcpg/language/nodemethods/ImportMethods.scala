package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.Import
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language._

class ImportMethods(val node: Import) extends AnyVal with NodeExtension {

  def lineNumber: Option[Integer] = node.call.lineNumber.headOption

  def columnNumber : Option[Integer] = node.call.columnNumber.headOption

}
