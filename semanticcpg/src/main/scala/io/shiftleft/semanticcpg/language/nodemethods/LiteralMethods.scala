package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.generated.nodes.{Literal, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*

class LiteralMethods(val literal: Literal) extends AnyVal with NodeExtension with HasLocation {
  def value(typeFullName: String): String = {
    if (literal.typeFullName == typeFullName) {
      return "asd"
    }

    return ""
  }

  override def location: NewLocation = {
    LocationCreator(literal, literal.code, literal.label, literal.lineNumber, literal.method)

  }
}
