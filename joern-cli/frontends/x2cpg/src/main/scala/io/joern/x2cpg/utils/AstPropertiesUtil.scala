package io.joern.x2cpg.utils

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.PropertyNames

object AstPropertiesUtil {

  implicit class RootProperties(ast: Ast) {

    private def rootProperty(propertyName: String): Option[String] = {
      ast.root.flatMap(_.properties.get(propertyName).map(_.toString))
    }

    def rootType: Option[String] = rootProperty(PropertyNames.TYPE_FULL_NAME)

    def rootCode: Option[String] = rootProperty(PropertyNames.CODE)

    def rootCodeOrEmpty: String = rootCode.getOrElse("")

  }
}
