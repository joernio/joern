package io.joern.x2cpg.utils

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.PropertyNames

object AstPropertiesUtil {

  implicit class RootProperties(val ast: Ast) extends AnyVal {

    private def rootProperty(propertyName: String): Option[String] = {
      ast.root.flatMap(_.properties.get(propertyName).map(_.toString))
    }

    def rootType: Option[String] = rootProperty(PropertyNames.TYPE_FULL_NAME)

    def rootCode: Option[String] = rootProperty(PropertyNames.CODE)

    def rootName: Option[String] = rootProperty(PropertyNames.NAME)

    def rootCodeOrEmpty: String = rootCode.getOrElse("")

  }

  implicit class RootPropertiesOnSeq(val asts: Seq[Ast]) extends AnyVal {

    def rootType: Option[String] = asts.headOption.flatMap(_.rootType)

    def rootCode: Option[String] = asts.headOption.flatMap(_.rootCode)

    def rootName: Option[String] = asts.headOption.flatMap(_.rootName)

    def rootCodeOrEmpty: String = asts.rootCode.getOrElse("")
  }
}
