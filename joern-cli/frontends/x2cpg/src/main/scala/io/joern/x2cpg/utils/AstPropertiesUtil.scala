package io.joern.x2cpg.utils

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.PropertyNames

object AstPropertiesUtil {

  implicit class RootProperties(val ast: Ast) extends AnyVal {

    private def rootStringProperty(propertyName: String): Option[String] = {
      rootProperty(propertyName).map(_.toString)
    }

    private def rootProperty(propertyName: String): Option[Any] = {
      ast.root.flatMap(_.properties.get(propertyName))
    }

    def rootType: Option[String] = rootStringProperty(PropertyNames.TYPE_FULL_NAME)

    def rootCode: Option[String] = rootStringProperty(PropertyNames.CODE)

    def rootName: Option[String] = rootStringProperty(PropertyNames.NAME)

    def rootCodeOrEmpty: String = rootCode.getOrElse("")

    def rootLine: Option[Int] = rootProperty(PropertyNames.LINE_NUMBER).map(_.asInstanceOf[Int])

    def rootColumn: Option[Int] = rootProperty(PropertyNames.COLUMN_NUMBER).map(_.asInstanceOf[Int])
  }

  implicit class RootPropertiesOnSeq(val asts: Seq[Ast]) extends AnyVal {

    def rootType: Option[String] = asts.headOption.flatMap(_.rootType)

    def rootCode: Option[String] = asts.headOption.flatMap(_.rootCode)

    def rootName: Option[String] = asts.headOption.flatMap(_.rootName)

    def rootCodeOrEmpty: String = asts.rootCode.getOrElse("")
  }
}
