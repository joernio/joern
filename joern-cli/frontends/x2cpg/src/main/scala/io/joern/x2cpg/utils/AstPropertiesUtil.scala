package io.joern.x2cpg.utils

import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.PropertyNames

object AstPropertiesUtil {

  implicit class RootProperties(val ast: Ast) extends AnyVal {

    private def rootStringProperty(propertyName: String): Option[String] = {
      ast.root.flatMap(_.properties.get(propertyName).map(_.toString))
    }

    private def rootIntProperty(propertyName: String): Option[Int] = {
      ast.root.flatMap(_.properties.get(propertyName).collectFirst { case i: Int => i })
    }

    def rootType: Option[String] = rootStringProperty(PropertyNames.TYPE_FULL_NAME)

    def rootCode: Option[String] = rootStringProperty(PropertyNames.CODE)

    def rootName: Option[String] = rootStringProperty(PropertyNames.NAME)

    def rootLine: Option[Int] = rootIntProperty(PropertyNames.LINE_NUMBER)

    def rootColumn: Option[Int] = rootIntProperty(PropertyNames.COLUMN_NUMBER)

    def rootCodeOrEmpty: String = rootCode.getOrElse("")
  }

  implicit class RootPropertiesOnSeq(val asts: Seq[Ast]) extends AnyVal {

    def rootType: Option[String] = asts.headOption.flatMap(_.rootType)

    def rootCode: Option[String] = asts.headOption.flatMap(_.rootCode)

    def rootName: Option[String] = asts.headOption.flatMap(_.rootName)

    def rootCodeOrEmpty: String = asts.rootCode.getOrElse("")
  }
}
