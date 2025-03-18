package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.nodemethods.LiteralMethods.delimiters
import org.apache.commons.lang3.StringUtils

class LiteralMethods(val literal: Literal) extends AnyVal with NodeExtension with HasLocation {
  def innerText: Option[String] = {
    delimiters(literal)
      .filter(literal.code.startsWith(_))
      .map(StringUtils.strip(literal.code, _))
      .headOption
  }

  override def location: NewLocation = {
    LocationCreator(literal, literal.code, literal.label, literal.lineNumber, literal.method)

  }
}

object LiteralMethods {
  def delimiters(literal: Literal): List[String] = Cpg(literal.graph).metaData.language.headOption match {
    case Some(Languages.JAVASRC | Languages.JAVA | Languages.KOTLIN | Languages.SWIFTSRC) =>
      "\"\"\"" :: "\"" :: Nil
    case Some(Languages.C | Languages.NEWC | Languages.PHP) =>
      "\"" :: "'" :: Nil
    case Some(Languages.JAVASCRIPT | Languages.JSSRC) =>
      "\"" :: "'" :: "`" :: Nil
    case Some(Languages.GOLANG) =>
      "\"" :: "`" :: Nil
    case Some(Languages.CSHARP | Languages.CSHARPSRC) =>
      "\"" :: Nil
    case Some(Languages.RUBYSRC) =>
      "\"" :: "'" :: Nil
    case Some(Languages.PYTHON | Languages.PYTHONSRC) =>
      "\"\"\"" :: "'''" :: "\"" :: "'" :: Nil
    case _ =>
      "\"" :: Nil
  }
}
