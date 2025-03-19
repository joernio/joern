package io.shiftleft.semanticcpg.language.nodemethods

import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.codepropertygraph.generated.nodes.{Literal, NewLocation}
import io.shiftleft.semanticcpg.NodeExtension
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.language.nodemethods.LiteralMethods.{delimiters, logger}
import org.apache.commons.lang3.StringUtils
import org.slf4j.{Logger, LoggerFactory}

import scala.collection.immutable.HashMap

class LiteralMethods(val literal: Literal) extends AnyVal with NodeExtension with HasLocation {
  def strippedCode: String = {
    val language         = Cpg(literal.graph).metaData.language.head
    val stringDelimiters = delimiters(language)

    stringDelimiters
      .filter(literal.code.startsWith(_))
      .map(delimiter =>
        val start = literal.code.indexOf(delimiter) + delimiter.length
        val end   = literal.code.lastIndexOf(delimiter)

        literal.code.substring(start, end)
      )
      .headOption
      .getOrElse(literal.code)
  }

  override def location: NewLocation = {
    LocationCreator(literal, literal.code, literal.label, literal.lineNumber, literal.method)

  }
}

object LiteralMethods {
  val logger: Logger = LoggerFactory.getLogger(getClass)
  val delimiters: Map[String, List[String]] = HashMap[String, List[String]](
    Languages.JAVASRC    -> List("\"\"\"", "\""),
    Languages.JAVA       -> List("\"\"\"", "\""),
    Languages.KOTLIN     -> List("\"\"\"", "\""),
    Languages.SWIFTSRC   -> List("\"\"\"", "\""),
    Languages.C          -> List("\"", "'"),
    Languages.NEWC       -> List("\"", "'"),
    Languages.GHIDRA     -> List("\"", "'"),
    Languages.PHP        -> List("\"", "'"),
    Languages.JAVASCRIPT -> List("\"", "'", "`"),
    Languages.JSSRC      -> List("\"", "'", "`"),
    Languages.GOLANG     -> List("\"", "`"),
    Languages.CSHARP     -> List("\""),
    Languages.CSHARPSRC  -> List("\""),
    Languages.RUBYSRC    -> List("\"", "'"),
    Languages.PYTHON     -> List("\"\"\"", "'''", "\"", "'"),
    Languages.PYTHONSRC  -> List("\"\"\"", "'''", "\"", "'")
  )
}
