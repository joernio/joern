package io.shiftleft.pythonparser.ast

import java.util
import scala.jdk.CollectionConverters._

sealed trait iexcepthandler extends iast with iattributes

case class ExceptHandler(typ: Option[iexpr],
                         name: Option[String],
                         body: Iterable[istmt],
                         lineno: Int,
                         col_offset: Int) extends iexcepthandler {
  def this(typ: iexpr,
           name: String,
           body: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(Option(typ),
      Option(name),
      body.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }

  override def print: String = {
    "except" +
      typ.map(t => " " + t.print).getOrElse("") +
      name.map(n => " as " + n).getOrElse("") +
      ":" +
      body.map(s => indent(s.print)).mkString("\n", "\n", "")
  }
}
