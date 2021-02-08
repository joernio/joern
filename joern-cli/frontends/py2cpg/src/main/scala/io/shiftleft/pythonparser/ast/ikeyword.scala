package io.shiftleft.pythonparser.ast

sealed trait ikeyword extends iast
case class Keyword(arg: Option[String],
                   value: iexpr,
                   lineno: Int,
                   col_offset: Int) extends ikeyword with iattributes {
  def this(arg: String, value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(arg), value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    arg match {
      case Some(argName) =>
        argName + " = " + value.print
      case None =>
        "**" + value.print
    }
  }
}
