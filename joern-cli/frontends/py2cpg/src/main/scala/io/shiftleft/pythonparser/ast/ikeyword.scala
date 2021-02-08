package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

sealed trait ikeyword extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Keyword(arg: Option[String],
                   value: iexpr,
                   lineno: Int,
                   col_offset: Int) extends ikeyword with iattributes {
  def this(arg: String, value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(arg), value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
