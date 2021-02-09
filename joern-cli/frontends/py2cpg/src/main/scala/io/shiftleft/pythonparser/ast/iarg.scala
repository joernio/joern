package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

trait iarg extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Arg(arg: String,
               annotation: Option[iexpr],
               type_comment: Option[String],
               lineno: Int,
               col_offset: Int) extends iarg {
  def this(arg: String,
           annotation: iexpr,
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(arg,
      Option(annotation),
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
