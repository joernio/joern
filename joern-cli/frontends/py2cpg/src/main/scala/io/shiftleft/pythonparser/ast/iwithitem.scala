package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

trait iwithitem extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class WithItem(context_expr: iexpr, optional_vars: Option[iexpr]) extends iwithitem {
  def this(context_expr: iexpr, optional_vars: iexpr) = {
    this(context_expr, Option(optional_vars))
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
