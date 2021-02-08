package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

sealed trait iboolop extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

object And extends iboolop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Or extends iboolop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
