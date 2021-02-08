package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

sealed trait iunaryop extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Invert extends iunaryop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Not extends iunaryop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object UAdd extends iunaryop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object USub extends iunaryop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
