package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

sealed trait iconstant extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class StringConstant(value: String,
                          isRaw: Boolean,
                          isUnicode: Boolean,
                          isByte: Boolean) extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case class BoolConstant(value: Boolean) extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case class IntConstant(value: String) extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case class FloatConstant(value: String) extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object NoneConstant extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object EllipsisConstant extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
