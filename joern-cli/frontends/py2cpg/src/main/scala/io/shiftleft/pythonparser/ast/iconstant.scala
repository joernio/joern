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
// Python integer class is unbounded. Lets use BigInt to
// reflect this and see whether that gets to expensive.
case class IntConstant(value: BigInt) extends iconstant {
  def this(value: String) = {
    this(BigInt(value))
  }
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
