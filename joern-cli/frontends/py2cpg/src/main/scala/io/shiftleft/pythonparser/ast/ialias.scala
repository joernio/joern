package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

sealed trait ialias extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Alias(name: String, asName: Option[String]) extends ialias {
  def this(name: String, asName: String) = {
    this(name, Option(asName))
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
