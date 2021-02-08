package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

trait iast {
  def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
trait imod extends iast

