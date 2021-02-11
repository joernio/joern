package io.shiftleft.pythonparser.ast

import java.util
import io.shiftleft.pythonparser.AstVisitor
import scala.jdk.CollectionConverters._

trait icomprehension extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Comprehension(target: iexpr,
                         iter: iexpr,
                         ifs: Iterable[iexpr],
                         is_async: Boolean) extends icomprehension {
  def this(target: iexpr,
           iter: iexpr,
           ifs: util.ArrayList[iexpr],
           is_async: Boolean) = {
    this(target, iter, ifs.asScala, is_async)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
