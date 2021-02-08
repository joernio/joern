package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

import java.util
import scala.jdk.CollectionConverters._

sealed trait iexcepthandler extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ExceptHandler(typ: Option[iexpr],
                         name: Option[String],
                         body: Iterable[istmt],
                         lineno: Int,
                         col_offset: Int) extends iexcepthandler {
  def this(typ: iexpr,
           name: String,
           body: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(Option(typ),
      Option(name),
      body.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
