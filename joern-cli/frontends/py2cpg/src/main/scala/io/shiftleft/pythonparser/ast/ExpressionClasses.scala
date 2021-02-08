package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

import java.util
import scala.jdk.CollectionConverters._

sealed trait iexpr extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Name(id: String, lineno: Int, col_offset: Int) extends iexpr {
  def this(id: String, attributeProvider: AttributeProvider) = {
    this(id, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Tuple(elts: Iterable[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Constant(value: iconstant, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iconstant, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class IfExp(test: iexpr, body: iexpr, orElse: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(test: iexpr, body: iexpr, orElse: iexpr, attributeProvider: AttributeProvider) = {
    this(test, body, orElse, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class BoolOp(op: iboolop, values: Iterable[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iboolop, values: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(op, values.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class UnaryOp(op: iunaryop, operand: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iunaryop, operand: iexpr, attributeProvider: AttributeProvider) = {
    this(op, operand, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Compare(left: iexpr,
                   ops: Iterable[icompop],
                   comparators: Iterable[iexpr],
                   lineno: Int,
                   col_offset: Int) extends iexpr {
  def this(left: iexpr,
           ops: util.ArrayList[icompop],
           comparators: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(left, ops.asScala, comparators.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class BinOp(left: iexpr,
                 op: ioperator,
                 right: iexpr,
                 lineno: Int,
                 col_offset: Int) extends iexpr {
  def this(left: iexpr,
           op: ioperator,
           right: iexpr,
           attributeProvider: AttributeProvider) = {
    this(left, op, right, attributeProvider.lineno, attributeProvider.col_offset)
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Attribute(value: iexpr, attr: String, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attr: String, attributeProvider: AttributeProvider) = {
    this(value, attr, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class NamedExpr(target: iexpr, value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(target: iexpr, value: iexpr, attributeProvider: AttributeProvider) = {
    this(target, value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Starred(value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Call(func: iexpr,
                args: Iterable[iexpr],
                keywords: Iterable[ikeyword],
                lineno: Int,
                col_offset: Int) extends iexpr {
  def this(func: iexpr,
           args: util.ArrayList[iexpr],
           keywords: util.ArrayList[ikeyword],
           attributeProvider: AttributeProvider) = {
    this(func, args.asScala, keywords.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Subscript(value: iexpr, slice: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, slice: iexpr, attributeProvider: AttributeProvider) = {
    this(value, slice, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Slice(lower: Option[iexpr],
                 upper: Option[iexpr],
                 step: Option[iexpr],
                 lineno: Int,
                 col_offset: Int) extends iexpr {
  def this(lower: iexpr, upper: iexpr, step: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(lower), Option(upper), Option(step),
      attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Yield(value: Option[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class YieldFrom(value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Await(value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr) = {
    this(value, value.lineno, value.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
