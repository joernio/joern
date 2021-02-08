package io.shiftleft.pythonparser.ast

import java.util
import scala.jdk.CollectionConverters._

sealed trait iexpr extends iast with iattributes

case class Name(id: String, lineno: Int, col_offset: Int) extends iexpr {
  def this(id: String, attributeProvider: AttributeProvider) = {
    this(id, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    id
  }
}

case class Tuple(elts: Iterable[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }

  override def print: String = {
    if (elts.size == 1) {
      "(" + elts.head.print + ",)"
    } else {
      "(" + elts.map(_.print).mkString(",") + ")"
    }
  }
}

case class Constant(value: iconstant, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iconstant, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    value.print
  }
}

case class IfExp(test: iexpr, body: iexpr, orElse: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(test: iexpr, body: iexpr, orElse: iexpr, attributeProvider: AttributeProvider) = {
    this(test, body, orElse, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    body.print + " if " + test.print + " else " + orElse.print
  }
}

case class BoolOp(op: iboolop, values: Iterable[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iboolop, values: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(op, values.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    val opString = " " + op.print + " "
    values.map(_.print).mkString(opString)
  }
}

case class UnaryOp(op: iunaryop, operand: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iunaryop, operand: iexpr, attributeProvider: AttributeProvider) = {
    this(op, operand, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    val opString = op match {
      case Not =>
        op.print + " "
      case _ =>
        op.print
    }
    opString + operand.print
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

  override def print: String = {
    left.print + ops.zip(comparators).map { case (op, comparator) =>
      " " + op.print + " " + comparator.print
    }.mkString("")
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

  override def print: String = {
    left.print + " " + op.print + " " + right.print
  }
}

case class Attribute(value: iexpr, attr: String, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attr: String, attributeProvider: AttributeProvider) = {
    this(value, attr, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    value.print + "." + attr
  }
}

case class NamedExpr(target: iexpr, value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(target: iexpr, value: iexpr, attributeProvider: AttributeProvider) = {
    this(target, value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    target.print + " := " + value.print
  }
}

case class Starred(value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "*" + value.print
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
  override def print: String = {
    val optionArgEndComma = if (args.nonEmpty && keywords.nonEmpty) ", " else ""
    func.print + "(" + args.map(_.print).mkString(", ") + optionArgEndComma +
      keywords.map(_.print).mkString(", ") + ")"
  }
}

case class Subscript(value: iexpr, slice: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, slice: iexpr, attributeProvider: AttributeProvider) = {
    this(value, slice, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    value.print + "[" + slice.print + "]"
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
  override def print: String = {
    lower.map(_.print).getOrElse("") +
      ":" + upper.map(_.print).getOrElse("") +
      step.map(expr => ":" + expr.print).getOrElse("")
  }
}

case class Yield(value: Option[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "yield" + value.map(v => " " + v.print).getOrElse("")
  }
}

case class YieldFrom(value: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def print: String = {
    "yield from " + value.print
  }
}
