package io.shiftleft.pythonparser.ast


import io.shiftleft.pythonparser.AstVisitor

import java.util
import scala.jdk.CollectionConverters._

trait istmt extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ErrorStatement(lineno: Int, col_offset: Int) extends istmt

case class Module(stmts: Iterable[istmt]) extends imod {
  def this(stmts: util.ArrayList[istmt]) = {
    this(stmts.asScala)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Assign(targets: Iterable[iexpr],
                  value: iexpr,
                  typeComment: Option[String],
                  lineno: Int,
                  col_offset: Int) extends istmt {
  def this(targets: util.ArrayList[iexpr],
           value: iexpr,
           attributeProvider: AttributeProvider) = {
    this(targets.asScala, value, None, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AnnAssign(target: iexpr,
                     annotation: iexpr,
                     value: Option[iexpr],
                     simple: Boolean,
                     lineno: Int,
                     col_offset: Int) extends istmt {
  def this(target: iexpr,
           annotation: iexpr,
           value: iexpr,
           simple: Boolean,
           attributeProvider: AttributeProvider) = {
    this(target, annotation, Option(value), simple, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AugAssign(target: iexpr,
                     op: ioperator,
                     value: iexpr,
                     lineno: Int,
                     col_offset: Int) extends istmt {
  def this(target: iexpr,
           op: ioperator,
           value: iexpr,
           attributeProvider: AttributeProvider) = {
    this(target, op, value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Expr(value: iexpr, lineno: Int, col_offset: Int) extends istmt {
  def this(value: iexpr) = {
    this(value, value.lineno, value.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Return(value: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Import(names: Iterable[ialias], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[ialias], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ImportFrom(module: Option[String],
                      names: Iterable[ialias],
                      level: Int,
                      lineno: Int,
                      col_offset: Int) extends istmt {
  def this(module: String, names: util.ArrayList[ialias], level: Int, attributeProvider: AttributeProvider) = {
    this(Option(module), names.asScala, level, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Raise(exc: Option[iexpr], cause: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(exc: iexpr, cause: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(exc), Option(cause), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Pass(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Delete(targets: Iterable[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(targets: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(targets.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Assert(test: iexpr, msg: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(test: iexpr, msg: iexpr, attributeProvider: AttributeProvider) = {
    this(test, Option(msg), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Break(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Continue(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Global(names: Iterable[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Nonlocal(names: Iterable[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class If(test: iexpr,
              body: Iterable[istmt],
              orelse: Iterable[istmt],
              lineno: Int,
              col_offset: Int) extends istmt {
  def this(test: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(test, body.asScala, orelse.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class While(test: iexpr,
                 body: Iterable[istmt],
                 orelse: Iterable[istmt],
                 lineno: Int,
                 col_offset: Int) extends istmt {
  def this(test: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(test, body.asScala, orelse.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Try(body: Iterable[istmt],
               handlers: Iterable[iexcepthandler],
               orelse: Iterable[istmt],
               finalbody: Iterable[istmt],
               lineno: Int,
               col_offset: Int) extends istmt {
  def this(body: util.ArrayList[istmt],
           handlers: util.ArrayList[iexcepthandler],
           orelse: util.ArrayList[istmt],
           finalbody: util.ArrayList[istmt],
           attributeProvider: AttributeProvider) = {
    this(body.asScala,
      handlers.asScala,
      orelse.asScala,
      finalbody.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ClassDef(name: String,
                    bases: Iterable[iexpr],
                    keywords: Iterable[ikeyword],
                    body: Iterable[istmt],
                    decorator_list: Iterable[iexpr],
                    lineno: Int,
                    col_offset: Int) extends istmt {
  def this(name: String,
           bases: util.ArrayList[iexpr],
           keywords: util.ArrayList[ikeyword],
           body: util.ArrayList[istmt],
           decorator_list: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(name,
      bases.asScala,
      keywords.asScala,
      body.asScala,
      decorator_list.asScala,
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class FunctionDef(name: String,
                       args: iarguments,
                       body: Iterable[istmt],
                       decorator_list: Iterable[iexpr],
                       returns: Option[iexpr],
                       type_comment: Option[String],
                       lineno: Int,
                       col_offset: Int) extends istmt {
  def this(name: String,
           args: iarguments,
           body: util.ArrayList[istmt],
           decorator_list: util.ArrayList[iexpr],
           returns: iexpr,
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncFunctionDef(name: String,
                            args: iarguments,
                            body: Iterable[istmt],
                            decorator_list: Iterable[iexpr],
                            returns: Option[iexpr],
                            type_comment: Option[String],
                            lineno: Int,
                            col_offset: Int) extends istmt {
  def this(name: String,
           args: iarguments,
           body: util.ArrayList[istmt],
           decorator_list: util.ArrayList[iexpr],
           returns: iexpr,
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class With(items: Iterable[iwithitem],
                body: Iterable[istmt],
                type_comment: Option[String],
                lineno: Int,
                col_offset: Int) extends istmt {
  def this(items: util.ArrayList[iwithitem],
           body: util.ArrayList[istmt],
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(items.asScala,
      body.asScala,
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncWith(items: Iterable[iwithitem],
                     body: Iterable[istmt],
                     type_comment: Option[String],
                     lineno: Int,
                     col_offset: Int) extends istmt {
  def this(items: util.ArrayList[iwithitem],
           body: util.ArrayList[istmt],
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(items.asScala,
      body.asScala,
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class For(target: iexpr,
               iter: iexpr,
               body: Iterable[istmt],
               orelse: Iterable[istmt],
               type_comment: Option[String],
               lineno: Int,
               col_offset: Int) extends istmt {
  def this(target: iexpr,
           iter: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(target,
      iter,
      body.asScala,
      orelse.asScala,
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncFor(target: iexpr,
                    iter: iexpr,
                    body: Iterable[istmt],
                    orelse: Iterable[istmt],
                    type_comment: Option[String],
                    lineno: Int,
                    col_offset: Int) extends istmt {
  def this(target: iexpr,
           iter: iexpr,
           body: util.ArrayList[istmt],
           orelse: util.ArrayList[istmt],
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(target,
      iter,
      body.asScala,
      orelse.asScala,
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
