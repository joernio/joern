package io.shiftleft.pythonparser.ast

import io.shiftleft.pythonparser.AstVisitor

import java.util
import scala.jdk.CollectionConverters._

// This file describes the AST classes.
// It tries to stay as close as possible to the AST defined by CPython at
// https://docs.python.org/3/library/ast.html.
// The base type from which all AST classes derive is iast.
// For every left hand side entity in the CPython AST definition there is
// a corresponding lower case trait with an "i" prefixed.
// E.g. stmt => istmt
// If the right hand side entities in the CPython AST are named there is
// a corresponding case class of that name else the case class is named
// as the left hand side with a capitalized start character.
//
// There are some deviations from the CPython AST.
// 1. expr_context is omitted since deriving whether e.g. an attribute is
//    a "load", "store" or "del" is context sensitive and we do not want
//    to keep that context during parsing.
// 2. type_ignore and type_comment are current not populated as comments
//    are currently not put in the normal token stream. This could become
//    a TODO if we need it at some point.
// 3. We added an ErrorStatement in order to reflect parse errors inline
//    with the code we parsed successfully.
// 4. We added a StringExprList since we do not want to combine string
//    constants during parsing. This allows us to be more fine grained
//    in terms of string prefixes like r"someString" or b"someBytes".
// 5. Constants are not evaluated during parsing. E.g. CPython generates
//    an integer constant reflecting the integer value for "0x11" whereas
//    we just keep the string representation.
//    In general expect the constant representation to be different from
//    CPythons.

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST root trait
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iast {
  def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST module classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait imod extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Module(stmts: CollType[istmt], type_ignores: CollType[itype_ignore]) extends imod {
  def this(stmts: util.ArrayList[istmt], type_ignores: util.ArrayList[itype_ignore]) = {
    this(stmts.asScala, type_ignores.asScala)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST statement classes
///////////////////////////////////////////////////////////////////////////////////////////////////

trait istmt extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class FunctionDef(name: String,
                       args: iarguments,
                       body: CollType[istmt],
                       decorator_list: CollType[iexpr],
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
                            body: CollType[istmt],
                            decorator_list: CollType[iexpr],
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

case class ClassDef(name: String,
                    bases: CollType[iexpr],
                    keywords: CollType[ikeyword],
                    body: CollType[istmt],
                    decorator_list: CollType[iexpr],
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

case class Return(value: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Delete(targets: CollType[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(targets: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(targets.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Assign(targets: CollType[iexpr],
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

case class For(target: iexpr,
               iter: iexpr,
               body: CollType[istmt],
               orelse: CollType[istmt],
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
                    body: CollType[istmt],
                    orelse: CollType[istmt],
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

case class While(test: iexpr,
                 body: CollType[istmt],
                 orelse: CollType[istmt],
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

case class If(test: iexpr,
              body: CollType[istmt],
              orelse: CollType[istmt],
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

case class With(items: CollType[iwithitem],
                body: CollType[istmt],
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

case class AsyncWith(items: CollType[iwithitem],
                     body: CollType[istmt],
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

case class Raise(exc: Option[iexpr], cause: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(exc: iexpr, cause: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(exc), Option(cause), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Try(body: CollType[istmt],
               handlers: CollType[iexcepthandler],
               orelse: CollType[istmt],
               finalbody: CollType[istmt],
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

case class Assert(test: iexpr, msg: Option[iexpr], lineno: Int, col_offset: Int) extends istmt {
  def this(test: iexpr, msg: iexpr, attributeProvider: AttributeProvider) = {
    this(test, Option(msg), attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Import(names: CollType[ialias], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[ialias], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ImportFrom(module: Option[String],
                      names: CollType[ialias],
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

case class Global(names: CollType[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Nonlocal(names: CollType[String], lineno: Int, col_offset: Int) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider.lineno, attributeProvider.col_offset)
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

case class Pass(lineno: Int, col_offset: Int) extends istmt {
  def this(attributeProvider: AttributeProvider) = {
    this(attributeProvider.lineno, attributeProvider.col_offset)
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

// This statement is not part of the CPython AST definition and
// was added to represent parse errors inline with valid AST
// statements.
case class ErrorStatement(exception: Exception, lineno: Int, col_offset: Int) extends istmt {
  def this(exception: Exception, attributeProvider: AttributeProvider) = {
    this(exception, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST expression classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iexpr extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class BoolOp(op: iboolop, values: CollType[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iboolop, values: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(op, values.asScala, attributeProvider.lineno, attributeProvider.col_offset)
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

case class UnaryOp(op: iunaryop, operand: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(op: iunaryop, operand: iexpr, attributeProvider: AttributeProvider) = {
    this(op, operand, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Lambda(arg: iarguments, body: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(arg: iarguments, body: iexpr, attributeProvider: AttributeProvider) = {
    this(arg, body, attributeProvider.lineno, attributeProvider.col_offset)
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

case class Dict(keys: CollType[Option[iexpr]],
                values: CollType[iexpr],
                lineno: Int,
                col_offset: Int) extends iexpr {
  def this(keys: util.ArrayList[iexpr],
           values: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(keys.asScala.map(Option.apply), values.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Set(elts: CollType[iexpr],
               lineno: Int,
               col_offset: Int) extends iexpr {
  def this(elts: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ListComp(elt: iexpr,
                    generators: CollType[icomprehension],
                    lineno: Int,
                    col_offset: Int) extends iexpr {
  def this(elt: iexpr,
           generators: util.ArrayList[icomprehension],
           attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class SetComp(elt: iexpr,
                   generators: CollType[icomprehension],
                   lineno: Int,
                   col_offset: Int) extends iexpr {
  def this(elt: iexpr,
           generators: util.ArrayList[icomprehension],
           attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class DictComp(key: iexpr,
                    value: iexpr,
                    generators: CollType[icomprehension],
                    lineno: Int,
                    col_offset: Int) extends iexpr {
  def this(key: iexpr,
           value: iexpr,
           generators: util.ArrayList[icomprehension],
           attributeProvider: AttributeProvider) = {
    this(key, value, generators.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class GeneratorExp(elt: iexpr,
                        generators: CollType[icomprehension],
                        lineno: Int,
                        col_offset: Int) extends iexpr {
  def this(elt: iexpr,
           generators: util.ArrayList[icomprehension],
           attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider.lineno, attributeProvider.col_offset)
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

case class Compare(left: iexpr,
                   ops: CollType[icompop],
                   comparators: CollType[iexpr],
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

case class Call(func: iexpr,
                args: CollType[iexpr],
                keywords: CollType[ikeyword],
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

case class Constant(value: iconstant, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iconstant, attributeProvider: AttributeProvider) = {
    this(value, attributeProvider.lineno, attributeProvider.col_offset)
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

case class Subscript(value: iexpr, slice: iexpr, lineno: Int, col_offset: Int) extends iexpr {
  def this(value: iexpr, slice: iexpr, attributeProvider: AttributeProvider) = {
    this(value, slice, attributeProvider.lineno, attributeProvider.col_offset)
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

case class Name(id: String, lineno: Int, col_offset: Int) extends iexpr {
  def this(id: String, attributeProvider: AttributeProvider) = {
    this(id, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class List(elts: CollType[iexpr],
                lineno: Int,
                col_offset: Int) extends iexpr {
  def this(elts: util.ArrayList[iexpr],
           attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Tuple(elts: CollType[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider.lineno, attributeProvider.col_offset)
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

// This class is not part of the CPython AST definition at
// https://docs.python.org/3/library/ast.html
// But since we do not want to combine strings like CPython does, we need
// this extra kind of expression.
// A StringExpList must always have at least 2 elements and its elements must
// be either a Constant which contains a StringConstant or a JoinedString.
case class StringExpList(elts: CollType[iexpr], lineno: Int, col_offset: Int) extends iexpr {
  assert(elts.size >= 2)
  def this(elts: util.ArrayList[iexpr]) = {
    this(elts.asScala, elts.asScala.head.lineno, elts.asScala.head.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST boolop classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iboolop extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

object And extends iboolop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Or extends iboolop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST operator classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait ioperator extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Add extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Sub extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Mult extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object MatMult extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Div extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Mod extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Pow extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object LShift extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object RShift extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object BitOr extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object BitXor extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object BitAnd extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object FloorDiv extends ioperator {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST unaryop classes
///////////////////////////////////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST compop classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait icompop extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case object Eq extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object NotEq extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Lt extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object LtE extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Gt extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object GtE extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object Is extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object IsNot extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object In extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case object NotIn extends icompop {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST comprehension classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait icomprehension extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Comprehension(target: iexpr,
                         iter: iexpr,
                         ifs: CollType[iexpr],
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST exceptHandler classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iexcepthandler extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ExceptHandler(typ: Option[iexpr],
                         name: Option[String],
                         body: CollType[istmt],
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST arguments classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iarguments extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// Excerpt from the CPython docs:
// posonlyargs, args and kwonlyargs are lists of arg nodes.
// vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
// kw_defaults is a list of default values for keyword-only arguments.
// If one is None, the corresponding argument is required.
// defaults is a list of default values for arguments that can be passed positionally.
// If there are fewer defaults, they correspond to the last n arguments.
case class Arguments(posonlyargs: CollType[iarg],
                     args: CollType[iarg],
                     vararg: Option[iarg],
                     kwonlyargs: CollType[iarg],
                     kw_defaults: CollType[Option[iexpr]],
                     kw_arg: Option[iarg],
                     defaults: CollType[iexpr]) extends iarguments {
  def this(posonlyargs: util.List[iarg],
           args: util.List[iarg],
           vararg: iarg,
           kwonlyargs: util.List[iarg],
           kw_defaults: util.List[iexpr],
           kw_arg: iarg,
           defaults: util.List[iexpr]) = {
    this(posonlyargs.asScala,
      args.asScala,
      Option(vararg),
      kwonlyargs.asScala,
      kw_defaults.asScala.map(Option.apply),
      Option(kw_arg),
      defaults.asScala
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST arg classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iarg extends iast with iattributes {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Arg(arg: String,
               annotation: Option[iexpr],
               type_comment: Option[String],
               lineno: Int,
               col_offset: Int) extends iarg {
  def this(arg: String,
           annotation: iexpr,
           type_comment: String,
           attributeProvider: AttributeProvider) = {
    this(arg,
      Option(annotation),
      Option(type_comment),
      attributeProvider.lineno,
      attributeProvider.col_offset
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST keyword classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait ikeyword extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Keyword(arg: Option[String],
                   value: iexpr,
                   lineno: Int,
                   col_offset: Int) extends ikeyword with iattributes {
  def this(arg: String, value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(arg), value, attributeProvider.lineno, attributeProvider.col_offset)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST alias classes
///////////////////////////////////////////////////////////////////////////////////////////////////
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

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST withitem classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iwithitem extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class WithItem(context_expr: iexpr, optional_vars: Option[iexpr]) extends iwithitem {
  def this(context_expr: iexpr, optional_vars: iexpr) = {
    this(context_expr, Option(optional_vars))
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST type_ignore classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait itype_ignore extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class TypeIgnore(lineno: Int, tag: String) extends itype_ignore {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST attributes classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iattributes {
  val lineno: Int
  val col_offset: Int
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST constant classes
///////////////////////////////////////////////////////////////////////////////////////////////////
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
case class ImaginaryConstant(value: String) extends iconstant {
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
