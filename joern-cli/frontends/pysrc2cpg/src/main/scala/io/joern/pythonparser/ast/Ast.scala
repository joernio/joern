package io.joern.pythonparser.ast

import io.joern.pythonparser.AstVisitor

import java.util
import scala.jdk.CollectionConverters.*

// This file describes the AST classes.
// It tries to stay as close as possible to the AST defined by CPython at
// https://docs.python.org/3/library/ast.html.
// The base type from which all AST classes derive is iast.
// For every left hand side entity in the CPython AST definition there is
// a corresponding lower case trait with an "i" prefixed, if there is
// more than one right hand side entity. Otherwise the left hand side
// entity becomes a class with a capitalized start character.
// E.g. stmt => istmt
//      arguments => Arguments
//
// There are some deviations from the CPython AST.
// 1. expr_context is omitted since deriving whether e.g. an attribute is
//    a "load", "store" or "del" is context sensitive and we do not want
//    to keep that context during parsing.
// 2. type_ignore and type_comment are currently not populated as comments
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
  def accept[T](visitor: AstVisitor[T]): T
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST module classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait imod extends iast

case class Module(stmts: CollType[istmt], type_ignores: CollType[TypeIgnore]) extends imod {
  def this(stmts: util.ArrayList[istmt], type_ignores: util.ArrayList[TypeIgnore]) = {
    this(stmts.asScala, type_ignores.asScala)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST statement classes
///////////////////////////////////////////////////////////////////////////////////////////////////

trait istmt extends iast with iattributes

case class FunctionDef(
  name: String,
  args: Arguments,
  body: CollType[istmt],
  decorator_list: CollType[iexpr],
  returns: Option[iexpr],
  type_comment: Option[String],
  type_params: CollType[itypeParam],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    name: String,
    args: Arguments,
    body: util.ArrayList[istmt],
    decorator_list: util.ArrayList[iexpr],
    returns: iexpr,
    type_comment: String,
    type_params: util.ArrayList[itypeParam],
    attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
      type_params.asScala,
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncFunctionDef(
  name: String,
  args: Arguments,
  body: CollType[istmt],
  decorator_list: CollType[iexpr],
  returns: Option[iexpr],
  type_comment: Option[String],
  type_params: CollType[itypeParam],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    name: String,
    args: Arguments,
    body: util.ArrayList[istmt],
    decorator_list: util.ArrayList[iexpr],
    returns: iexpr,
    type_comment: String,
    type_params: util.ArrayList[itypeParam],
    attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
      type_params.asScala,
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ClassDef(
  name: String,
  bases: CollType[iexpr],
  keywords: CollType[Keyword],
  body: CollType[istmt],
  decorator_list: CollType[iexpr],
  type_params: CollType[itypeParam],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    name: String,
    bases: util.ArrayList[iexpr],
    keywords: util.ArrayList[Keyword],
    body: util.ArrayList[istmt],
    decorator_list: util.ArrayList[iexpr],
    type_params: util.ArrayList[itypeParam],
    attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      bases.asScala,
      keywords.asScala,
      body.asScala,
      decorator_list.asScala,
      type_params.asScala,
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Return(value: Option[iexpr], attributeProvider: AttributeProvider) extends istmt {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Delete(targets: CollType[iexpr], attributeProvider: AttributeProvider) extends istmt {
  def this(targets: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(targets.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Assign(
  targets: CollType[iexpr],
  value: iexpr,
  typeComment: Option[String],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(targets: util.ArrayList[iexpr], value: iexpr, attributeProvider: AttributeProvider) = {
    this(targets.asScala, value, None, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class TypeAlias(name: iexpr, type_params: CollType[itypeParam], value: iexpr, attributeProvider: AttributeProvider)
    extends istmt {
  def this(name: iexpr, typeParams: util.ArrayList[itypeParam], value: iexpr, attributeProvider: AttributeProvider) = {
    this(name, typeParams.asScala, value, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AugAssign(target: iexpr, op: ioperator, value: iexpr, attributeProvider: AttributeProvider) extends istmt {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AnnAssign(
  target: iexpr,
  annotation: iexpr,
  value: Option[iexpr],
  simple: Boolean,
  attributeProvider: AttributeProvider
) extends istmt {
  def this(target: iexpr, annotation: iexpr, value: iexpr, simple: Boolean, attributeProvider: AttributeProvider) = {
    this(target, annotation, Option(value), simple, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class For(
  target: iexpr,
  iter: iexpr,
  body: CollType[istmt],
  orelse: CollType[istmt],
  type_comment: Option[String],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    target: iexpr,
    iter: iexpr,
    body: util.ArrayList[istmt],
    orelse: util.ArrayList[istmt],
    type_comment: String,
    attributeProvider: AttributeProvider
  ) = {
    this(target, iter, body.asScala, orelse.asScala, Option(type_comment), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncFor(
  target: iexpr,
  iter: iexpr,
  body: CollType[istmt],
  orelse: CollType[istmt],
  type_comment: Option[String],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    target: iexpr,
    iter: iexpr,
    body: util.ArrayList[istmt],
    orelse: util.ArrayList[istmt],
    type_comment: String,
    attributeProvider: AttributeProvider
  ) = {
    this(target, iter, body.asScala, orelse.asScala, Option(type_comment), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class While(test: iexpr, body: CollType[istmt], orelse: CollType[istmt], attributeProvider: AttributeProvider)
    extends istmt {
  def this(
    test: iexpr,
    body: util.ArrayList[istmt],
    orelse: util.ArrayList[istmt],
    attributeProvider: AttributeProvider
  ) = {
    this(test, body.asScala, orelse.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class If(test: iexpr, body: CollType[istmt], orelse: CollType[istmt], attributeProvider: AttributeProvider)
    extends istmt {
  def this(
    test: iexpr,
    body: util.ArrayList[istmt],
    orelse: util.ArrayList[istmt],
    attributeProvider: AttributeProvider
  ) = {
    this(test, body.asScala, orelse.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class With(
  items: CollType[Withitem],
  body: CollType[istmt],
  type_comment: Option[String],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    items: util.ArrayList[Withitem],
    body: util.ArrayList[istmt],
    type_comment: String,
    attributeProvider: AttributeProvider
  ) = {
    this(items.asScala, body.asScala, Option(type_comment), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncWith(
  items: CollType[Withitem],
  body: CollType[istmt],
  type_comment: Option[String],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    items: util.ArrayList[Withitem],
    body: util.ArrayList[istmt],
    type_comment: String,
    attributeProvider: AttributeProvider
  ) = {
    this(items.asScala, body.asScala, Option(type_comment), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Match(subject: iexpr, cases: CollType[MatchCase], attributeProvider: AttributeProvider) extends istmt {
  def this(subject: iexpr, cases: util.List[MatchCase], attributeProvider: AttributeProvider) = {
    this(subject, cases.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Raise(exc: Option[iexpr], cause: Option[iexpr], attributeProvider: AttributeProvider) extends istmt {
  def this(exc: iexpr, cause: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(exc), Option(cause), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Try(
  body: CollType[istmt],
  handlers: CollType[ExceptHandler],
  orelse: CollType[istmt],
  finalbody: CollType[istmt],
  attributeProvider: AttributeProvider
) extends istmt {
  def this(
    body: util.ArrayList[istmt],
    handlers: util.ArrayList[ExceptHandler],
    orelse: util.ArrayList[istmt],
    finalbody: util.ArrayList[istmt],
    attributeProvider: AttributeProvider
  ) = {
    this(body.asScala, handlers.asScala, orelse.asScala, finalbody.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Assert(test: iexpr, msg: Option[iexpr], attributeProvider: AttributeProvider) extends istmt {
  def this(test: iexpr, msg: iexpr, attributeProvider: AttributeProvider) = {
    this(test, Option(msg), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Import(names: CollType[Alias], attributeProvider: AttributeProvider) extends istmt {
  def this(names: util.ArrayList[Alias], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ImportFrom(module: Option[String], names: CollType[Alias], level: Int, attributeProvider: AttributeProvider)
    extends istmt {
  def this(module: String, names: util.ArrayList[Alias], level: Int, attributeProvider: AttributeProvider) = {
    this(Option(module), names.asScala, level, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Global(names: CollType[String], attributeProvider: AttributeProvider) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Nonlocal(names: CollType[String], attributeProvider: AttributeProvider) extends istmt {
  def this(names: util.ArrayList[String], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Expr(value: iexpr, attributeProvider: AttributeProvider) extends istmt {
  def this(value: iexpr) = {
    this(value, value.attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Pass(attributeProvider: AttributeProvider) extends istmt {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Break(attributeProvider: AttributeProvider) extends istmt {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Continue(attributeProvider: AttributeProvider) extends istmt {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// This is the python2 raise statement.
// It is different enough from the python3 version to justify an
// extra class.
// Note that not all raise statements found in python2 code are
// represented as and instance of this class. Only if the syntax
// of a raise does not match the python3 syntax we represent it
// as such a class.
case class RaiseP2(typ: Option[iexpr], inst: Option[iexpr], tback: Option[iexpr], attributeProvider: AttributeProvider)
    extends istmt {
  def this(typ: iexpr, inst: iexpr, tback: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(typ), Option(inst), Option(tback), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// This statement is not part of the CPython AST definition and
// was added to represent parse errors inline with valid AST
// statements.
case class ErrorStatement(exception: Exception, attributeProvider: AttributeProvider) extends istmt {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST expression classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iexpr extends iast with iattributes

case class BoolOp(op: iboolop, values: CollType[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  def this(op: iboolop, values: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(op, values.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class NamedExpr(target: iexpr, value: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class BinOp(left: iexpr, op: ioperator, right: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class UnaryOp(op: iunaryop, operand: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Lambda(args: Arguments, body: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class IfExp(test: iexpr, body: iexpr, orelse: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Dict(keys: CollType[Option[iexpr]], values: CollType[iexpr], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(keys: util.ArrayList[iexpr], values: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(keys.asScala.map(Option.apply), values.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Set(elts: CollType[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ListComp(elt: iexpr, generators: CollType[Comprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(elt: iexpr, generators: util.ArrayList[Comprehension], attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class SetComp(elt: iexpr, generators: CollType[Comprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(elt: iexpr, generators: util.ArrayList[Comprehension], attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class DictComp(key: iexpr, value: iexpr, generators: CollType[Comprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(
    key: iexpr,
    value: iexpr,
    generators: util.ArrayList[Comprehension],
    attributeProvider: AttributeProvider
  ) = {
    this(key, value, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class GeneratorExp(elt: iexpr, generators: CollType[Comprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(elt: iexpr, generators: util.ArrayList[Comprehension], attributeProvider: AttributeProvider) = {
    this(elt, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Await(value: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  def this(value: iexpr) = {
    this(value, value.attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Yield(value: Option[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  def this(value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(value), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class YieldFrom(value: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Compare(
  left: iexpr,
  ops: CollType[icompop],
  comparators: CollType[iexpr],
  attributeProvider: AttributeProvider
) extends iexpr {
  def this(
    left: iexpr,
    ops: util.ArrayList[icompop],
    comparators: util.ArrayList[iexpr],
    attributeProvider: AttributeProvider
  ) = {
    this(left, ops.asScala, comparators.asScala, attributeProvider)
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Call(func: iexpr, args: CollType[iexpr], keywords: CollType[Keyword], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(
    func: iexpr,
    args: util.ArrayList[iexpr],
    keywords: util.ArrayList[Keyword],
    attributeProvider: AttributeProvider
  ) = {
    this(func, args.asScala, keywords.asScala, attributeProvider)
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// In addition to the CPython version of this class we also stored
// whether the value expression was followed by "=" in "equalSign".
// In deviation to CPython format_spec is of type String and not
// a JoinedString itself. This way we do not have to handle recursive
// format string parsing yet.
case class FormattedValue(
  value: iexpr,
  conversion: Int,
  format_spec: Option[String],
  equalSign: Boolean,
  attributeProvider: AttributeProvider
) extends iexpr {
  def this(
    value: iexpr,
    conversion: Int,
    format_spec: String,
    equalSign: Boolean,
    attributeProvider: AttributeProvider
  ) = {
    this(value, conversion, Option(format_spec), equalSign, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

// In addition to the CPython version of this class we have the fields
// "quote" which stores the kind of quote used and "prefix"
// which stores the exact prefix used with the string.
case class JoinedString(values: CollType[iexpr], quote: String, prefix: String, attributeProvider: AttributeProvider)
    extends iexpr {
  def this(values: util.List[iexpr], quote: String, prefix: String, attributeProvider: AttributeProvider) = {
    this(values.asScala, quote, prefix, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Constant(value: iconstant, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Attribute(value: iexpr, attr: String, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Subscript(value: iexpr, slice: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Starred(value: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Name(id: String, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class List(elts: CollType[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Tuple(elts: CollType[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  def this(elts: util.ArrayList[iexpr], attributeProvider: AttributeProvider) = {
    this(elts.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Slice(lower: Option[iexpr], upper: Option[iexpr], step: Option[iexpr], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(lower: iexpr, upper: iexpr, step: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(lower), Option(upper), Option(step), attributeProvider)
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
case class StringExpList(elts: CollType[iexpr], attributeProvider: AttributeProvider) extends iexpr {
  assert(elts.size >= 2)
  def this(elts: util.ArrayList[iexpr]) = {
    this(elts.asScala, elts.asScala.head.attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST boolop classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iboolop extends iast

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
sealed trait ioperator extends iast

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
sealed trait iunaryop extends iast

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
sealed trait icompop extends iast

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
case class Comprehension(target: iexpr, iter: iexpr, ifs: CollType[iexpr], is_async: Boolean) extends iast {
  def this(target: iexpr, iter: iexpr, ifs: util.ArrayList[iexpr], is_async: Boolean) = {
    this(target, iter, ifs.asScala, is_async)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST exceptHandler classes
///////////////////////////////////////////////////////////////////////////////////////////////////
case class ExceptHandler(
  typ: Option[iexpr],
  name: Option[String],
  body: CollType[istmt],
  attributeProvider: AttributeProvider
) extends iast
    with iattributes {
  def this(typ: iexpr, name: String, body: util.ArrayList[istmt], attributeProvider: AttributeProvider) = {
    this(Option(typ), Option(name), body.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST arguments classes
///////////////////////////////////////////////////////////////////////////////////////////////////
// Excerpt from the CPython docs:
// posonlyargs, args and kwonlyargs are lists of arg nodes.
// vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
// kw_defaults is a list of default values for keyword-only arguments.
// If one is None, the corresponding argument is required.
// defaults is a list of default values for arguments that can be passed positionally.
// If there are fewer defaults, they correspond to the last n arguments.
case class Arguments(
  posonlyargs: CollType[Arg],
  args: CollType[Arg],
  vararg: Option[Arg],
  kwonlyargs: CollType[Arg],
  kw_defaults: CollType[Option[iexpr]],
  kw_arg: Option[Arg],
  defaults: CollType[iexpr]
) extends iast {
  def this(
    posonlyargs: util.List[Arg],
    args: util.List[Arg],
    vararg: Arg,
    kwonlyargs: util.List[Arg],
    kw_defaults: util.List[iexpr],
    kw_arg: Arg,
    defaults: util.List[iexpr]
  ) = {
    this(
      posonlyargs.asScala,
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
case class Arg(
  arg: String,
  annotation: Option[iexpr],
  type_comment: Option[String],
  attributeProvider: AttributeProvider
) extends iast
    with iattributes {
  def this(arg: String, annotation: iexpr, type_comment: String, attributeProvider: AttributeProvider) = {
    this(arg, Option(annotation), Option(type_comment), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST keyword classes
///////////////////////////////////////////////////////////////////////////////////////////////////
case class Keyword(arg: Option[String], value: iexpr, attributeProvider: AttributeProvider)
    extends iast
    with iattributes {
  def this(arg: String, value: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(arg), value, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST alias classes
///////////////////////////////////////////////////////////////////////////////////////////////////

case class Alias(name: String, asName: Option[String]) extends iast {
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
case class Withitem(context_expr: iexpr, optional_vars: Option[iexpr]) extends iast {
  def this(context_expr: iexpr, optional_vars: iexpr) = {
    this(context_expr, Option(optional_vars))
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST match_case classes
///////////////////////////////////////////////////////////////////////////////////////////////////
case class MatchCase(pattern: ipattern, guard: Option[iexpr], body: CollType[istmt]) extends iast {
  def this(pattern: ipattern, guard: iexpr, body: util.List[istmt]) = {
    this(pattern, Option(guard), body.asScala)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST pattern classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait ipattern extends iast with iattributes

case class MatchValue(value: iexpr, attributeProvider: AttributeProvider) extends ipattern {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchSingleton(value: iconstant, attributeProvider: AttributeProvider) extends ipattern {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchSequence(patterns: CollType[ipattern], attributeProvider: AttributeProvider) extends ipattern {
  def this(patterns: util.List[ipattern], attributeProvider: AttributeProvider) = {
    this(patterns.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchMapping(
  keys: CollType[iexpr],
  patterns: CollType[ipattern],
  rest: Option[String],
  attributeProvider: AttributeProvider
) extends ipattern {
  def this(
    keys: util.List[iexpr],
    patterns: util.List[ipattern],
    rest: String,
    attributeProvider: AttributeProvider
  ) = {
    this(keys.asScala, patterns.asScala, Option(rest), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchClass(
  cls: iexpr,
  patterns: CollType[ipattern],
  kwd_attrs: CollType[String],
  kwd_patterns: CollType[ipattern],
  attributeProvider: AttributeProvider
) extends ipattern {
  def this(
    cls: iexpr,
    patterns: util.List[ipattern],
    kwd_attrs: util.List[String],
    kwd_patterns: util.List[ipattern],
    attributeProvider: AttributeProvider
  ) = {
    this(cls, patterns.asScala, kwd_attrs.asScala, kwd_patterns.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchStar(name: Option[String], attributeProvider: AttributeProvider) extends ipattern {
  def this(name: String, attributeProvider: AttributeProvider) = {
    this(Option(name), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchAs(pattern: Option[ipattern], name: Option[String], attributeProvider: AttributeProvider)
    extends ipattern {
  def this(pattern: ipattern, name: String, attributeProvider: AttributeProvider) = {
    this(Option(pattern), Option(name), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class MatchOr(patterns: CollType[ipattern], attributeProvider: AttributeProvider) extends ipattern {
  def this(patterns: util.List[ipattern], attributeProvider: AttributeProvider) = {
    this(patterns.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST type_ignore classes
///////////////////////////////////////////////////////////////////////////////////////////////////
case class TypeIgnore(lineno: Int, tag: String) extends iast {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST type_param classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait itypeParam extends iast with iattributes

case class TypeVar(name: String, bound: Option[iexpr], attributeProvider: AttributeProvider) extends itypeParam {
  def this(name: String, bound: iexpr, attributeProvider: AttributeProvider) = {
    this(name, Option(bound), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ParamSpec(name: String, attributeProvider: AttributeProvider) extends itypeParam {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class TypeVarTuple(name: String, attributeProvider: AttributeProvider) extends itypeParam {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST attributes classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iattributes {
  val attributeProvider: AttributeProvider
  def lineno: Int           = attributeProvider.lineno
  def col_offset: Int       = attributeProvider.col_offset
  def input_offset: Int     = attributeProvider.input_offset
  def end_lineno: Int       = attributeProvider.end_lineno
  def end_col_offset: Int   = attributeProvider.end_col_offset
  def end_input_offset: Int = attributeProvider.end_input_offset
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST constant classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iconstant extends iast

case class StringConstant(value: String, quote: String, prefix: String) extends iconstant {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}
case class JoinedStringConstant(value: String) extends iconstant {
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
