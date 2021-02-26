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
  def accept[T](visitor: AstVisitor[T]): T
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST module classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait imod extends iast

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

trait istmt extends iast with iattributes

case class FunctionDef(
    name: String,
    args: iarguments,
    body: CollType[istmt],
    decorator_list: CollType[iexpr],
    returns: Option[iexpr],
    type_comment: Option[String],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      name: String,
      args: iarguments,
      body: util.ArrayList[istmt],
      decorator_list: util.ArrayList[iexpr],
      returns: iexpr,
      type_comment: String,
      attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncFunctionDef(
    name: String,
    args: iarguments,
    body: CollType[istmt],
    decorator_list: CollType[iexpr],
    returns: Option[iexpr],
    type_comment: Option[String],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      name: String,
      args: iarguments,
      body: util.ArrayList[istmt],
      decorator_list: util.ArrayList[iexpr],
      returns: iexpr,
      type_comment: String,
      attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      args,
      body.asScala,
      decorator_list.asScala,
      Option(returns),
      Option(type_comment),
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
    keywords: CollType[ikeyword],
    body: CollType[istmt],
    decorator_list: CollType[iexpr],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      name: String,
      bases: util.ArrayList[iexpr],
      keywords: util.ArrayList[ikeyword],
      body: util.ArrayList[istmt],
      decorator_list: util.ArrayList[iexpr],
      attributeProvider: AttributeProvider
  ) = {
    this(
      name,
      bases.asScala,
      keywords.asScala,
      body.asScala,
      decorator_list.asScala,
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

case class AugAssign(target: iexpr, op: ioperator, value: iexpr, attributeProvider: AttributeProvider)
    extends istmt {
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
  def this(
      target: iexpr,
      annotation: iexpr,
      value: iexpr,
      simple: Boolean,
      attributeProvider: AttributeProvider
  ) = {
    this(
      target,
      annotation,
      Option(value),
      simple,
      attributeProvider
    )
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
    this(
      target,
      iter,
      body.asScala,
      orelse.asScala,
      Option(type_comment),
      attributeProvider
    )
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
    this(
      target,
      iter,
      body.asScala,
      orelse.asScala,
      Option(type_comment),
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class While(
    test: iexpr,
    body: CollType[istmt],
    orelse: CollType[istmt],
    attributeProvider: AttributeProvider
) extends istmt {
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

case class If(
    test: iexpr,
    body: CollType[istmt],
    orelse: CollType[istmt],
    attributeProvider: AttributeProvider
) extends istmt {
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
    items: CollType[iwithitem],
    body: CollType[istmt],
    type_comment: Option[String],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      items: util.ArrayList[iwithitem],
      body: util.ArrayList[istmt],
      type_comment: String,
      attributeProvider: AttributeProvider
  ) = {
    this(
      items.asScala,
      body.asScala,
      Option(type_comment),
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class AsyncWith(
    items: CollType[iwithitem],
    body: CollType[istmt],
    type_comment: Option[String],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      items: util.ArrayList[iwithitem],
      body: util.ArrayList[istmt],
      type_comment: String,
      attributeProvider: AttributeProvider
  ) = {
    this(
      items.asScala,
      body.asScala,
      Option(type_comment),
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Raise(exc: Option[iexpr], cause: Option[iexpr], attributeProvider: AttributeProvider)
    extends istmt {
  def this(exc: iexpr, cause: iexpr, attributeProvider: AttributeProvider) = {
    this(Option(exc), Option(cause), attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Try(
    body: CollType[istmt],
    handlers: CollType[iexcepthandler],
    orelse: CollType[istmt],
    finalbody: CollType[istmt],
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      body: util.ArrayList[istmt],
      handlers: util.ArrayList[iexcepthandler],
      orelse: util.ArrayList[istmt],
      finalbody: util.ArrayList[istmt],
      attributeProvider: AttributeProvider
  ) = {
    this(
      body.asScala,
      handlers.asScala,
      orelse.asScala,
      finalbody.asScala,
      attributeProvider
    )
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

case class Import(names: CollType[ialias], attributeProvider: AttributeProvider) extends istmt {
  def this(names: util.ArrayList[ialias], attributeProvider: AttributeProvider) = {
    this(names.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class ImportFrom(
    module: Option[String],
    names: CollType[ialias],
    level: Int,
    attributeProvider: AttributeProvider
) extends istmt {
  def this(
      module: String,
      names: util.ArrayList[ialias],
      level: Int,
      attributeProvider: AttributeProvider
  ) = {
    this(
      Option(module),
      names.asScala,
      level,
      attributeProvider
    )
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
case class RaiseP2(typ: Option[iexpr],
                   inst: Option[iexpr],
                   tback: Option[iexpr],
                   attributeProvider: AttributeProvider) extends istmt {
  def this(typ: iexpr,
           inst: iexpr,
           tback: iexpr,
           attributeProvider: AttributeProvider) = {
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

case class BoolOp(op: iboolop, values: CollType[iexpr], attributeProvider: AttributeProvider)
    extends iexpr {
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

case class BinOp(left: iexpr, op: ioperator, right: iexpr, attributeProvider: AttributeProvider)
    extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class UnaryOp(op: iunaryop, operand: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Lambda(arg: iarguments, body: iexpr, attributeProvider: AttributeProvider) extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class IfExp(test: iexpr, body: iexpr, orelse: iexpr, attributeProvider: AttributeProvider)
    extends iexpr {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Dict(
    keys: CollType[Option[iexpr]],
    values: CollType[iexpr],
    attributeProvider: AttributeProvider
) extends iexpr {
  def this(
      keys: util.ArrayList[iexpr],
      values: util.ArrayList[iexpr],
      attributeProvider: AttributeProvider
  ) = {
    this(
      keys.asScala.map(Option.apply),
      values.asScala,
      attributeProvider
    )
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

case class ListComp(elt: iexpr, generators: CollType[icomprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(
      elt: iexpr,
      generators: util.ArrayList[icomprehension],
      attributeProvider: AttributeProvider
  ) = {
    this(elt, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class SetComp(elt: iexpr, generators: CollType[icomprehension], attributeProvider: AttributeProvider)
    extends iexpr {
  def this(
      elt: iexpr,
      generators: util.ArrayList[icomprehension],
      attributeProvider: AttributeProvider
  ) = {
    this(elt, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class DictComp(
    key: iexpr,
    value: iexpr,
    generators: CollType[icomprehension],
    attributeProvider: AttributeProvider
) extends iexpr {
  def this(
      key: iexpr,
      value: iexpr,
      generators: util.ArrayList[icomprehension],
      attributeProvider: AttributeProvider
  ) = {
    this(key, value, generators.asScala, attributeProvider)
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class GeneratorExp(
    elt: iexpr,
    generators: CollType[icomprehension],
    attributeProvider: AttributeProvider
) extends iexpr {
  def this(
      elt: iexpr,
      generators: util.ArrayList[icomprehension],
      attributeProvider: AttributeProvider
  ) = {
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
    this(
      left,
      ops.asScala,
      comparators.asScala,
      attributeProvider
    )
  }

  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

case class Call(
    func: iexpr,
    args: CollType[iexpr],
    keywords: CollType[ikeyword],
    attributeProvider: AttributeProvider
) extends iexpr {
  def this(
      func: iexpr,
      args: util.ArrayList[iexpr],
      keywords: util.ArrayList[ikeyword],
      attributeProvider: AttributeProvider
  ) = {
    this(
      func,
      args.asScala,
      keywords.asScala,
      attributeProvider
    )
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

case class Slice(
    lower: Option[iexpr],
    upper: Option[iexpr],
    step: Option[iexpr],
    attributeProvider: AttributeProvider
) extends iexpr {
  def this(lower: iexpr, upper: iexpr, step: iexpr, attributeProvider: AttributeProvider) = {
    this(
      Option(lower),
      Option(upper),
      Option(step),
      attributeProvider
    )
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
trait icomprehension extends iast

case class Comprehension(target: iexpr, iter: iexpr, ifs: CollType[iexpr], is_async: Boolean)
    extends icomprehension {
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
sealed trait iexcepthandler extends iast with iattributes

case class ExceptHandler(
    typ: Option[iexpr],
    name: Option[String],
    body: CollType[istmt],
    attributeProvider: AttributeProvider
) extends iexcepthandler {
  def this(
      typ: iexpr,
      name: String,
      body: util.ArrayList[istmt],
      attributeProvider: AttributeProvider
  ) = {
    this(
      Option(typ),
      Option(name),
      body.asScala,
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST arguments classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iarguments extends iast

// Excerpt from the CPython docs:
// posonlyargs, args and kwonlyargs are lists of arg nodes.
// vararg and kwarg are single arg nodes, referring to the *args, **kwargs parameters.
// kw_defaults is a list of default values for keyword-only arguments.
// If one is None, the corresponding argument is required.
// defaults is a list of default values for arguments that can be passed positionally.
// If there are fewer defaults, they correspond to the last n arguments.
case class Arguments(
    posonlyargs: CollType[iarg],
    args: CollType[iarg],
    vararg: Option[iarg],
    kwonlyargs: CollType[iarg],
    kw_defaults: CollType[Option[iexpr]],
    kw_arg: Option[iarg],
    defaults: CollType[iexpr]
) extends iarguments {
  def this(
      posonlyargs: util.List[iarg],
      args: util.List[iarg],
      vararg: iarg,
      kwonlyargs: util.List[iarg],
      kw_defaults: util.List[iexpr],
      kw_arg: iarg,
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
trait iarg extends iast with iattributes

case class Arg(
    arg: String,
    annotation: Option[iexpr],
    type_comment: Option[String],
    attributeProvider: AttributeProvider
) extends iarg {
  def this(
      arg: String,
      annotation: iexpr,
      type_comment: String,
      attributeProvider: AttributeProvider
  ) = {
    this(
      arg,
      Option(annotation),
      Option(type_comment),
      attributeProvider
    )
  }
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST keyword classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait ikeyword extends iast

case class Keyword(arg: Option[String], value: iexpr, attributeProvider: AttributeProvider)
    extends ikeyword
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
sealed trait ialias extends iast

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
trait iwithitem extends iast

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
trait itype_ignore extends iast

case class TypeIgnore(lineno: Int, tag: String) extends itype_ignore {
  override def accept[T](visitor: AstVisitor[T]): T = {
    visitor.visit(this)
  }
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST attributes classes
///////////////////////////////////////////////////////////////////////////////////////////////////
trait iattributes {
  val attributeProvider: AttributeProvider
  def lineno: Int = attributeProvider.lineno
  def col_offset: Int = attributeProvider.col_offset
}

///////////////////////////////////////////////////////////////////////////////////////////////////
// AST constant classes
///////////////////////////////////////////////////////////////////////////////////////////////////
sealed trait iconstant extends iast

case class StringConstant(value: String, isRaw: Boolean, isUnicode: Boolean, isByte: Boolean)
    extends iconstant {
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
