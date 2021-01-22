package io.shiftleft.py2cpg.pyast

import io.bullet.borer.derivation.key

object PyAst {
  type CollType[T] = Iterable[T]
  private val empty = Nil

  trait attributes {
    val lineno: Int
    val col_offset: Int
  }

  sealed trait imod
  case class Module(body: CollType[istmt] = empty , type_ignores: CollType[itype_ignore] = empty) extends imod
  case class Interactive(body: CollType[istmt] = empty) extends imod
  case class Expression(body: iexpr) extends imod
  case class FunctionType(argtypes: CollType[iexpr] = empty, returns: iexpr) extends imod

  sealed trait istmt extends attributes
  case class FunctionDef(name: String, args: iarguments, body: CollType[istmt] = empty, decorator_list: CollType[iexpr] = empty, returns: Option[iexpr] = None, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class AsyncFunctionDef(name: String, args: iarguments, body: CollType[istmt] = empty, decorator_list: CollType[iexpr] = empty, returns: Option[iexpr] = None, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class ClassDef(name: String, bases: CollType[iexpr] = empty, keywords: CollType[ikeyword] = empty, body: CollType[istmt] = empty, decorator_list: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Return(value: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Delete(targets: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Assign(targets: CollType[iexpr] = empty, value: iexpr, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class AugAssign(target: iexpr, op: ioperator, value: iexpr, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class AnnAssign(target: iexpr, annotation: iexpr, value: Option[iexpr] = None, simple: Int, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class For(target: iexpr, iter: iexpr, body: CollType[istmt] = empty, orelse: CollType[istmt] = empty, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class AsyncFor(target: iexpr, iter: iexpr, body: CollType[istmt] = empty, orelse: CollType[istmt] = empty, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class While(test: iexpr, body: CollType[istmt] = empty, orelse: CollType[istmt] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class If(test: iexpr, body: CollType[istmt] = empty, orelse: CollType[istmt] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class With(itmes: CollType[iwithitem] = empty, body: CollType[istmt] = empty, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class AsyncWith(itmes: CollType[iwithitem] = empty, body: CollType[istmt] = empty, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Raise(exc: Option[iexpr] = None, cause: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Try(body: CollType[istmt] = empty, handlers: CollType[iexcepthandler] = empty, orelse: CollType[istmt] = empty, finalbody: CollType[istmt] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Assert(test: iexpr, msg: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Import(names: CollType[ialias] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class ImportFrom(module: Option[String] = None, names: CollType[ialias] = empty, level: Option[Int] = None, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Global(names: CollType[String] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Nonlocal(names: CollType[String] = empty, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Expr(value: iexpr, lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Pass(lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Break(lineno: Int = 0, col_offset: Int = 0) extends istmt
  case class Continue(lineno: Int = 0, col_offset: Int = 0) extends istmt

  sealed trait iexpr extends attributes
  case class BoolOp(op: iboolop, values: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class NamedExpr(target: iexpr, value: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class BinOp(left: iexpr, op: ioperator, right: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class UnaryOp(op: iunaryop, operand: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Lambda(args: iarguments, body: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class IfExp(test: iexpr, body: iexpr, orelse: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Dict(keys: CollType[iexpr] = empty, values: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Set(elts: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class ListComp(elt: iexpr, generators: CollType[icomprehension] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class SetComp(elt: iexpr, generators: CollType[icomprehension] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class DictComp(key: iexpr, value: iexpr, generators: CollType[icomprehension] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class GeneratorExp(elt: iexpr, generators: CollType[icomprehension] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Await(value: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Yield(value: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class YieldFrom(value: iexpr, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Compare(left: iexpr, ops: CollType[icmpop] = empty, comparators: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Call(func: iexpr, args: CollType[iexpr] = empty, keywords: CollType[ikeyword] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class FormattedValue(value: iexpr, conversion: Option[Int] = None, format_spec: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class JoinedStr(values: CollType[iexpr] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Constant(value: iconstant, kind: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Attribute(value: iexpr, attr: String, ctx: iexpr_context, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Subscript(value: iexpr, slice: iexpr, ctx: iexpr_context, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Starred(value: iexpr, ctx: iexpr_context, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Name(id: String, ctx: iexpr_context, lineno:Int = 0, col_offset: Int = 0) extends iexpr
  case class List(elts: CollType[iexpr] = empty, ctx: iexpr_context, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Tuple(elts: CollType[iexpr] = empty, ctx: iexpr_context, lineno: Int = 0, col_offset: Int = 0) extends iexpr
  case class Slice(lower: Option[iexpr] = None, upper: Option[iexpr] = None, step: Option[iexpr] = None, lineno: Int = 0, col_offset: Int = 0) extends iexpr

  sealed trait iexpr_context
  case object Load extends iexpr_context
  case object Store extends iexpr_context
  case object Del extends iexpr_context

  sealed trait iboolop
  case object And extends iboolop
  case object Or extends iboolop

  sealed trait ioperator
  case object Add extends ioperator
  case object Sub extends ioperator
  case object Mult extends ioperator
  case object MatMult extends ioperator
  case object Div extends ioperator
  case object Mod extends ioperator
  case object Pow extends ioperator
  case object LShift extends ioperator
  case object RShift extends ioperator
  case object BitOr extends ioperator
  case object BitXOr extends ioperator
  case object BitAnd extends ioperator
  case object FloorDiv extends ioperator

  sealed trait iunaryop
  case object Invert extends iunaryop
  case object Not extends iunaryop
  case object UAdd extends iunaryop
  case object USub extends iunaryop

  sealed trait icmpop
  case object Eq extends icmpop
  case object NotEq extends icmpop
  case object Lt extends icmpop
  case object LtE extends icmpop
  case object Gt extends icmpop
  case object GtE extends icmpop
  case object Is extends icmpop
  case object IsNot extends icmpop
  case object In extends icmpop
  case object NotIn extends icmpop

  sealed trait icomprehension
  @key("comprehension")
  case class Comprehension(target: iexpr, iter: iexpr, ifs: CollType[iexpr] = empty, is_async: Int) extends icomprehension

  sealed trait iexcepthandler extends attributes
  case class ExceptHandler(`type`: Option[iexpr] = None, name: Option[String] = None, body: CollType[istmt] = empty, lineno: Int = 0, col_offset: Int = 0) extends iexcepthandler

  sealed trait iarguments
  @key("arguments")
  case class Arguments(posonlyargs: CollType[iarg] = empty, args: CollType[iarg] = empty, varary: Option[iarg] = None, kwonlyargs: CollType[iarg] = empty, kw_default: CollType[iexpr] = empty, kwarg: Option[iarg] = None, defaults: CollType[iexpr] = empty) extends iarguments

  sealed trait iarg
  @key("arg")
  case class Arg(arg: String, annotation: Option[iexpr] = None, type_comment: Option[String] = None, lineno: Int = 0, col_offset: Int = 0) extends iarg

  sealed trait ikeyword
  @key("keyword")
  case class Keyword(arg: String, value: iexpr) extends ikeyword

  sealed trait ialias
  @key("alias")
  case class Alias(name: String, asname: Option[String] = None) extends ialias

  sealed trait iwithitem
  @key("withitem")
  case class Withitem(context_expr: iexpr, optional_vars: Option[iexpr] = None) extends iwithitem

  sealed trait itype_ignore
  case class TypeIgnore(lineno: Int, tag: String) extends itype_ignore

  sealed trait iconstant
  case class IntConstant(value: Int) extends iconstant
  case class StringConstant(value: String) extends iconstant
  case class BytesConstant(value: String) extends iconstant
  case class BoolConstant(value: Boolean) extends iconstant
  case object NoneConstant extends iconstant
}
