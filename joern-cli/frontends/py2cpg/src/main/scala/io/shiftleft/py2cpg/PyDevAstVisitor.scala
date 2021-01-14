package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes
import io.shiftleft.passes.DiffGraph
import org.python.pydev.parser.jython.ast
import org.python.pydev.parser.jython.ast.{
  Assert,
  Assign,
  Attribute,
  AugAssign,
  Await,
  BinOp,
  BoolOp,
  Break,
  Call,
  ClassDef,
  Compare,
  Comprehension,
  Continue,
  Delete,
  Dict,
  DictComp,
  Ellipsis,
  Exec,
  Expr,
  Expression,
  ExtSlice,
  For,
  FunctionDef,
  GeneratorExp,
  Global,
  If,
  IfExp,
  Import,
  ImportFrom,
  Index,
  Interactive,
  Lambda,
  ListComp,
  Name,
  NameTok,
  NamedExpr,
  NonLocal,
  Num,
  Pass,
  Print,
  Raise,
  Repr,
  Return,
  SetComp,
  Slice,
  Starred,
  Str,
  StrJoin,
  Subscript,
  Suite,
  TryExcept,
  TryFinally,
  Tuple,
  UnaryOp,
  VisitorIF,
  While,
  With,
  WithItem,
  Yield
}

class PyDevAstVisitor extends VisitorIF {
  private val diffGraph = new DiffGraph.Builder()

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
  }

  override def visitModule(module: ast.Module): nodes.NewNode = {}

  override def visitInteractive(interactive: Interactive): nodes.NewNode = ???

  override def visitExpression(expression: Expression): nodes.NewNode = ???

  override def visitNameTok(nameTok: NameTok): nodes.NewNode = ???

  override def visitSuite(suite: Suite): nodes.NewNode = ???

  override def visitWithItem(withItem: WithItem): nodes.NewNode = ???

  override def visitFunctionDef(functionDef: FunctionDef): nodes.NewNode = ???

  override def visitClassDef(classDef: ClassDef): nodes.NewNode = ???

  override def visitReturn(aReturn: Return): nodes.NewNode = ???

  override def visitDelete(delete: Delete): nodes.NewNode = ???

  override def visitAssign(assign: Assign): nodes.NewNode = ???

  override def visitAugAssign(augAssign: AugAssign): nodes.NewNode = ???

  override def visitPrint(print: Print): nodes.NewNode = ???

  override def visitFor(aFor: For): nodes.NewNode = ???

  override def visitWhile(aWhile: While): nodes.NewNode = ???

  override def visitIf(anIf: If): nodes.NewNode = ???

  override def visitWith(`with`: With): nodes.NewNode = ???

  override def visitRaise(raise: Raise): nodes.NewNode = ???

  override def visitTryExcept(tryExcept: TryExcept): nodes.NewNode = ???

  override def visitTryFinally(tryFinally: TryFinally): nodes.NewNode = ???

  override def visitAssert(anAssert: Assert): nodes.NewNode = ???

  override def visitImport(anImport: Import): nodes.NewNode = ???

  override def visitImportFrom(importFrom: ImportFrom): nodes.NewNode = ???

  override def visitExec(exec: Exec): nodes.NewNode = ???

  override def visitGlobal(global: Global): nodes.NewNode = ???

  override def visitNonLocal(nonLocal: NonLocal): nodes.NewNode = ???

  override def visitExpr(expr: Expr): nodes.NewNode = ???

  override def visitPass(pass: Pass): nodes.NewNode = ???

  override def visitBreak(aBreak: Break): nodes.NewNode = ???

  override def visitContinue(aContinue: Continue): nodes.NewNode = ???

  override def visitBoolOp(boolOp: BoolOp): nodes.NewNode = ???

  override def visitNamedExpr(namedExpr: NamedExpr): nodes.NewNode = ???

  override def visitBinOp(binOp: BinOp): nodes.NewNode = ???

  override def visitUnaryOp(unaryOp: UnaryOp): nodes.NewNode = ???

  override def visitLambda(lambda: Lambda): nodes.NewNode = ???

  override def visitIfExp(ifExp: IfExp): nodes.NewNode = ???

  override def visitDict(dict: Dict): nodes.NewNode = ???

  override def visitSet(set: ast.Set): nodes.NewNode = ???

  override def visitListComp(listComp: ListComp): nodes.NewNode = ???

  override def visitSetComp(setComp: SetComp): nodes.NewNode = ???

  override def visitDictComp(dictComp: DictComp): nodes.NewNode = ???

  override def visitGeneratorExp(generatorExp: GeneratorExp): nodes.NewNode = ???

  override def visitYield(`yield`: Yield): nodes.NewNode = ???

  override def visitAwait(await: Await): nodes.NewNode = ???

  override def visitCompare(compare: Compare): nodes.NewNode = ???

  override def visitCall(call: Call): nodes.NewNode = ???

  override def visitRepr(repr: Repr): nodes.NewNode = ???

  override def visitNum(num: Num): nodes.NewNode = ???

  override def visitStr(str: Str): nodes.NewNode = ???

  override def visitStrJoin(strJoin: StrJoin): nodes.NewNode = ???

  override def visitAttribute(attribute: Attribute): nodes.NewNode = ???

  override def visitSubscript(subscript: Subscript): nodes.NewNode = ???

  override def visitStarred(starred: Starred): nodes.NewNode = ???

  override def visitName(name: Name): nodes.NewNode = ???

  override def visitList(list: ast.List): nodes.NewNode = ???

  override def visitTuple(tuple: Tuple): nodes.NewNode = ???

  override def visitEllipsis(ellipsis: Ellipsis): nodes.NewNode = ???

  override def visitSlice(slice: Slice): nodes.NewNode = ???

  override def visitExtSlice(extSlice: ExtSlice): nodes.NewNode = ???

  override def visitIndex(index: Index): nodes.NewNode = ???

  override def visitComprehension(comprehension: Comprehension): nodes.NewNode = ???
}
