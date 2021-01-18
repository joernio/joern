package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
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
  Yield,
  operatorType,
  unaryopType
}

object PyDevAstVisitor {
  private implicit class ToNewNodeConverter(node: AnyRef) {
    def cast: nodes.NewNode = {
      node.asInstanceOf[nodes.NewNode]
    }
  }
}

class PyDevAstVisitor extends VisitorIF {
  import PyDevAstVisitor._

  private val diffGraph = new DiffGraph.Builder()
  private val nodeBuilder = new NodeBuilder(diffGraph)
  private val edgeBuilder = new EdgeBuilder(diffGraph)

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
  }

  private def codeOf(node: nodes.NewNode): String = {
    node.asInstanceOf[nodes.HasCode].code
  }

  override def visitModule(module: ast.Module): nodes.NewNode = {
    module.traverse(this)
    null
  }

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

  override def visitExpr(expr: Expr): nodes.NewNode = {
    expr.value.accept(this).cast
  }

  override def visitPass(pass: Pass): nodes.NewNode = ???

  override def visitBreak(aBreak: Break): nodes.NewNode = ???

  override def visitContinue(aContinue: Continue): nodes.NewNode = ???

  override def visitBoolOp(boolOp: BoolOp): nodes.NewNode = ???

  override def visitNamedExpr(namedExpr: NamedExpr): nodes.NewNode = ???

  override def visitBinOp(binOp: BinOp): nodes.NewNode = {
    val lhsNode = binOp.left.accept(this).cast
    val rhsNode = binOp.right.accept(this).cast

    val (operatorCode, methodFullName) =
      binOp.op match {
        case operatorType.Add    => (" + ", Operators.addition)
        case operatorType.Sub    => (" - ", Operators.subtraction)
        case operatorType.Mult   => (" * ", Operators.multiplication)
        case operatorType.Div    => (" / ", Operators.division)
        case operatorType.Mod    => (" % ", Operators.modulo)
        case operatorType.Pow    => (" ** ", Operators.exponentiation)
        case operatorType.LShift => (" << ", Operators.shiftLeft)
        case operatorType.RShift => (" << ", Operators.arithmeticShiftRight)
        case operatorType.BitOr  => (" | ", Operators.or)
        case operatorType.BitXor => (" ^ ", Operators.xor)
        case operatorType.BitAnd => (" & ", Operators.and)
        case operatorType.BitAnd =>
          (" // ", "<operator>.floorDiv") // TODO make this a define and add policy for this
      }

    val code = codeOf(lhsNode) + operatorCode + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      binOp.beginLine,
      binOp.beginColumn
    )

    edgeBuilder.astEdge(lhsNode, callNode, 1)
    edgeBuilder.argumentEdge(lhsNode, callNode, 1)
    edgeBuilder.astEdge(rhsNode, callNode, 2)
    edgeBuilder.argumentEdge(rhsNode, callNode, 2)

    callNode
  }

  override def visitUnaryOp(unaryOp: UnaryOp): nodes.NewNode = {
    val operandNode = unaryOp.operand.accept(this).cast

    val (operatorCode, methodFullName) =
      unaryOp.op match {
        case unaryopType.Invert => ("~", Operators.not)
        case unaryopType.Not    => ("not ", Operators.logicalNot)
        case unaryopType.UAdd   => ("+", Operators.plus)
        case unaryopType.USub   => ("-", Operators.minus)
      }

    val code = operatorCode + codeOf(operandNode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      unaryOp.beginLine,
      unaryOp.beginColumn
    )

    edgeBuilder.astEdge(operandNode, callNode, 1)
    edgeBuilder.argumentEdge(operandNode, callNode, 1)

    callNode
  }

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

  override def visitCall(call: Call): nodes.NewNode = {
    val receiverNode = call.func.accept(this).cast

    val argumentNodes = call.args.map(_.accept(this).cast)

    val callNode = nodeBuilder
      .callNode("TODO", "TODO", DispatchTypes.DYNAMIC_DISPATCH, call.beginLine, call.beginColumn)
      .cast

    callNode
  }

  override def visitRepr(repr: Repr): nodes.NewNode = ???

  override def visitNum(num: Num): nodes.NewNode = {
    nodeBuilder.literalNode(num.num, num.beginLine, num.beginColumn)
  }

  override def visitStr(str: Str): nodes.NewNode = {
    nodeBuilder.literalNode(str.s, str.beginLine, str.beginColumn)
  }

  override def visitStrJoin(strJoin: StrJoin): nodes.NewNode = ???

  override def visitAttribute(attribute: Attribute): nodes.NewNode = ???

  override def visitSubscript(subscript: Subscript): nodes.NewNode = ???

  override def visitStarred(starred: Starred): nodes.NewNode = ???

  override def visitName(name: Name): nodes.NewNode = {
    nodeBuilder.identifierNode(name.id, name.beginLine, name.beginColumn)
  }

  override def visitList(list: ast.List): nodes.NewNode = ???

  override def visitTuple(tuple: Tuple): nodes.NewNode = ???

  override def visitEllipsis(ellipsis: Ellipsis): nodes.NewNode = ???

  override def visitSlice(slice: Slice): nodes.NewNode = ???

  override def visitExtSlice(extSlice: ExtSlice): nodes.NewNode = ???

  override def visitIndex(index: Index): nodes.NewNode = ???

  override def visitComprehension(comprehension: Comprehension): nodes.NewNode = ???
}
