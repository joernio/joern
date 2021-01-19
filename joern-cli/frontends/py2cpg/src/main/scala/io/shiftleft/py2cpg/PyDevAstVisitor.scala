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
  boolopType,
  exprType,
  operatorType,
  unaryopType
}

import scala.collection.mutable

object PyDevAstVisitor {
  private implicit class ToNewNodeConverter(node: AnyRef) {
    def cast: nodes.NewNode = {
      node.asInstanceOf[nodes.NewNode]
    }
  }
}

class PyDevAstVisitor extends VisitorIF with PyDevAstVisitorHelpers {
  import PyDevAstVisitor._

  private val diffGraph = new DiffGraph.Builder()
  protected val nodeBuilder = new NodeBuilder(diffGraph)
  protected val edgeBuilder = new EdgeBuilder(diffGraph)

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
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

  override def visitAssign(assign: Assign): nodes.NewNode = {
    if (assign.targets.size == 1) {
      val target = assign.targets(0)
      val targetWithAccessChains = getTargetsWithAccessChains(target)
      if (targetWithAccessChains.size == 1) {
        // Case with single entity one the left hand side.
        // We always have an empty acces chain in this case.
        val valueNode = assign.value.accept(this).cast
        val targetNode = target.accept(this).cast

        createAssignment(targetNode, valueNode, lineAndColOf(assign))
      } else {
        // Case with a list of entities on the left hand side.
        val valueNode = assign.value.accept(this).cast
        val tmpVariableName = getUnusedName()

        val localNode = nodeBuilder.localNode(tmpVariableName)

        val tmpIdentifierNode =
          nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(assign))
        val tmpVariableAssignNode =
          createAssignment(tmpIdentifierNode, valueNode, lineAndColOf(assign))

        val targetAssignNodes =
          targetWithAccessChains.map { case (target, accessChain) =>
            val targetNode = target.accept(this).cast
            val tmpIdentifierNode =
              nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(assign))
            val indexTmpIdentifierNode = createIndexAccessChain(
              tmpIdentifierNode,
              accessChain,
              lineAndColOf(assign)
            )

            createAssignment(
              targetNode,
              indexTmpIdentifierNode,
              lineAndColOf(assign)
            )
          }

        val blockCode = (codeOf(tmpVariableAssignNode) :: targetAssignNodes.map(codeOf).toList)
          .mkString("\n")
        val blockNode = nodeBuilder.blockNode(blockCode, lineAndColOf(assign))
        var order = 1
        order = addAstChildNodes(blockNode, order, localNode, tmpVariableAssignNode)
        addAstChildNodes(blockNode, order, targetAssignNodes)
        blockNode
      }
    } else {
      throw new RuntimeException("Unexpected assign with more than one target.")
    }
  }

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

  override def visitBoolOp(boolOp: BoolOp): nodes.NewNode = {
    val argNodes = boolOp.values.map(_.accept(this).cast)

    val (operatorCode, methodFullName) =
      boolOp.op match {
        case boolopType.And => (" and ", Operators.logicalAnd)
        case boolopType.Or  => (" or ", Operators.logicalOr)
      }

    val code = argNodes.map(argNode => codeOf(argNode)).mkString(operatorCode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(boolOp)
    )

    addAstChildrenAsArguments(callNode, 1, argNodes)

    callNode
  }

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
        case operatorType.FloorDiv =>
          (" // ", "<operator>.floorDiv") // TODO make this a define and add policy for this
      }

    val code = codeOf(lhsNode) + operatorCode + codeOf(rhsNode)
    val callNode = nodeBuilder.callNode(
      code,
      methodFullName,
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(binOp)
    )

    addAstChildrenAsArguments(callNode, 1, lhsNode, rhsNode)

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
      lineAndColOf(unaryOp)
    )

    addAstChildrenAsArguments(callNode, 1, operandNode)

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
      .callNode("TODO", "TODO", DispatchTypes.DYNAMIC_DISPATCH, lineAndColOf(call))
      .cast

    callNode
  }

  override def visitRepr(repr: Repr): nodes.NewNode = ???

  override def visitNum(num: Num): nodes.NewNode = {
    nodeBuilder.numberLiteralNode(num.num, lineAndColOf(num))
  }

  override def visitStr(str: Str): nodes.NewNode = {
    nodeBuilder.stringLiteralNode(str.s, lineAndColOf(str))
  }

  override def visitStrJoin(strJoin: StrJoin): nodes.NewNode = ???

  override def visitAttribute(attribute: Attribute): nodes.NewNode = ???

  override def visitSubscript(subscript: Subscript): nodes.NewNode = ???

  override def visitStarred(starred: Starred): nodes.NewNode = ???

  override def visitName(name: Name): nodes.NewNode = {
    nodeBuilder.identifierNode(name.id, lineAndColOf(name))
  }

  override def visitList(list: ast.List): nodes.NewNode = ???

  override def visitTuple(tuple: Tuple): nodes.NewNode = ???

  override def visitEllipsis(ellipsis: Ellipsis): nodes.NewNode = ???

  override def visitSlice(slice: Slice): nodes.NewNode = ???

  override def visitExtSlice(extSlice: ExtSlice): nodes.NewNode = ???

  override def visitIndex(index: Index): nodes.NewNode = ???

  override def visitComprehension(comprehension: Comprehension): nodes.NewNode = ???
}
