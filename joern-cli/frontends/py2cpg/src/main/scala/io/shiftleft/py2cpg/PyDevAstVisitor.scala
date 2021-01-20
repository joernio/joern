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

  override def visitNameTok(nameTok: NameTok): nodes.NewNode = {
    nodeBuilder.fieldIdentifierNode(nameTok.id, lineAndColOf(nameTok))
  }

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

  /** TODO
    * For now this function compromises on the correctness of the
    * lowering in order to get some data flow tracking going.
    * 1. For constructs like x.func() we assume x to be the
    *    instance which is passed into func. This is not true
    *    since the instance method object gets the instance
    *    already bound/captured during function access.
    *    This becomes relevant for constructs like:
    *    x.func = y.func <- y.func is class method object
    *    x.func()
    *    In this case the instance passed into func is y and
    *    not x. We cannot represent this in th CPG and thus
    *    stick to the assumption that the part before the "."
    *    and the bound/captured instance will be the same.
    *    For reference see:
    *    https://docs.python.org/3/reference/datamodel.html#the-standard-type-hierarchy
    *    search for "Instance methods"
    *
    * 2. Due to the decision in 1. for calls like x.func() the
    *    expression x is part of the call receiver AST and its
    *    instance AST. This would be only ok if x is side effect
    *    free which is not necessarily the case if x == getX().
    *    Currently we ignore this fact and just emit the expression
    *    twice. A fix would mean to emit a tmp variable which holds
    *    the expression result.
    *    Not yet implemented because this gets obsolete if 1. is
    *    fixed.
    * 3. No named parameter support. CPG does not supports this.
    */
  override def visitCall(call: Call): nodes.NewNode = {
    val argumentNodes = call.args.map(_.accept(this).cast)
    val receiverNode = call.func.accept(this).cast

    val code = codeOf(receiverNode) + "(" + argumentNodes.map(codeOf).mkString(", ") + ")"
    val callNode = nodeBuilder
      .callNode(code, "", DispatchTypes.DYNAMIC_DISPATCH, lineAndColOf(call))
      .cast

    var orderIndex = 0
    edgeBuilder.receiverEdge(receiverNode, callNode)
    edgeBuilder.astEdge(receiverNode, callNode, orderIndex)
    orderIndex += 1

    call.func match {
      case attribute: Attribute =>
        val instanceNode = attribute.value.accept(this).cast
        edgeBuilder.astEdge(instanceNode, callNode, orderIndex)
        edgeBuilder.argumentEdge(instanceNode, callNode, 0)
        orderIndex += 1
      case _ =>
    }

    var argIndex = 1
    argumentNodes.foreach { argumentNode =>
      edgeBuilder.astEdge(argumentNode, callNode, orderIndex)
      edgeBuilder.argumentEdge(argumentNode, callNode, argIndex)
      orderIndex += 1
      argIndex += 1
    }

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

  /** TODO
    * We currently ignore possible attribute access provider/interception
    * mechanisms like __getattr__, __getattribute__ and __get__.
    */
  override def visitAttribute(attribute: Attribute): nodes.NewNode = {
    val baseNode = attribute.value.accept(this).cast
    val fieldIdNode = attribute.attr.accept(this).cast

    createFieldAccess(baseNode, fieldIdNode, lineAndColOf(attribute))
  }

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
