package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, nodes}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.pythonparser.AstVisitor
import io.shiftleft.pythonparser.ast
import io.shiftleft.pythonparser.ast.{Add, Alias, And, AnnAssign, Arg, Arguments, Assert, AsyncFor, AsyncFunctionDef, AsyncWith, AugAssign, Await, BitAnd, BitOr, BitXor, BoolConstant, ClassDef, Compare, Comprehension, Delete, Dict, DictComp, Div, EllipsisConstant, Eq, ErrorStatement, ExceptHandler, FloatConstant, FloorDiv, For, FunctionDef, GeneratorExp, Global, Gt, GtE, IfExp, ImaginaryConstant, Import, ImportFrom, In, IntConstant, Invert, Is, IsNot, Keyword, LShift, Lambda, ListComp, Lt, LtE, MatMult, Mod, Mult, NamedExpr, NoneConstant, Nonlocal, Not, NotEq, NotIn, Or, Pow, RShift, Raise, Return, SetComp, Slice, Starred, StringConstant, StringExpList, Sub, Subscript, Try, Tuple, TypeIgnore, UAdd, USub, With, WithItem, Yield, YieldFrom, ialias, iarg, iarguments, iboolop, icompop, icomprehension, iconstant, iexcepthandler, iexpr, ikeyword, imod, ioperator, istmt, itype_ignore, iunaryop, iwithitem}

import scala.collection.mutable

class PythonAstVisitor extends AstVisitor[nodes.NewNode] with PythonAstVisitorHelpers {

  private val diffGraph = new DiffGraph.Builder()
  protected val nodeBuilder = new NodeBuilder(diffGraph)
  protected val edgeBuilder = new EdgeBuilder(diffGraph)

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
  }

  override def visit(astNode: ast.iast): nodes.NewNode = ???

  override def visit(mod: imod): NewNode = ???

  override def visit(module: ast.Module): nodes.NewNode = {
    module.stmts.foreach(_.accept(this))
    null
  }

  override def visit(stmt: istmt): NewNode = ???

  override def visit(errorStatement: ErrorStatement): NewNode = ???

  override def visit(functionDef: FunctionDef): NewNode = ???

  override def visit(functionDef: AsyncFunctionDef): NewNode = ???

  override def visit(classDef: ClassDef): NewNode = ???

  override def visit(ret: Return): NewNode = ???

  override def visit(delete: Delete): NewNode = ???

  override def visit(assign: ast.Assign): nodes.NewNode = {
    if (assign.targets.size == 1) {
      val target = assign.targets.head
      val targetWithAccessChains = getTargetsWithAccessChains(target)
      if (targetWithAccessChains.size == 1) {
        // Case with single entity one the left hand side.
        // We always have an empty acces chain in this case.
        val valueNode = assign.value.accept(this)
        val targetNode = target.accept(this)

        createAssignment(targetNode, valueNode, lineAndColOf(assign))
      } else {
        // Case with a list of entities on the left hand side.
        val valueNode = assign.value.accept(this)
        val tmpVariableName = getUnusedName()

        val localNode = nodeBuilder.localNode(tmpVariableName)

        val tmpIdentifierNode =
          nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(assign))
        val tmpVariableAssignNode =
          createAssignment(tmpIdentifierNode, valueNode, lineAndColOf(assign))

        val targetAssignNodes =
          targetWithAccessChains.map { case (target, accessChain) =>
            val targetNode = target.accept(this)
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

        val blockNode =
          createBlock(
            Iterable.single(localNode),
            tmpVariableAssignNode :: targetAssignNodes.toList,
            lineAndColOf(assign)
          )

        blockNode
      }
    } else {
      throw new RuntimeException("Unexpected assign with more than one target.")
    }
  }

  override def visit(annAssign: AnnAssign): NewNode = ???

  override def visit(augAssign: AugAssign): NewNode = ???

  override def visit(forStmt: For): NewNode = ???

  override def visit(forStmt: AsyncFor): NewNode = ???

  override def visit(astWhile: ast.While): nodes.NewNode = {
    val conditionNode = astWhile.test.accept(this)
    val bodyStmtNodes = astWhile.body.map(_.accept(this))
    val elseStmtNodes = astWhile.orelse.map(_.accept(this))

    val bodyBlockNode = createBlock(Iterable.empty, bodyStmtNodes, lineAndColOf(astWhile))
    val elseBlockNode = createBlock(Iterable.empty, elseStmtNodes, lineAndColOf(astWhile.orelse.head))

    val controlStructureNode =
      nodeBuilder.controlStructureNode("while ... : ...", "WhileStatement", lineAndColOf(astWhile))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode, elseBlockNode)

    controlStructureNode
  }

  override def visit(astIf: ast.If): nodes.NewNode = {
    val conditionNode = astIf.test.accept(this)
    val bodyStmtNodes = astIf.body.map(_.accept(this))
    val elseStmtNodes = astIf.orelse.map(_.accept(this))

    val bodyBlockNode = createBlock(Iterable.empty, bodyStmtNodes, lineAndColOf(astIf))
    val elseBlockNode = createBlock(Iterable.empty, elseStmtNodes, lineAndColOf(astIf.orelse.head))

    val controlStructureNode =
      nodeBuilder.controlStructureNode("if ... : ...", "IfStatement", lineAndColOf(astIf))
    edgeBuilder.conditionEdge(conditionNode, controlStructureNode)
    addAstChildNodes(controlStructureNode, 1, conditionNode, bodyBlockNode, elseBlockNode)

    controlStructureNode
  }

  override def visit(withStmt: With): NewNode = ???

  override def visit(withStmt: AsyncWith): NewNode = ???

  override def visit(raise: Raise): NewNode = ???

  override def visit(tryStmt: Try): NewNode = ???

  override def visit(assert: Assert): NewNode = ???

  override def visit(importStmt: Import): NewNode = ???

  override def visit(importFrom: ImportFrom): NewNode = ???

  override def visit(global: Global): NewNode = ???

  override def visit(nonlocal: Nonlocal): NewNode = ???

  override def visit(expr: ast.Expr): nodes.NewNode = {
    expr.value.accept(this)
  }

  override def visit(pass: ast.Pass): nodes.NewNode = {
    nodeBuilder.callNode(
      "pass",
      "<operator>.pass",
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(pass)
    )
  }

  override def visit(astBreak: ast.Break): nodes.NewNode = {
    nodeBuilder.controlStructureNode("break", "BreakStatement", lineAndColOf(astBreak))
  }

  override def visit(astContinue: ast.Continue): nodes.NewNode = {
    nodeBuilder.controlStructureNode("continue", "ContinueStatement", lineAndColOf(astContinue))
  }

  override def visit(expr: iexpr): NewNode = ???

  override def visit(boolOp: ast.BoolOp): nodes.NewNode = {
    val argNodes = boolOp.values.map(_.accept(this))

    val (operatorCode, methodFullName) =
      boolOp.op match {
        case ast.And => (" and ", Operators.logicalAnd)
        case ast.Or  => (" or ", Operators.logicalOr)
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

  override def visit(namedExpr: NamedExpr): NewNode = ???

  override def visit(binOp: ast.BinOp): nodes.NewNode = {
    val lhsNode = binOp.left.accept(this)
    val rhsNode = binOp.right.accept(this)

    val (operatorCode, methodFullName) =
      binOp.op match {
        case ast.Add    => (" + ", Operators.addition)
        case ast.Sub    => (" - ", Operators.subtraction)
        case ast.Mult   => (" * ", Operators.multiplication)
        case ast.MatMult =>
          (" @ ", "<operator>.matMult") // TODO make this a define and add policy for this
        case ast.Div    => (" / ", Operators.division)
        case ast.Mod    => (" % ", Operators.modulo)
        case ast.Pow    => (" ** ", Operators.exponentiation)
        case ast.LShift => (" << ", Operators.shiftLeft)
        case ast.RShift => (" << ", Operators.arithmeticShiftRight)
        case ast.BitOr  => (" | ", Operators.or)
        case ast.BitXor => (" ^ ", Operators.xor)
        case ast.BitAnd => (" & ", Operators.and)
        case ast.FloorDiv =>
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

  override def visit(unaryOp: ast.UnaryOp): nodes.NewNode = {
    val operandNode = unaryOp.operand.accept(this)

    val (operatorCode, methodFullName) =
      unaryOp.op match {
        case ast.Invert => ("~", Operators.not)
        case ast.Not    => ("not ", Operators.logicalNot)
        case ast.UAdd   => ("+", Operators.plus)
        case ast.USub   => ("-", Operators.minus)
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

  override def visit(lambda: Lambda): NewNode = ???

  override def visit(ifExp: IfExp): NewNode = ???

  override def visit(dict: Dict): NewNode = ???

  override def visit(set: ast.Set): NewNode = ???

  override def visit(listComp: ListComp): NewNode = ???

  override def visit(setComp: SetComp): NewNode = ???

  override def visit(dictComp: DictComp): NewNode = ???

  override def visit(generatorExp: GeneratorExp): NewNode = ???

  override def visit(await: Await): NewNode = ???

  override def visit(yieldExpr: Yield): NewNode = ???

  override def visit(yieldFrom: YieldFrom): NewNode = ???

  override def visit(compare: Compare): NewNode = ???

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
  override def visit(call: ast.Call): nodes.NewNode = {
    val argumentNodes = call.args.map(_.accept(this)).toSeq
    val receiverNode = call.func.accept(this)

    call.func match {
      case attribute: ast.Attribute =>
        val instanceNode = attribute.value.accept(this)
        createInstanceCall(receiverNode, instanceNode, lineAndColOf(call), argumentNodes: _*)
      case _ =>
        createCall(receiverNode, lineAndColOf(call), argumentNodes: _*)
    }
  }

  override def visit(constant: ast.Constant): nodes.NewNode = {
    constant.value match {
      case intConstant: ast.IntConstant =>
        nodeBuilder.numberLiteralNode(intConstant.value, lineAndColOf(constant))
      case stringConstant: ast.StringConstant =>
        nodeBuilder.stringLiteralNode(stringConstant.value, lineAndColOf(constant))

    }
  }

  /** TODO
    * We currently ignore possible attribute access provider/interception
    * mechanisms like __getattr__, __getattribute__ and __get__.
    */
  override def visit(attribute: ast.Attribute): nodes.NewNode = {
    val baseNode = attribute.value.accept(this)
    val fieldIdNode = nodeBuilder.fieldIdentifierNode(attribute.attr, lineAndColOf(attribute))

    createFieldAccess(baseNode, fieldIdNode, lineAndColOf(attribute))
  }

  override def visit(subscript: Subscript): NewNode = ???

  override def visit(starred: Starred): NewNode = ???

  override def visit(name: ast.Name): nodes.NewNode = {
    nodeBuilder.identifierNode(name.id, lineAndColOf(name))
  }

  /**
    * Lowering of [1, 2]:
    *   {
    *     tmp = list
    *     tmp.append(1)
    *     tmp.append(2)
    *     tmp
    *   }
    */
  override def visit(list: ast.List): nodes.NewNode = {
    val tmpVariableName = getUnusedName()
    val localNode = nodeBuilder.localNode(tmpVariableName)

    val listInstanceId = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
    val listIdNode = nodeBuilder.identifierNode("list", lineAndColOf(list))
    val listConstructorCall = createCall(listIdNode, lineAndColOf(list))
    val listInstanceAssignment = createAssignment(listInstanceId, listConstructorCall, lineAndColOf(list))

    val appendCallNodes = list.elts.map { listElement =>
      val listInstanceIdForReceiver = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
      val appendFieldAccessNode = createFieldAccess(listInstanceIdForReceiver, "append", lineAndColOf(list))

      val listeInstanceId = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))
      val elementNode = listElement.accept(this)
      createInstanceCall(appendFieldAccessNode, listeInstanceId, lineAndColOf(list), elementNode)
    }

    val listInstanceIdForReturn = nodeBuilder.identifierNode(tmpVariableName, lineAndColOf(list))

    val blockElements = mutable.ArrayBuffer.empty[nodes.NewNode]
    blockElements.append(listInstanceAssignment)
    blockElements.appendAll(appendCallNodes)
    blockElements.append(listInstanceIdForReturn)
    createBlock(Iterable.single(localNode), blockElements, lineAndColOf(list))
  }

  override def visit(tuple: Tuple): NewNode = ???

  override def visit(slice: Slice): NewNode = ???

  override def visit(stringExpList: StringExpList): NewNode = ???

  override def visit(boolop: iboolop): NewNode = ???

  override def visit(and: And.type): NewNode = ???

  override def visit(or: Or.type): NewNode = ???

  override def visit(operator: ioperator): NewNode = ???

  override def visit(add: Add.type): NewNode = ???

  override def visit(sub: Sub.type): NewNode = ???

  override def visit(mult: Mult.type): NewNode = ???

  override def visit(matMult: MatMult.type): NewNode = ???

  override def visit(div: Div.type): NewNode = ???

  override def visit(mod: Mod.type): NewNode = ???

  override def visit(pow: Pow.type): NewNode = ???

  override def visit(lShift: LShift.type): NewNode = ???

  override def visit(rShift: RShift.type): NewNode = ???

  override def visit(bitOr: BitOr.type): NewNode = ???

  override def visit(bitXor: BitXor.type): NewNode = ???

  override def visit(bitAnd: BitAnd.type): NewNode = ???

  override def visit(floorDiv: FloorDiv.type): NewNode = ???

  override def visit(unaryop: iunaryop): NewNode = ???

  override def visit(invert: Invert.type): NewNode = ???

  override def visit(not: Not.type): NewNode = ???

  override def visit(uAdd: UAdd.type): NewNode = ???

  override def visit(uSub: USub.type): NewNode = ???

  override def visit(compop: icompop): NewNode = ???

  override def visit(eq: Eq.type): NewNode = ???

  override def visit(notEq: NotEq.type): NewNode = ???

  override def visit(lt: Lt.type): NewNode = ???

  override def visit(ltE: LtE.type): NewNode = ???

  override def visit(gt: Gt.type): NewNode = ???

  override def visit(gtE: GtE.type): NewNode = ???

  override def visit(is: Is.type): NewNode = ???

  override def visit(isNot: IsNot.type): NewNode = ???

  override def visit(in: In.type): NewNode = ???

  override def visit(notIn: NotIn.type): NewNode = ???

  override def visit(comprehension: icomprehension): NewNode = ???

  override def visit(comprehension: Comprehension): NewNode = ???

  override def visit(exceptHandler: iexcepthandler): NewNode = ???

  override def visit(exceptHandler: ExceptHandler): NewNode = ???

  override def visit(arguments: iarguments): NewNode = ???

  override def visit(arguments: Arguments): NewNode = ???

  override def visit(arg: iarg): NewNode = ???

  override def visit(arg: Arg): NewNode = ???

  override def visit(constant: iconstant): NewNode = ???

  override def visit(stringConstant: StringConstant): NewNode = ???

  override def visit(boolConstant: BoolConstant): NewNode = ???

  override def visit(intConstant: IntConstant): NewNode = ???

  override def visit(intConstant: FloatConstant): NewNode = ???

  override def visit(imaginaryConstant: ImaginaryConstant): NewNode = ???

  override def visit(noneConstant: NoneConstant.type): NewNode = ???

  override def visit(ellipsisConstant: EllipsisConstant.type): NewNode = ???

  override def visit(keyword: ikeyword): NewNode = ???

  override def visit(keyword: Keyword): NewNode = ???

  override def visit(alias: ialias): NewNode = ???

  override def visit(alias: Alias): NewNode = ???

  override def visit(withItem: iwithitem): NewNode = ???

  override def visit(withItem: WithItem): NewNode = ???

  override def visit(typeIgnore: itype_ignore): NewNode = ???

  override def visit(typeIgnore: TypeIgnore): NewNode = ???
}
