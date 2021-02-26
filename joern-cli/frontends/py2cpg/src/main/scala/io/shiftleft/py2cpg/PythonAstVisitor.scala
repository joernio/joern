package io.shiftleft.py2cpg

import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, ModifierTypes, Operators, nodes}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.pythonparser.AstVisitor
import io.shiftleft.pythonparser.ast
import io.shiftleft.semanticcpg.language.toMethodForCallGraph

import scala.collection.mutable

class PythonAstVisitor(fileName: String) extends AstVisitor[nodes.NewNode] with PythonAstVisitorHelpers {

  private val diffGraph = new DiffGraph.Builder()
  protected val nodeBuilder = new NodeBuilder(diffGraph)
  protected val edgeBuilder = new EdgeBuilder(diffGraph)

  private val contextStack = new ContextStack()

  def getDiffGraph: DiffGraph = {
    diffGraph.build()
  }

  override def visit(astNode: ast.iast): nodes.NewNode = ???

  override def visit(mod: ast.imod): NewNode = ???

  // Entry method for the visitor.
  override def visit(module: ast.Module): nodes.NewNode = {
    val fileNode = nodeBuilder.fileNode(fileName)
    val namespaceBlockNode = nodeBuilder.namespaceBlockNode(fileName)
    edgeBuilder.astEdge(namespaceBlockNode, fileNode, 1)

    contextStack.pushNamespaceBlock(namespaceBlockNode)

    val methodFullName = calcMethodFullNameFromContext("module")

    createMethod("module",
      methodFullName,
      (_: nodes.NewMethod) => (),
      body = module.stmts,
      decoratorList = Nil,
      returns = None,
      isAsync = false,
      LineAndColumn(1, 1)
    )

    contextStack.pop()

    null
  }

  override def visit(stmt: ast.istmt): NewNode = ???

  override def visit(functionDef: ast.FunctionDef): NewNode = {
    // TODO create local variable with same name as functionDef and assign the method reference
    // to it.
    createMethodAndMethodRef(functionDef.name,
      createParameterProcessingFunction(functionDef.args.asInstanceOf[ast.Arguments]),
      functionDef.body,
      functionDef.decorator_list,
      functionDef.returns,
      isAsync = false,
      lineAndColOf(functionDef))
  }

  override def visit(functionDef: ast.AsyncFunctionDef): NewNode = {
    // TODO create local variable with same name as functionDef and assign the method reference
    // to it.
    createMethodAndMethodRef(functionDef.name,
      createParameterProcessingFunction(functionDef.args.asInstanceOf[ast.Arguments]),
      functionDef.body,
      functionDef.decorator_list,
      functionDef.returns,
      isAsync = true,
      lineAndColOf(functionDef))
  }

  private def createParameterProcessingFunction(parameters: ast.Arguments)
                                               (methodNode: nodes.NewMethod): Unit = {
    val parameterOrder = if (contextStack.isClassContext) {
      new AutoIncIndex(0)
    } else {
      new AutoIncIndex(1)
    }
    parameters.posonlyargs.map(_.accept(this)).foreach { parameterNode =>
      edgeBuilder.astEdge(parameterNode, methodNode, parameterOrder.getAndInc)
    }
    parameters.args.map(_.accept(this)).foreach { parameterNode =>
      edgeBuilder.astEdge(parameterNode, methodNode, parameterOrder.getAndInc)
    }
    // TODO implement non position arguments and vararg.
  }

  private def createMethodAndMethodRef(methodName: String,
                                       parameterProcessing: nodes.NewMethod => Unit,
                                       body: Iterable[ast.istmt],
                                       decoratorList: Iterable[ast.iexpr],
                                       returns: Option[ast.iexpr],
                                       isAsync: Boolean,
                                       lineAndColumn: LineAndColumn): nodes.NewMethodRef = {
    val methodFullName = calcMethodFullNameFromContext(methodName)

    val methodNode =
      createMethod(methodName,
        methodFullName,
        parameterProcessing,
        body,
        decoratorList,
        returns,
        isAsync = true,
        lineAndColumn)

    val typeNode = nodeBuilder.typeNode(methodName, methodFullName)
    val typeDeclNode = nodeBuilder.typeDeclNode(methodName, methodFullName)
    edgeBuilder.astEdge(typeDeclNode, contextStack.astParent, contextStack.order.getAndInc)

    createBinding(methodNode, typeDeclNode)

    nodeBuilder.methodRefNode(methodName, methodFullName, lineAndColumn)
  }

  private def createMethod(name: String,
                           fullName: String,
                           parameterProcessing: nodes.NewMethod => Unit,
                           body: Iterable[ast.istmt],
                           decoratorList: Iterable[ast.iexpr],
                           returns: Option[ast.iexpr],
                           isAsync: Boolean,
                           lineAndColumn: LineAndColumn): nodes.NewMethod = {
    val methodNode = nodeBuilder.methodNode(name, fullName, lineAndColumn)
    edgeBuilder.astEdge(methodNode, contextStack.astParent, contextStack.order.getAndInc)

    contextStack.pushMethod(name, methodNode)

    val virtualModifierNode = nodeBuilder.modifierNode(ModifierTypes.VIRTUAL)
    edgeBuilder.astEdge(virtualModifierNode, methodNode, 0)

    val blockNode = nodeBuilder.blockNode("", lineAndColumn)
    edgeBuilder.astEdge(blockNode, methodNode, 1)

    parameterProcessing(methodNode)

    val methodReturnNode = nodeBuilder.methodReturnNode(lineAndColumn)
    edgeBuilder.astEdge(methodReturnNode, methodNode, 2)

    val bodyOrder = new AutoIncIndex(1)
    body.map(_.accept(this)).foreach { bodyStmt =>
      edgeBuilder.astEdge(bodyStmt, blockNode, bodyOrder.getAndInc)
    }

    contextStack.pop()
    methodNode
  }

  override def visit(classDef: ast.ClassDef): NewNode = ???

  override def visit(ret: ast.Return): NewNode = {
    ret.value match {
      case Some(value) =>
        val valueNode = value.accept(this)
        val code = "return " + codeOf(valueNode)
        val returnNode = nodeBuilder.returnNode(code, lineAndColOf(ret))

        addAstChildrenAsArguments(returnNode, 1, valueNode)
        returnNode
      case None =>
        nodeBuilder.returnNode("return", lineAndColOf(ret))
    }
  }

  override def visit(delete: ast.Delete): NewNode = {
    val deleteArgs = delete.targets.map(_.accept(this))

    val code = "del " + deleteArgs.map(codeOf).mkString(", ")
    val callNode = nodeBuilder.callNode(
      code,
      "<operator>.delete",
      DispatchTypes.STATIC_DISPATCH,
      lineAndColOf(delete)
    )

    addAstChildrenAsArguments(callNode, 1, deleteArgs)
    callNode
  }

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

  // TODO for now we ignore the annotation part and just emit the pure
  // assignment.
  override def visit(annotatedAssign: ast.AnnAssign): NewNode = {
    val targetNode = annotatedAssign.target.accept(this)

    annotatedAssign.value match {
      case Some(value) =>
        val valueNode = value.accept(this)
        createAssignment(targetNode, valueNode, lineAndColOf(annotatedAssign))
      case None =>
        // If there is no value, this is just an expr: annotation and since
        // we for now ignore the annotation we emit just the expr because
        // it may have side effects.
        targetNode
    }
  }

  override def visit(augAssign: ast.AugAssign): NewNode = {
    val targetNode = augAssign.target.accept(this)
    val valueNode = augAssign.value.accept(this)

    val (operatorCode, operatorFullName) =
      augAssign.op match {
        case ast.Add    => ("+=", Operators.assignmentPlus)
        case ast.Sub    => ("-=", Operators.assignmentMinus)
        case ast.Mult   => ("*=", Operators.assignmentMultiplication)
        case ast.MatMult =>
          ("@=", "<operator>.assignmentMatMult") // TODO make this a define and add policy for this
        case ast.Div    => ("/=", Operators.assignmentDivision)
        case ast.Mod    => ("%=", Operators.assignmentModulo)
        case ast.Pow    => ("**=", Operators.assignmentExponentiation)
        case ast.LShift => ("<<=", Operators.assignmentShiftLeft)
        case ast.RShift => ("<<=", Operators.assignmentArithmeticShiftRight)
        case ast.BitOr  => ("|=", Operators.assignmentOr)
        case ast.BitXor => ("^=", Operators.assignmentXor)
        case ast.BitAnd => ("&=", Operators.assignmentAnd)
        case ast.FloorDiv =>
          ("//=", "<operator>.assignmentFloorDiv") // TODO make this a define and add policy for this
      }

    createAugAssignment(targetNode, operatorCode, valueNode, operatorFullName, lineAndColOf(augAssign))
  }

  override def visit(forStmt: ast.For): NewNode = ???

  override def visit(forStmt: ast.AsyncFor): NewNode = ???

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

  override def visit(withStmt: ast.With): NewNode = ???

  override def visit(withStmt: ast.AsyncWith): NewNode = ???

  override def visit(raise: ast.Raise): NewNode = ???

  override def visit(tryStmt: ast.Try): NewNode = ???

  override def visit(assert: ast.Assert): NewNode = ???

  override def visit(importStmt: ast.Import): NewNode = ???

  override def visit(importFrom: ast.ImportFrom): NewNode = ???

  override def visit(global: ast.Global): NewNode = ???

  override def visit(nonlocal: ast.Nonlocal): NewNode = ???

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

  def visit(raise: ast.RaiseP2): NewNode = ???

  override def visit(errorStatement: ast.ErrorStatement): NewNode = ???

  override def visit(expr: ast.iexpr): NewNode = ???

  override def visit(boolOp: ast.BoolOp): nodes.NewNode = {
    def boolOpToCodeAndFullName(operator: ast.iboolop): () => (String, String) = {
      () => {
        operator match {
          case ast.And => ("and", Operators.logicalAnd)
          case ast.Or  => ("or", Operators.logicalOr)
        }
      }
    }

    val operandNodes = boolOp.values.map(_.accept(this))
    createNAryOperatorCall(boolOpToCodeAndFullName(boolOp.op), operandNodes, lineAndColOf(boolOp))
  }

  override def visit(namedExpr: ast.NamedExpr): NewNode = ???

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

  override def visit(lambda: ast.Lambda): NewNode = {
    // TODO test lambda expression.
    createMethodAndMethodRef("lambda",
      createParameterProcessingFunction(lambda.args.asInstanceOf[ast.Arguments]),
      Iterable.single(new ast.Return(lambda.body, lambda.attributeProvider)),
      decoratorList = Nil,
      returns = None,
      isAsync = false,
      lineAndColOf(lambda)
    )
  }

  override def visit(ifExp: ast.IfExp): NewNode = ???

  override def visit(dict: ast.Dict): NewNode = ???

  override def visit(set: ast.Set): NewNode = ???

  override def visit(listComp: ast.ListComp): NewNode = ???

  override def visit(setComp: ast.SetComp): NewNode = ???

  override def visit(dictComp: ast.DictComp): NewNode = ???

  override def visit(generatorExp: ast.GeneratorExp): NewNode = ???

  override def visit(await: ast.Await): NewNode = ???

  override def visit(yieldExpr: ast.Yield): NewNode = ???

  override def visit(yieldFrom: ast.YieldFrom): NewNode = ???

  // In case of a single compare operation there is no lowering applied.
  // So e.g. x < y stay untouched.
  // Otherwise the lowering is as follows:
  //  Src AST:
  //    x < y < z < a
  //  Lowering:
  //    {
  //      tmp1 = y
  //      x < tmp1 && {
  //        tmp2 = z
  //        tmp1 < tmp2 && {
  //          tmp2 < a
  //        }
  //      }
  //    }
  override def visit(compare: ast.Compare): NewNode = {
    assert(compare.ops.size == compare.comparators.size)
    var lhsNode = compare.left.accept(this)

    val topLevelExprNodes = lowerComparatorChain(lhsNode, compare.ops, compare.comparators, lineAndColOf(compare))
    if (topLevelExprNodes.size > 1) {
      createBlock(Iterable.empty, topLevelExprNodes, lineAndColOf(compare))
    } else {
      topLevelExprNodes.head
    }
  }

  private def compopToOpCodeAndFullName(compareOp: ast.icompop): () => (String, String) = {
    () => {
      compareOp match {
        case ast.Eq => ("==", Operators.equals)
        case ast.NotEq => ("!=", Operators.notEquals)
        case ast.Lt => ("<", Operators.lessThan)
        case ast.LtE => ("<=", Operators.lessEqualsThan)
        case ast.Gt => (">", Operators.greaterThan)
        case ast.GtE => (">=", Operators.greaterEqualsThan)
        case ast.Is => ("is", "<operator>.is")
        case ast.IsNot => ("is not", "<operator>.isNot")
        case ast.In => ("in", "<operator>.in")
        case ast.NotIn => ("not in", "<operator>.notIn")
      }
    }
  }

  def lowerComparatorChain(lhsNode: nodes.NewNode,
                           compOperators: Iterable[ast.icompop],
                           comparators: Iterable[ast.iexpr],
                           lineAndColumn: LineAndColumn): Iterable[nodes.NewNode] = {
    val rhsNode = comparators.head.accept(this)

    if (compOperators.size == 1) {
      val compareNode = createBinaryOperatorCall(lhsNode, compopToOpCodeAndFullName(compOperators.head), rhsNode, lineAndColumn)
      Iterable.single(compareNode)
    } else {
      val tmpVariableName = getUnusedName()
      val tmpLocalNode = nodeBuilder.localNode(tmpVariableName)
      val tmpIdentifierAssign = nodeBuilder.identifierNode(tmpVariableName, lineAndColumn)
      val assignmentNode = createAssignment(tmpIdentifierAssign, rhsNode, lineAndColumn)

      val tmpIdentifierCompare1 = nodeBuilder.identifierNode(tmpVariableName, lineAndColumn)
      val compareNode = createBinaryOperatorCall(lhsNode, compopToOpCodeAndFullName(compOperators.head), tmpIdentifierCompare1, lineAndColumn)

      val tmpIdentifierCompare2 = nodeBuilder.identifierNode(tmpVariableName, lineAndColumn)
      val childNodes = lowerComparatorChain(tmpIdentifierCompare2, compOperators.tail, comparators.tail, lineAndColumn)

      val blockNode = createBlock(Iterable.empty, childNodes, lineAndColumn)

      Iterable(assignmentNode, createBinaryOperatorCall(compareNode, andOpCodeAndFullName(),  blockNode, lineAndColumn))
    }
  }

  private def andOpCodeAndFullName(): () => (String, String) = {
    () => ("and", Operators.logicalAnd)
  }

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

  override def visit(subscript: ast.Subscript): NewNode = ???

  override def visit(starred: ast.Starred): NewNode = ???

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

  override def visit(tuple: ast.Tuple): NewNode = ???

  override def visit(slice: ast.Slice): NewNode = ???

  override def visit(stringExpList: ast.StringExpList): NewNode = ???

  override def visit(boolop: ast.iboolop): NewNode = ???

  override def visit(and: ast.And.type): NewNode = ???

  override def visit(or: ast.Or.type): NewNode = ???

  override def visit(operator: ast.ioperator): NewNode = ???

  override def visit(add: ast.Add.type): NewNode = ???

  override def visit(sub: ast.Sub.type): NewNode = ???

  override def visit(mult: ast.Mult.type): NewNode = ???

  override def visit(matMult: ast.MatMult.type): NewNode = ???

  override def visit(div: ast.Div.type): NewNode = ???

  override def visit(mod: ast.Mod.type): NewNode = ???

  override def visit(pow: ast.Pow.type): NewNode = ???

  override def visit(lShift: ast.LShift.type): NewNode = ???

  override def visit(rShift: ast.RShift.type): NewNode = ???

  override def visit(bitOr: ast.BitOr.type): NewNode = ???

  override def visit(bitXor: ast.BitXor.type): NewNode = ???

  override def visit(bitAnd: ast.BitAnd.type): NewNode = ???

  override def visit(floorDiv: ast.FloorDiv.type): NewNode = ???

  override def visit(unaryop: ast.iunaryop): NewNode = ???

  override def visit(invert: ast.Invert.type): NewNode = ???

  override def visit(not: ast.Not.type): NewNode = ???

  override def visit(uAdd: ast.UAdd.type): NewNode = ???

  override def visit(uSub: ast.USub.type): NewNode = ???

  override def visit(compop: ast.icompop): NewNode = ???

  override def visit(eq: ast.Eq.type): NewNode = ???

  override def visit(notEq: ast.NotEq.type): NewNode = ???

  override def visit(lt: ast.Lt.type): NewNode = ???

  override def visit(ltE: ast.LtE.type): NewNode = ???

  override def visit(gt: ast.Gt.type): NewNode = ???

  override def visit(gtE: ast.GtE.type): NewNode = ???

  override def visit(is: ast.Is.type): NewNode = ???

  override def visit(isNot: ast.IsNot.type): NewNode = ???

  override def visit(in: ast.In.type): NewNode = ???

  override def visit(notIn: ast.NotIn.type): NewNode = ???

  override def visit(comprehension: ast.icomprehension): NewNode = ???

  override def visit(comprehension: ast.Comprehension): NewNode = ???

  override def visit(exceptHandler: ast.iexcepthandler): NewNode = ???

  override def visit(exceptHandler: ast.ExceptHandler): NewNode = ???

  override def visit(arguments: ast.iarguments): NewNode = ???

  override def visit(arguments: ast.Arguments): NewNode = ???

  override def visit(arg: ast.iarg): NewNode = ???

  override def visit(arg: ast.Arg): NewNode = {
    nodeBuilder.methodParameterNode(arg.arg, lineAndColOf(arg))
  }

  override def visit(constant: ast.iconstant): NewNode = ???

  override def visit(stringConstant: ast.StringConstant): NewNode = ???

  override def visit(boolConstant: ast.BoolConstant): NewNode = ???

  override def visit(intConstant: ast.IntConstant): NewNode = ???

  override def visit(intConstant: ast.FloatConstant): NewNode = ???

  override def visit(imaginaryConstant: ast.ImaginaryConstant): NewNode = ???

  override def visit(noneConstant: ast.NoneConstant.type): NewNode = ???

  override def visit(ellipsisConstant: ast.EllipsisConstant.type): NewNode = ???

  override def visit(keyword: ast.ikeyword): NewNode = ???

  override def visit(keyword: ast.Keyword): NewNode = ???

  override def visit(alias: ast.ialias): NewNode = ???

  override def visit(alias: ast.Alias): NewNode = ???

  override def visit(withItem: ast.iwithitem): NewNode = ???

  override def visit(withItem: ast.WithItem): NewNode = ???

  override def visit(typeIgnore: ast.itype_ignore): NewNode = ???

  override def visit(typeIgnore: ast.TypeIgnore): NewNode = ???

  private def calcMethodFullNameFromContext(name: String): String = {
    val contextQualName = contextStack.qualName
    if (contextQualName != "") {
      fileName + ":" + contextQualName + "." + name
    } else {
      fileName + ":" + name
    }
  }
}
