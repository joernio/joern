package io.shiftleft.fuzzyc2cpg.passes.astcreation

import io.shiftleft.codepropertygraph.generated.nodes._
import io.shiftleft.codepropertygraph.generated.{ControlStructureTypes, EdgeTypes, Operators}
import io.shiftleft.fuzzyc2cpg.ast.AstNode
import io.shiftleft.fuzzyc2cpg.ast.declarations.{ClassDefStatement, IdentifierDecl}
import io.shiftleft.fuzzyc2cpg.ast.expressions.{
  AdditiveExpression,
  AndExpression,
  Argument,
  ArgumentList,
  ArrayIndexing,
  AssignmentExpression,
  BinaryExpression,
  BitAndExpression,
  CastExpression,
  CastTarget,
  Condition,
  ConditionalExpression,
  Constant,
  DeleteExpression,
  EqualityExpression,
  ExclusiveOrExpression,
  Expression,
  ForInit,
  Identifier,
  InclusiveOrExpression,
  InitializerList,
  MemberAccess,
  MultiplicativeExpression,
  NewExpression,
  OrExpression,
  PostIncDecOperationExpression,
  PtrMemberAccess,
  RelationalExpression,
  ShiftExpression,
  SizeofOperand,
  UnaryExpression
}
import io.shiftleft.fuzzyc2cpg.ast.functionDef.Template
import io.shiftleft.fuzzyc2cpg.ast.langc.expressions.{CallExpression, SizeofExpression}
import io.shiftleft.fuzzyc2cpg.ast.langc.functiondef.{FunctionDef, Parameter}
import io.shiftleft.fuzzyc2cpg.ast.langc.statements.blockstarters.IfStatement
import io.shiftleft.fuzzyc2cpg.ast.logical.statements.{BlockStarter, CompoundStatement, Label, Statement}
import io.shiftleft.fuzzyc2cpg.ast.statements.blockstarters.{CatchList, ForStatement}
import io.shiftleft.fuzzyc2cpg.ast.statements.jump.{
  BreakStatement,
  ContinueStatement,
  GotoStatement,
  ReturnStatement,
  ThrowStatement
}
import io.shiftleft.fuzzyc2cpg.ast.statements.{ExpressionStatement, IdentifierDeclStatement}
import io.shiftleft.fuzzyc2cpg.ast.walking.ASTNodeVisitor
import io.shiftleft.fuzzyc2cpg.{Defines, Global}
import io.shiftleft.passes.DiffGraph
import io.shiftleft.proto.cpg.Cpg.{DispatchTypes, EvaluationStrategies}
import org.slf4j.LoggerFactory

import scala.jdk.CollectionConverters._

private[astcreation] class AstCreator(diffGraph: DiffGraph.Builder,
                                      namespaceBlock: NewNamespaceBlock,
                                      global: Global,
                                      childNum: Int)
    extends ASTNodeVisitor {

  implicit def int2IntegerOpt(x: Option[Int]): Option[Integer] = x.map(java.lang.Integer.valueOf)
  implicit def int2Integer(x: Int): Integer = java.lang.Integer.valueOf(x)

  private val logger = LoggerFactory.getLogger(getClass)

  private var contextStack = List[Context]()
  private val scope = new Scope[String, (AbstractNode, String), AbstractNode]()

  pushContext(namespaceBlock, childNum)

  private class Context(val cpgParent: AbstractNode,
                        var childNum: Int,
                        val parentIsClassDef: Boolean,
                        val parentIsMemberAccess: Boolean = false,
                        var addConditionEdgeOnNextAstEdge: Boolean = false,
                        var addArgumentEdgeOnNextAstEdge: Boolean = false) {}

  private def pushContext(cpgParent: AbstractNode,
                          startChildNum: Int,
                          parentIsClassDef: Boolean = false,
                          parentIsMemberAccess: Boolean = false): Unit = {
    contextStack = new Context(cpgParent, startChildNum, parentIsClassDef, parentIsMemberAccess) :: contextStack
  }

  private def popContext(): Unit = contextStack = contextStack.tail
  private def context: Context = contextStack.head

  /**
    * Entry point for AST construction
    * */
  def convert(astNode: AstNode): Unit = astNode.accept(this)

  override def visit(astFunction: FunctionDef): Unit = {
    val returnType = if (astFunction.getReturnType != null) {
      astFunction.getReturnType.getEscapedCodeStr
    } else {
      "int"
    }

    val signature = returnType + " " + astFunction.getFunctionSignature(false)
    val code = returnType + " " + astFunction.getFunctionSignature(true)

    val location = astFunction.getLocation
    val method = NewMethod()
      .name(astFunction.getName)
      .code(code)
      .isExternal(false)
      .fullName(astFunction.getName)
      .lineNumber(location.startLine)
      .columnNumber(location.startPos)
      .lineNumberEnd(location.endLine)
      .columnNumberEnd(location.endPos)
      .signature(signature)
      .filename(namespaceBlock.filename)
      .order(context.childNum)

    addAndConnectAsAstChild(method)

    pushContext(method, 1)
    scope.pushNewScope(method)

    val templateParamList = astFunction.getTemplateParameterList
    if (templateParamList != null) {
      templateParamList.asScala.foreach { template =>
        template.accept(this)
      }
    }

    val methodReturnLocation =
      if (astFunction.getReturnType != null) {
        astFunction.getReturnType.getLocation
      } else {
        astFunction.getLocation
      }

    astFunction.getParameterList.asScala.foreach { parameter =>
      parameter.accept(this)
    }

    astFunction.getContent.accept(this)

    val retCode = Option(astFunction.getReturnType)
      .map(_.getEscapedCodeStr)
      .getOrElse("RET")

    val methodReturn = NewMethodReturn()
      .code(retCode)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE.name())
      .typeFullName(registerType(returnType))
      .lineNumber(methodReturnLocation.startLine)
      .columnNumber(methodReturnLocation.startPos)
      .order(context.childNum)

    addAndConnectAsAstChild(methodReturn)

    scope.popScope()
    popContext()
  }

  override def visit(astParameter: Parameter): Unit = {
    val parameterType = if (astParameter.getType != null) {
      astParameter.getType.getEscapedCodeStr
    } else {
      "int"
    }
    val location = astParameter.getLocation
    val parameter = NewMethodParameterIn()
      .code(astParameter.getEscapedCodeStr)
      .name(astParameter.getName)
      .order(astParameter.getChildNumber + 1)
      .evaluationStrategy(EvaluationStrategies.BY_VALUE.name())
      .typeFullName(registerType(parameterType))
      .lineNumber(location.startLine)
      .columnNumber(location.startPos)

    diffGraph.addNode(parameter)
    scope.addToScope(astParameter.getName, (parameter, parameterType))
    connectAstChild(parameter)
  }

  override def visit(template: Template): Unit = {
    // TODO (#60): Populate templated types in CPG.
    logger.debug("NYI: Template parsing.")
  }

  override def visit(argument: Argument): Unit = {
    argument.getExpression.accept(this)
  }

  override def visit(argumentList: ArgumentList): Unit = {
    acceptChildren(argumentList, withArgEdges = true)
  }

  override def visit(astAssignment: AssignmentExpression): Unit = {
    val operatorMethod = astAssignment.getOperator match {
      case "="   => Operators.assignment
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentDivision
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "<<=" => Operators.assignmentShiftLeft
      case ">>=" => Operators.assignmentArithmeticShiftRight
      case "&="  => Operators.assignmentAnd
      case "^="  => Operators.assignmentXor
      case "|="  => Operators.assignmentOr
    }
    visitBinaryExpr(astAssignment, operatorMethod)
  }

  override def visit(astAdd: AdditiveExpression): Unit = {
    val operatorMethod = astAdd.getOperator match {
      case "+" => Operators.addition
      case "-" => Operators.subtraction
    }
    visitBinaryExpr(astAdd, operatorMethod)
  }

  override def visit(astMult: MultiplicativeExpression): Unit = {
    val operatorMethod = astMult.getOperator match {
      case "*" => Operators.multiplication
      case "/" => Operators.division
      case "%" => Operators.modulo
    }
    visitBinaryExpr(astMult, operatorMethod)
  }

  override def visit(astRelation: RelationalExpression): Unit = {
    val operatorMethod = astRelation.getOperator match {
      case "<"  => Operators.lessThan
      case ">"  => Operators.greaterThan
      case "<=" => Operators.lessEqualsThan
      case ">=" => Operators.greaterEqualsThan
    }
    visitBinaryExpr(astRelation, operatorMethod)
  }

  override def visit(astShift: ShiftExpression): Unit = {
    val operatorMethod = astShift.getOperator match {
      case "<<" => Operators.shiftLeft
      case ">>" => Operators.arithmeticShiftRight
    }
    visitBinaryExpr(astShift, operatorMethod)
  }

  override def visit(astEquality: EqualityExpression): Unit = {
    val operatorMethod = astEquality.getOperator match {
      case "==" => Operators.equals
      case "!=" => Operators.notEquals
    }
    visitBinaryExpr(astEquality, operatorMethod)
  }

  override def visit(astBitAnd: BitAndExpression): Unit = {
    visitBinaryExpr(astBitAnd, Operators.and)
  }

  override def visit(astInclOr: InclusiveOrExpression): Unit = {
    visitBinaryExpr(astInclOr, Operators.or)
  }

  override def visit(astExclOr: ExclusiveOrExpression): Unit = {
    visitBinaryExpr(astExclOr, Operators.or)
  }

  override def visit(astOr: OrExpression): Unit = {
    visitBinaryExpr(astOr, Operators.logicalOr)
  }

  override def visit(astAnd: AndExpression): Unit = {
    visitBinaryExpr(astAnd, Operators.logicalAnd)
  }

  override def visit(astUnary: UnaryExpression): Unit = {
    Option(astUnary.getChild(0)) match {
      case Some(_) =>
        val operatorMethod = astUnary.getChild(0).getEscapedCodeStr match {
          case "+"  => Operators.plus
          case "-"  => Operators.minus
          case "*"  => Operators.indirection
          case "&"  => Operators.addressOf
          case "~"  => Operators.not
          case "!"  => Operators.logicalNot
          case "++" => Operators.preIncrement
          case "--" => Operators.preDecrement
        }

        val cpgUnary = newCallNode(astUnary, operatorMethod)

        addAndConnectAsAstChild(cpgUnary)

        pushContext(cpgUnary, 1)
        context.addArgumentEdgeOnNextAstEdge = true
        astUnary.getChild(1).accept(this)
        popContext()
      case None =>
        // We get here for `new` expression.
        val cpgNew = newUnknownNode(astUnary)
        addAndConnectAsAstChild(cpgNew)
    }
  }

  override def visit(astPostIncDecOp: PostIncDecOperationExpression): Unit = {
    val operatorMethod = astPostIncDecOp.getChild(1).getEscapedCodeStr match {
      case "++" => Operators.postIncrement
      case "--" => Operators.postDecrement
    }

    val cpgPostIncDecOp = newCallNode(astPostIncDecOp, operatorMethod)

    diffGraph.addNode(cpgPostIncDecOp)
    connectAstChild(cpgPostIncDecOp)

    pushContext(cpgPostIncDecOp, 1)
    context.addArgumentEdgeOnNextAstEdge = true
    astPostIncDecOp.getChild(0).accept(this)
    popContext()
  }

  override def visit(astCall: CallExpression): Unit = {
    val targetMethodName = astCall.getChild(0).getEscapedCodeStr
    // TODO the DISPATCH_TYPE needs to depend on the type of the identifier which is "called".
    // At the moment we use STATIC_DISPATCH also for calls of function pointers.
    // When this is done we need to draw a RECEIVER edge for DYNAMIC_DISPATCH function pointer
    // calls to the pointer expression.
    val cpgCall = newCallNode(astCall, targetMethodName)

    diffGraph.addNode(cpgCall)
    connectAstChild(cpgCall)

    pushContext(cpgCall, 1)
    // Argument edges are added when visiting each individual argument.
    astCall.getArgumentList.accept(this)
    popContext()
  }

  override def visit(astNew: NewExpression): Unit = {
    val call = newCallNode(astNew, "<operator>.new")

    diffGraph.addNode(call)
    connectAstChild(call)
    pushContext(call, 1)
    context.addArgumentEdgeOnNextAstEdge = true
    astNew.getTargetClass.accept(this)
    astNew.getArgumentList.accept(this)
    popContext()
  }

  override def visit(astDelete: DeleteExpression): Unit = {
    val call = newCallNode(astDelete, Operators.delete);

    diffGraph.addNode(call)
    connectAstChild(call)
    pushContext(call, 1)
    context.addArgumentEdgeOnNextAstEdge = true;
    astDelete.getTarget.accept(this)
    popContext()
  }

  override def visit(astConstant: Constant): Unit = {
    val constantType = deriveConstantTypeFromCode(astConstant.getEscapedCodeStr)
    val cpgConstant = NewLiteral()
      .typeFullName(registerType(constantType))
      .code(astConstant.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(astConstant.getLocation.startLine)
      .columnNumber(astConstant.getLocation.startPos)
    diffGraph.addNode(cpgConstant)
    connectAstChild(cpgConstant)
  }

  override def visit(astNode: BreakStatement): Unit = {
    val node = newControlStructureNode(astNode)
    diffGraph.addNode(node)
    connectAstChild(node)
  }

  override def visit(astNode: ContinueStatement): Unit = {
    val node = newControlStructureNode(astNode)
    diffGraph.addNode(node)
    connectAstChild(node)
  }

  override def visit(astNode: GotoStatement): Unit = {
    val node = newControlStructureNode(astNode)
    diffGraph.addNode(node)
    connectAstChild(node)
  }

  override def visit(astIdentifier: Identifier): Unit = {
    val identifierName = astIdentifier.getEscapedCodeStr

    if (contextStack.nonEmpty && contextStack.head.parentIsMemberAccess && contextStack.head.childNum == 2) {
      val cpgFieldIdentifier = NewFieldIdentifier()
        .canonicalName(identifierName)
        .code(astIdentifier.getEscapedCodeStr)
        .order(context.childNum)
        .argumentIndex(context.childNum)
        .lineNumber(astIdentifier.getLocation.startLine)
        .columnNumber(astIdentifier.getLocation.startPos)
      diffGraph.addNode(cpgFieldIdentifier)
      connectAstChild(cpgFieldIdentifier)
      return
    }

    val variableOption = scope.lookupVariable(identifierName)
    val identifierTypeName = variableOption match {
      case Some((_, variableTypeName)) =>
        variableTypeName
      case None =>
        Defines.anyTypeName
    }

    val cpgIdentifier = NewIdentifier()
      .name(identifierName)
      .typeFullName(registerType(identifierTypeName))
      .code(astIdentifier.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(astIdentifier.getLocation.startLine)
      .columnNumber(astIdentifier.getLocation.startPos)

    diffGraph.addNode(cpgIdentifier)
    connectAstChild(cpgIdentifier)

    variableOption match {
      case Some((variable, _)) =>
        diffGraph.addEdge(cpgIdentifier, variable, EdgeTypes.REF)
      case None =>
    }

  }

  override def visit(condition: Condition): Unit = {
    //not called for ConditionalExpression, cf joern#91
    context.addConditionEdgeOnNextAstEdge = true
    condition.getExpression.accept(this)
  }

  override def visit(astConditionalExpr: ConditionalExpression): Unit = {
    //this ought to be a ControlStructureNode, but we currently cannot handle that in the dataflow tracker
    val cpgConditionalExpr = newCallNode(astConditionalExpr, Operators.conditional)
    diffGraph.addNode(cpgConditionalExpr)
    connectAstChild(cpgConditionalExpr)
    val condition = astConditionalExpr.getChild(0).asInstanceOf[Condition]
    val trueExpression = astConditionalExpr.getChild(1)
    val falseExpression = astConditionalExpr.getChild(2)
    // avoid setting context.addConditionEdgeOnNextAstEdge in this.visit(condition), cf joern#91
    pushContext(cpgConditionalExpr, 1)
    context.addArgumentEdgeOnNextAstEdge = true
    condition.getExpression.accept(this)
    context.addArgumentEdgeOnNextAstEdge = true
    trueExpression.accept(this)
    context.addArgumentEdgeOnNextAstEdge = true
    falseExpression.accept(this)
    popContext()
  }

  override def visit(expression: Expression): Unit = {
    // We only end up here for expressions chained by ','.
    // Those expressions are then the children of the expression
    // given as parameter.
    val classOfExpression = expression.getClass
    if (classOfExpression != classOf[Expression]) {
      throw new RuntimeException(
        s"Only direct instances of Expressions expected " +
          s"but ${classOfExpression.getSimpleName} found")
    }

    val cpgBlock = NewBlock()
      .code("")
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .typeFullName(registerType(Defines.anyTypeName))
      .lineNumber(expression.getLocation.startLine)
      .columnNumber(expression.getLocation.startPos)

    diffGraph.addNode(cpgBlock)
    connectAstChild(cpgBlock)
    pushContext(cpgBlock, 1)
    acceptChildren(expression)
    popContext()
  }

  override def visit(forInit: ForInit): Unit = {
    acceptChildren(forInit)
  }

  override def visit(astBlockStarter: BlockStarter): Unit = {
    val cpgBlockStarter = newControlStructureNode(astBlockStarter)
    diffGraph.addNode(cpgBlockStarter)
    connectAstChild(cpgBlockStarter)
    pushContext(cpgBlockStarter, 1)

    astBlockStarter match {
      case forStatement: ForStatement =>
        // Special handling of for statements: since all three
        // parts of a for statement are optional, the AST we emit
        // does not currently allow distinguishing the three. We
        // may want to perform more elaborate modeling here in the
        // future. For now, we increase ORDER even when expressions
        // are empty, making it possible to distinguish the three
        // by the ORDER field.
        Option(forStatement.getForInitExpression)
          .map(_.accept(this))
          .getOrElse(context.childNum += 1)
        Option(forStatement.getCondition)
          .map(_.accept(this))
          .getOrElse { context.childNum += 1 }
        Option(forStatement.getForLoopExpression)
          .map(_.accept(this))
          .getOrElse(context.childNum += 1)
        Option(forStatement.getStatement)
          .map(_.accept(this))
          .getOrElse(context.childNum += 1)
      case _ =>
        acceptChildren(astBlockStarter)
    }

    popContext()
  }

  override def visit(astCatchList: CatchList): Unit = {
    val cpgCatchList = newUnknownNode(astCatchList)
    diffGraph.addNode(cpgCatchList)
    connectAstChild(cpgCatchList)

    pushContext(cpgCatchList, 1)
    astCatchList.asScala.foreach { catchElement =>
      catchElement.accept(this)
    }
    popContext()
  }

  override def visit(astThrow: ThrowStatement): Unit = {
    val cpgThrow = newControlStructureNode(astThrow)

    addAndConnectAsAstChild(cpgThrow)

    pushContext(cpgThrow, 1)
    val throwExpression = astThrow.getThrowExpression
    if (throwExpression != null) {
      throwExpression.accept(this)
    }
    popContext()
  }

  override def visit(astIfStmt: IfStatement): Unit = {
    val cpgIfStmt = newControlStructureNode(astIfStmt)
    addAndConnectAsAstChild(cpgIfStmt)
    pushContext(cpgIfStmt, 1)

    astIfStmt.getCondition.accept(this)
    astIfStmt.getStatement.accept(this)
    val astElseStmt = astIfStmt.getElseNode
    if (astElseStmt != null) {
      astElseStmt.accept(this)
    }
    popContext()
  }

  override def visit(statement: ExpressionStatement): Unit = {
    Option(statement.getExpression).foreach(_.accept(this))
  }

  override def visit(astBlock: CompoundStatement): Unit = {
    if (context.parentIsClassDef) {
      astBlock.getStatements.asScala.foreach { statement =>
        statement.accept(this)
      }
    } else {
      val block = NewBlock()
        .code("")
        .order(context.childNum)
        .argumentIndex(context.childNum)
        .typeFullName(registerType(Defines.voidTypeName))
        .lineNumber(astBlock.getLocation.startLine)
        .columnNumber(astBlock.getLocation.startPos)

      diffGraph.addNode(block)
      connectAstChild(block)

      pushContext(block, 1)
      scope.pushNewScope(block)
      astBlock.getStatements.asScala.foreach { statement =>
        statement.accept(this)
      }
      popContext()
      scope.popScope()
    }
  }

  override def visit(astNode: ReturnStatement): Unit = {
    val cpgReturn = NewReturn()
      .code(astNode.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(astNode.getLocation.startLine)
      .columnNumber(astNode.getLocation.startPos)

    addAndConnectAsAstChild(cpgReturn)

    pushContext(cpgReturn, 1)
    Option(astNode.getReturnExpression).foreach { returnExpr =>
      context.addArgumentEdgeOnNextAstEdge = true
      returnExpr.accept(this)
    }
    popContext()
  }

  override def visit(astIdentifierDeclStmt: IdentifierDeclStatement): Unit = {
    astIdentifierDeclStmt.getIdentifierDeclList.asScala.foreach { identifierDecl =>
      identifierDecl.accept(this)
    }
  }

  override def visit(identifierDecl: IdentifierDecl): Unit = {
    val declTypeName = identifierDecl.getType.getEscapedCodeStr

    if (identifierDecl.isTypedef) {
      val aliasTypeDecl = NewTypeDecl()
        .name(identifierDecl.getName.getEscapedCodeStr)
        .fullName(identifierDecl.getName.getEscapedCodeStr)
        .isExternal(false)
        .aliasTypeFullName(Some(registerType(declTypeName)))
        .filename(namespaceBlock.filename)
        .order(context.childNum)

      diffGraph.addNode(aliasTypeDecl)
      connectAstChild(aliasTypeDecl)
    } else if (context.parentIsClassDef) {
      val member = NewMember()
        .code(identifierDecl.getEscapedCodeStr)
        .name(identifierDecl.getName.getEscapedCodeStr)
        .typeFullName(registerType(declTypeName))
        .order(context.childNum)
      diffGraph.addNode(member)
      connectAstChild(member)
    } else {
      // We only process file level identifier declarations if they are typedefs.
      // Everything else is ignored.
      if (!scope.isEmpty) {
        val localName = identifierDecl.getName.getEscapedCodeStr
        val local = NewLocal()
          .code(localName)
          .name(localName)
          .typeFullName(registerType(declTypeName))
          .order(context.childNum)

        diffGraph.addNode(local)
        scope.addToScope(localName, (local, declTypeName))
        connectAstChild(local)

        val assignmentExpression = identifierDecl.getAssignment
        if (assignmentExpression != null) {
          assignmentExpression.accept(this)
        }
      }
    }
  }

  override def visit(astSizeof: SizeofExpression): Unit = {
    val cpgSizeof = newCallNode(astSizeof, Operators.sizeOf)

    addAndConnectAsAstChild(cpgSizeof)

    pushContext(cpgSizeof, 1)
    // Child 0 is just the keyword 'sizeof' which at this point is duplicate
    // information for us.
    context.addArgumentEdgeOnNextAstEdge = true
    astSizeof.getChild(1).accept(this)
    popContext()
  }

  override def visit(astSizeofOperand: SizeofOperand): Unit = {
    astSizeofOperand.getChildCount match {
      case 0 =>
        // Operand is a type.
        val cpgTypeRef = newUnknownNode(astSizeofOperand)
        addAndConnectAsAstChild(cpgTypeRef)
      case 1 =>
        // Operand is an expression.
        astSizeofOperand.getChild(1).accept(this)
    }
  }

  override def visit(astLabel: Label): Unit = {
    val cpgLabel = NewJumpTarget()
      .parserTypeName(astLabel.getClass.getSimpleName)
      .name(astLabel.getLabelName)
      .code(astLabel.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(astLabel.getLocation.startLine)
      .columnNumber(astLabel.getLocation.startPos)
    addAndConnectAsAstChild(cpgLabel)
  }

  override def visit(astArrayIndexing: ArrayIndexing): Unit = {
    val cpgArrayIndexing =
      newCallNode(astArrayIndexing, Operators.indirectIndexAccess)

    addAndConnectAsAstChild(cpgArrayIndexing)

    pushContext(cpgArrayIndexing, 1)
    context.addArgumentEdgeOnNextAstEdge = true
    astArrayIndexing.getArrayExpression.accept(this)
    context.addArgumentEdgeOnNextAstEdge = true
    astArrayIndexing.getIndexExpression.accept(this)
    popContext()
  }

  override def visit(astCast: CastExpression): Unit = {
    val cpgCast = newCallNode(astCast, Operators.cast)

    addAndConnectAsAstChild(cpgCast)

    pushContext(cpgCast, 1)
    context.addArgumentEdgeOnNextAstEdge = true
    astCast.getCastTarget.accept(this)
    context.addArgumentEdgeOnNextAstEdge = true
    astCast.getCastExpression.accept(this)
    popContext()
  }

  override def visit(astMemberAccess: MemberAccess): Unit = {
    val cpgMemberAccess =
      newCallNode(astMemberAccess, Operators.fieldAccess)

    addAndConnectAsAstChild(cpgMemberAccess)

    pushContext(cpgMemberAccess, 1, parentIsMemberAccess = true)
    acceptChildren(astMemberAccess, withArgEdges = true)
    popContext()
  }

  override def visit(astPtrMemberAccess: PtrMemberAccess): Unit = {
    val cpgPtrMemberAccess =
      newCallNode(astPtrMemberAccess, Operators.indirectFieldAccess)

    addAndConnectAsAstChild(cpgPtrMemberAccess)

    pushContext(cpgPtrMemberAccess, 1, parentIsMemberAccess = true)
    acceptChildren(astPtrMemberAccess, withArgEdges = true)
    popContext()
  }

  override def visit(astCastTarget: CastTarget): Unit = {
    val cpgCastTarget = newUnknownNode(astCastTarget)
    addAndConnectAsAstChild(cpgCastTarget)
  }

  override def visit(astInitializerList: InitializerList): Unit = {
    // TODO figure out how to represent.
  }

  override def visit(statement: Statement): Unit = {
    if (statement.getChildCount != 0) {
      throw new RuntimeException("Unhandled statement type: " + statement.getClass)
    } else {
      logger.debug("Parse error. Code: {}", statement.getEscapedCodeStr)
    }
  }

  override def visit(astClassDef: ClassDefStatement): Unit = {
    // TODO: currently NAME and FULL_NAME are the same, since
    // the parser does not detect C++ namespaces. Change that,
    // once the parser handles namespaces.
    var name = astClassDef.identifier.toString
    name = name.substring(1, name.length - 1)
    val baseClassList = astClassDef.baseClasses.asScala.map { identifier =>
      val baseClassName = identifier.toString
      baseClassName.substring(1, baseClassName.length - 1)
    }.toList

    baseClassList.foreach(registerType)

    val typeDecl = NewTypeDecl()
      .name(name)
      .fullName(name)
      .isExternal(false)
      .inheritsFromTypeFullName(baseClassList)
      .filename(namespaceBlock.filename)
      .order(context.childNum)

    diffGraph.addNode(typeDecl)
    connectAstChild(typeDecl)

    val templateParamList = astClassDef.getTemplateParameterList
    if (templateParamList != null) {
      templateParamList.asScala.foreach { template =>
        template.accept(this)
      }
    }

    pushContext(typeDecl, 1, parentIsClassDef = true)
    astClassDef.content.accept(this)
    popContext()
  }

  private def visitBinaryExpr(astBinaryExpr: BinaryExpression, operatorMethod: String): Unit = {
    val cpgBinaryExpr = newCallNode(astBinaryExpr, operatorMethod)
    diffGraph.addNode(cpgBinaryExpr)
    connectAstChild(cpgBinaryExpr)

    pushContext(cpgBinaryExpr, 1)

    context.addArgumentEdgeOnNextAstEdge = true
    astBinaryExpr.getLeft.accept(this)

    context.addArgumentEdgeOnNextAstEdge = true
    astBinaryExpr.getRight.accept(this)

    popContext()
  }

  private def acceptChildren(node: AstNode, withArgEdges: Boolean = false): Unit = {
    node.getChildIterator.forEachRemaining { child =>
      context.addArgumentEdgeOnNextAstEdge = withArgEdges
      child.accept(this)
    }
  }

  // TODO Implement this method properly, the current implementation is just a
  // quick hack to have some implementation at all.
  private def deriveConstantTypeFromCode(code: String): String = {
    val firstChar = code.charAt(0)
    val lastChar = code.charAt(code.length - 1)
    if (firstChar == '"') {
      Defines.charPointerTypeName
    } else if (firstChar == '\'') {
      Defines.charTypeName
    } else if (lastChar == 'f' || lastChar == 'F') {
      Defines.floatTypeName
    } else if (lastChar == 'd' || lastChar == 'D') {
      Defines.doubleTypeName
    } else if (lastChar == 'l' || lastChar == 'L') {
      Defines.longTypeName
    } else if (code.endsWith("ll") || code.endsWith("LL")) {
      Defines.longlongTypeName
    } else {
      Defines.intTypeName
    }
  }

  private def registerType(typeName: String): String = {
    global.usedTypes.put(typeName, true)
    typeName
  }

  private def addAndConnectAsAstChild(node: NewNode): Unit = {
    diffGraph.addNode(node)
    connectAstChild(node)
  }

  private def connectAstChild(child: NewNode): Unit = {
    diffGraph.addEdge(context.cpgParent, child, EdgeTypes.AST)
    context.childNum += 1
    if (context.addConditionEdgeOnNextAstEdge) {
      diffGraph.addEdge(context.cpgParent, child, EdgeTypes.CONDITION)
      context.addConditionEdgeOnNextAstEdge = false
    }

    if (context.addArgumentEdgeOnNextAstEdge) {
      diffGraph.addEdge(context.cpgParent, child, EdgeTypes.ARGUMENT)
      context.addArgumentEdgeOnNextAstEdge = false
    }
  }

  private def newCallNode(astNode: AstNode, methodName: String): NewCall = {
    val location = astNode.getLocation
    NewCall()
      .name(methodName)
      .dispatchType(DispatchTypes.STATIC_DISPATCH.name())
      .signature("TODO")
      .methodFullName(methodName)
      .code(astNode.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(location.startLine)
      .columnNumber(location.startPos)
  }

  private def newUnknownNode(astNode: AstNode): NewUnknown = {
    val location = astNode.getLocation
    NewUnknown()
      .parserTypeName(astNode.getClass.getSimpleName)
      .code(astNode.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(location.startLine)
      .columnNumber(location.startPos)
  }

  private def newControlStructureNode(astNode: AstNode): NewControlStructure = {
    val location = astNode.getLocation
    val controlStructureType = astNode.getClass.getSimpleName match {
      case "BreakStatement"    => ControlStructureTypes.BREAK
      case "ContinueStatement" => ControlStructureTypes.CONTINUE
      case "WhileStatement"    => ControlStructureTypes.WHILE
      case "DoStatement"       => ControlStructureTypes.DO
      case "ForStatement"      => ControlStructureTypes.FOR
      case "GotoStatement"     => ControlStructureTypes.GOTO
      case "IfStatement"       => ControlStructureTypes.IF
      case "ElseStatement"     => ControlStructureTypes.ELSE
      case "SwitchStatement"   => ControlStructureTypes.SWITCH
      case "TryStatement"      => ControlStructureTypes.TRY
      case someThingElse       => someThingElse
    }
    NewControlStructure()
      .parserTypeName(astNode.getClass.getSimpleName)
      .controlStructureType(controlStructureType)
      .code(astNode.getEscapedCodeStr)
      .order(context.childNum)
      .argumentIndex(context.childNum)
      .lineNumber(location.startLine)
      .columnNumber(location.startPos)
  }

}
