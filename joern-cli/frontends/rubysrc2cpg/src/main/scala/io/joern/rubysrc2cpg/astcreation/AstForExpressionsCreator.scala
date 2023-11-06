package io.joern.rubysrc2cpg.astcreation

import io.joern.rubysrc2cpg.parser.ParserAst
import io.joern.rubysrc2cpg.parser.ParserAst.*
import io.joern.rubysrc2cpg.passes.Defines
import io.joern.rubysrc2cpg.passes.Defines.{RubyOperators, getBuiltInType}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import org.antlr.v4.runtime.ParserRuleContext

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForExpression(ctx: ParserRuleContext): Ast = astForExpression(ParserAst(ctx))

  protected def astForExpression(node: ParserNode): Ast = node match
    case node: StaticLiteral         => astForStaticLiteral(node)
    case node: DynamicLiteral        => astForDynamicLiteral(node)
    case node: UnaryExpression       => astForUnary(node)
    case node: BinaryExpression      => astForBinary(node)
    case node: ConditionalExpression => astForConditional(node)
    case node: MemberAccess          => astForMemberAccess(node)
    case node: MemberCall            => astForMemberCall(node)
    case node: IndexAccess           => astForIndexAccess(node)
    case node: SingleAssignment      => astForSingleAssignment(node)
    case node: AttributeAssignment   => astForAttributeAssignment(node)
    case node: SimpleIdentifier      => astForSimpleIdentifier(node)
    case node: SimpleCall            => astForSimpleCall(node)
    case node: RangeExpression       => astForRange(node)
    case node: ArrayLiteral          => astForArrayLiteral(node)
    case node: HashLiteral           => astForHashLiteral(node)
    case node: Association           => astForAssociation(node)
    case _                           => astForUnknown(node)

  protected def astForStaticLiteral(node: StaticLiteral): Ast = {
    Ast(literalNode(node, node.text, node.typeFullName))
  }

  protected def astForDynamicLiteral(node: DynamicLiteral): Ast = {
    val fmtValueAsts = node.expressions.map { expressionCtx =>
      ParserAst(expressionCtx) match
        case stmtList: StatementList if stmtList.size == 1 =>
          val expressionAst = astForExpression(stmtList.statements.head)
          val call = callNode(
            node = stmtList,
            code = stmtList.text,
            name = Operators.formattedValue,
            methodFullName = Operators.formattedValue,
            dispatchType = DispatchTypes.STATIC_DISPATCH,
            signature = None,
            typeFullName = Some(node.typeFullName)
          )
          callAst(call, Seq(expressionAst))
        case stmtList: StatementList if stmtList.size > 1 =>
          logger.warn(
            s"Interpolations containing multiple statements are not supported yet: ${stmtList.text} ($relativeFileName), skipping"
          )
          astForUnknown(stmtList)
        case node =>
          logger.warn(s"Unsupported interpolated literal content: ${node.text} ($relativeFileName), skipping")
          astForUnknown(node)
    }
    callAst(
      callNode(
        node = node,
        code = node.text,
        name = Operators.formatString,
        methodFullName = Operators.formatString,
        dispatchType = DispatchTypes.STATIC_DISPATCH,
        signature = None,
        typeFullName = Some(node.typeFullName)
      ),
      fmtValueAsts
    )
  }

  protected def astForUnary(node: UnaryExpression): Ast = {
    getUnaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized unary operator: ${node.text} ($relativeFileName), skipping")
        astForUnknown(node)
      case Some(op) =>
        val expressionAst = astForExpression(node.expression)
        val call          = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(expressionAst))
  }

  protected def astForBinary(node: BinaryExpression): Ast = {
    getBinaryOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized binary operator: ${node.text} ($relativeFileName), skipping")
        astForUnknown(node)
      case Some(op) =>
        val lhsAst = astForExpression(node.lhs)
        val rhsAst = astForExpression(node.rhs)
        val call   = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        callAst(call, Seq(lhsAst, rhsAst))
  }

  protected def astForConditional(node: ConditionalExpression): Ast = {
    val conditionAst = astForExpression(node.condition)
    val thenAst      = astForExpression(node.trueBranch)
    val elseAst      = astForExpression(node.falseBranch)
    val call = callNode(node, node.text, Operators.conditional, Operators.conditional, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(conditionAst, thenAst, elseAst))
  }

  // Member accesses are lowered as calls, i.e. `x.y` is the call of `y` of `x` without any arguments.
  protected def astForMemberAccess(node: MemberAccess): Ast = {
    astForMemberCall(MemberCall(node.ctx, node.target, node.op, node.methodName, List.empty))
  }

  protected def astForMemberCall(node: ParserAst.MemberCall): Ast = {
    val fullName        = node.methodName // TODO
    val fieldAccessAst  = astForFieldAccess(MemberAccess(node.ctx, node.target, node.op, node.methodName))
    val argumentAsts    = node.arguments.map(astForExpression)
    val fieldAccessCall = callNode(node, node.text, node.methodName, fullName, DispatchTypes.STATIC_DISPATCH)
    callAst(fieldAccessCall, argumentAsts, Some(fieldAccessAst))
  }

  protected def astForIndexAccess(node: IndexAccess): Ast = {
    val indexAsts = node.indices.map(astForExpression)
    val targetAst = astForExpression(node.target)
    val call = callNode(node, node.text, Operators.indexAccess, Operators.indexAccess, DispatchTypes.STATIC_DISPATCH)
    callAst(call, indexAsts, Some(targetAst))
  }

  protected def astForSingleAssignment(node: SingleAssignment): Ast = {
    getAssignmentOperatorName(node.op) match
      case None =>
        logger.warn(s"Unrecognized assignment operator: ${node.text} ($relativeFileName), skipping")
        astForUnknown(node)
      case Some(op) =>
        val lhsAst        = astForExpression(node.lhs)
        val rhsAst        = astForExpression(node.rhs)
        val call          = callNode(node, node.text, op, op, DispatchTypes.STATIC_DISPATCH)
        val assignmentAst = callAst(call, Seq(lhsAst, rhsAst))
        scope.lookupVariable(node.lhs.getText) match
          case None =>
            // We are introducing a new variable. Thus, also create a LOCAL.
            // TODO: Add the newly created LOCAL to the current METHOD.
            val lhsNode = ParserAst(node.lhs)
            val local   = localNode(lhsNode, lhsNode.text, lhsNode.text, Defines.Any)
            scope.addToScope(lhsNode.text, local)
            assignmentAst
          case Some(_) =>
            // This variable is already in scope, so just emit the assignment.
            assignmentAst
  }

  // `x.y = 1` is lowered as `x.y=(1)`, i.e. as calling `y=` on `x` with argument `1`
  protected def astForAttributeAssignment(node: AttributeAssignment): Ast = {
    val call         = SimpleCall(node.ctx, node.ctx, List(node.rhs))
    val memberAccess = MemberAccess(node.ctx, node.target, ".", s"${node.attributeName}=")
    astForMemberCallWithoutBlock(call, memberAccess)
  }

  protected def astForSimpleIdentifier(node: SimpleIdentifier): Ast = {
    val name = node.text
    scope.lookupVariable(name) match
      case None    => astForSimpleCall(SimpleCall(node.ctx, node.ctx, List()))
      case Some(_) => Ast(identifierNode(node, name, name, node.typeFullName.getOrElse(Defines.Any)))
  }

  protected def astForSimpleCall(node: SimpleCall): Ast = {
    ParserAst(node.target) match
      case targetNode: SimpleIdentifier => astForMethodCallWithoutBlock(node, targetNode)
      case targetNode: MemberAccess     => astForMemberCallWithoutBlock(node, targetNode)
      case targetNode =>
        logger.warn(s"Unrecognized target of call: ${targetNode.text} ($relativeFileName), skipping")
        astForUnknown(targetNode)
  }

  protected def astForRange(node: RangeExpression): Ast = {
    val lbAst = astForExpression(node.lowerBound)
    val ubAst = astForExpression(node.upperBound)
    val call  = callNode(node, node.text, Operators.range, Operators.range, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(lbAst, ubAst))
  }

  protected def astForArrayLiteral(node: ArrayLiteral): Ast = {
    if (node.isDynamic) {
      logger.warn(s"Interpolated array literals are not supported yet: ${node.text} ($relativeFileName), skipping")
      astForUnknown(node)
    } else {
      val argumentsType = if (node.isStringArray) {
        getBuiltInType(Defines.String)
      } else {
        getBuiltInType(Defines.Symbol)
      }
      val argumentLiterals = node.elements.map(StaticLiteral(_, argumentsType))
      val argumentAsts     = argumentLiterals.map(astForExpression)
      val call =
        callNode(node, node.text, Operators.arrayInitializer, Operators.arrayInitializer, DispatchTypes.STATIC_DISPATCH)
      callAst(call, argumentAsts)
    }
  }

  protected def astForHashLiteral(node: HashLiteral): Ast = {
    val argumentAsts = node.elements.flatMap(ctx =>
      ParserAst(ctx) match
        case associationNode: Association => astForAssociation(associationNode) :: Nil
        case node =>
          logger.warn(s"Could not represent element: ${node.text} ($relativeFileName), skipping")
          astForUnknown(node) :: Nil
    )
    val call = callNode(
      node,
      node.text,
      RubyOperators.hashInitializer,
      RubyOperators.hashInitializer,
      DispatchTypes.STATIC_DISPATCH
    )
    callAst(call, argumentAsts)
  }

  protected def astForAssociation(node: Association): Ast = {
    val key   = astForExpression(node.key)
    val value = astForExpression(node.value)
    val call =
      callNode(node, node.text, RubyOperators.association, RubyOperators.association, DispatchTypes.STATIC_DISPATCH)
    callAst(call, Seq(key, value))
  }

  protected def astForUnknown(node: ParserNode): Ast = {
    val className = node.getClass.getSimpleName
    val text      = node.text
    logger.warn(s"Could not represent expression: $text ($className) ($relativeFileName), skipping")
    Ast(unknownNode(node, text))
  }

  private def astForMemberCallWithoutBlock(node: SimpleCall, memberAccess: MemberAccess): Ast = {
    val receiverAst    = astForFieldAccess(memberAccess)
    val methodName     = memberAccess.methodName
    val methodFullName = methodName // TODO
    val argumentAsts   = node.arguments.map(astForExpression)
    val call           = callNode(node, node.text, methodName, methodFullName, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argumentAsts, None, Some(receiverAst))
  }

  private def astForMethodCallWithoutBlock(node: SimpleCall, methodIdentifier: SimpleIdentifier): Ast = {
    val methodName     = methodIdentifier.ctx.getText
    val methodFullName = methodName // TODO
    val argumentAst    = node.arguments.map(astForExpression)
    val call           = callNode(node, node.text, methodName, methodFullName, DispatchTypes.STATIC_DISPATCH)
    callAst(call, argumentAst, None, None)
  }

  protected def astForFieldAccess(node: MemberAccess): Ast = {
    val fieldIdentifierAst = Ast(fieldIdentifierNode(node, node.methodName, node.methodName))
    val targetAst          = astForExpression(node.target)
    val code               = s"${node.target.getText}${node.op}${node.methodName}"
    val fieldAccess = callNode(node, code, Operators.fieldAccess, Operators.fieldAccess, DispatchTypes.STATIC_DISPATCH)
    callAst(fieldAccess, Seq(targetAst, fieldIdentifierAst))
  }

  private def getBinaryOperatorName(op: String): Option[String]     = BinaryOperatorNames.get(op)
  private def getUnaryOperatorName(op: String): Option[String]      = UnaryOperatorNames.get(op)
  private def getAssignmentOperatorName(op: String): Option[String] = AssignmentOperatorNames.get(op)
}
