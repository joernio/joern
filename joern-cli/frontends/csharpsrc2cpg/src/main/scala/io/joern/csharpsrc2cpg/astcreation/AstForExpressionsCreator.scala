package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.{IdentifierNode, *}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpressionStatement(expr: DotNetNodeInfo): Seq[Ast] = {
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    astForExpression(expressionNode)
  }

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    expr.node match
      case _: UnaryExpr         => astForUnaryExpression(expr)
      case _: BinaryExpr        => astForBinaryExpression(expr)
      case _: LiteralExpr       => astForLiteralExpression(expr)
      case InvocationExpression => astForInvocationExpression(expr)
      case _: IdentifierNode    => astForIdentifier(expr) :: Nil
      case _                    => notHandledYet(expr)
  }

  protected def astForLiteralExpression(_literalNode: DotNetNodeInfo): Seq[Ast] = {
    Seq(Ast(literalNode(_literalNode, code(_literalNode), nodeTypeFullName(_literalNode))))
  }

  protected def astForUnaryExpression(unaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = unaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+" => Operators.plus
      case "-" => Operators.minus
      case "++" =>
        if (unaryExpr.node.getClass == PostIncrementExpression.getClass) Operators.postIncrement
        else Operators.preIncrement
      case "--" =>
        if (unaryExpr.node.getClass == PostDecrementExpression.getClass) Operators.postDecrement
        else Operators.preDecrement
      case "~" => Operators.not
      case "!" => Operators.logicalNot
      case "&" => Operators.addressOf

    val args    = createDotNetNodeInfo(unaryExpr.json(ParserKeys.Operand))
    val argsAst = astForNode(args)
    Seq(
      callAst(createCallNodeForOperator(unaryExpr, operatorName, typeFullName = Some(nodeTypeFullName(args))), argsAst)
    )
  }

  protected def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = binaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+"   => Operators.addition
      case "-"   => Operators.subtraction
      case "*"   => Operators.multiplication
      case "/"   => Operators.division
      case "%"   => Operators.modulo
      case "=="  => Operators.equals
      case "!="  => Operators.notEquals
      case "&&"  => Operators.logicalAnd
      case "||"  => Operators.logicalOr
      case "="   => Operators.assignment
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentModulo
      case "&="  => Operators.assignmentAnd
      case "|="  => Operators.assignmentOr
      case "^="  => Operators.assignmentXor
      case ">>=" => Operators.assignmentLogicalShiftRight
      case "<<=" => Operators.assignmentShiftLeft
      case ">"   => Operators.greaterThan
      case "<"   => Operators.lessThan
      case ">="  => Operators.greaterEqualsThan
      case "<="  => Operators.lessEqualsThan
      case "|"   => Operators.or
      case "&"   => Operators.and
      case "^"   => Operators.xor
      case x =>
        logger.warn(s"Unhandled operator '$x' for ${code(binaryExpr)}")
        CSharpOperators.unknown

    val args = astForNode(binaryExpr.json(ParserKeys.Left)) ++: astForNode(binaryExpr.json(ParserKeys.Right))
    val cNode =
      createCallNodeForOperator(binaryExpr, operatorName, typeFullName = Some(getTypeFullNameFromAstNode(args)))
    Seq(callAst(cNode, args))
  }

  /** Handles the `= ...` part of the equals value clause, thus this only contains an RHS.
    */
  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    astForNode(rhsNode)
  }

  private def astForInvocationExpression(invocationExpr: DotNetNodeInfo): Seq[Ast] = {
    val dispatchType = DispatchTypes.STATIC_DISPATCH // TODO
    val typeFullName = None                          // TODO: Return type of the call
    val arguments    = astForArgumentList(createDotNetNodeInfo(invocationExpr.json(ParserKeys.ArgumentList)))
    val argString =
      s"${arguments.flatMap(_.root).collect { case x: NewMethodParameterIn => x.typeFullName }.mkString(",")}"
    val signature = Option(s"${typeFullName.getOrElse(Defines.Any)}:($argString)")

    val expression = createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression))
    val name       = nameFromNode(expression)

    val (receiver, baseTypeFullName) = expression.node match
      case SimpleMemberAccessExpression =>
        val baseNode = createDotNetNodeInfo(
          createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression)).json(ParserKeys.Expression)
        )
        val baseIdentifier =
          identifierNode(baseNode, nameFromNode(baseNode), code(baseNode), nodeTypeFullName(baseNode))
        (Option(Ast(baseIdentifier)), Option(baseIdentifier.typeFullName))
      case _ => (None, None)

    // TODO: Handle signature
    val methodFullName = baseTypeFullName match
      case Some(typeFullName) => s"$typeFullName.$name"
      case None               => s"${Defines.UnresolvedNamespace}.$name"

    val _callAst = callAst(
      callNode(invocationExpr, code(invocationExpr), name, methodFullName, dispatchType, signature, typeFullName),
      arguments,
      receiver
    )
    Seq(_callAst)
  }

  def astForObjectCreationExpression(objectCreation: DotNetNodeInfo): Seq[Ast] = {
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val typeFullName = nodeTypeFullName(objectCreation)
    val arguments    = astForArgumentList(createDotNetNodeInfo(objectCreation.json(ParserKeys.ArgumentList)))
    // TODO: Handle signature
    val signature      = None
    val name           = Defines.ConstructorMethodName
    val methodFullName = s"$typeFullName.$name"
    val _callNode = callNode(
      objectCreation,
      code(objectCreation),
      name,
      methodFullName,
      dispatchType,
      signature,
      Option(typeFullName)
    )

    Seq(callAst(_callNode, arguments, Option(Ast(thisNode))))
  }

  private def astForArgumentList(argumentList: DotNetNodeInfo): Seq[Ast] = {
    argumentList
      .json(ParserKeys.Arguments)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(astForExpressionStatement)
      .toSeq
  }

}
