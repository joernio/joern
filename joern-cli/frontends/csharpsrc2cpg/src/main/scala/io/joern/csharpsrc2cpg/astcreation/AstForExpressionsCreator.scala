package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewMethodParameterIn}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    expressionNode.node match
      case _: UnaryExpr         => astForUnaryExpression(expressionNode)
      case _: BinaryExpr        => astForBinaryExpression(expressionNode)
      case _: LiteralExpr       => astForLiteralExpression(expressionNode)
      case InvocationExpression => astForInvocationExpression(expressionNode)
      case _                    => notHandledYet(expressionNode)
  }

  protected def astForLiteralExpression(_literalNode: DotNetNodeInfo): Seq[Ast] = {
    Seq(Ast(literalNode(_literalNode, code(_literalNode), nodeTypeFullName(_literalNode))))
  }

  private def astForUnaryExpression(unaryExpr: DotNetNodeInfo): Seq[Ast] = {
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
    ) // TODO: typeFullName
  }
  private def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
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

    val args = astForNode(binaryExpr.json(ParserKeys.Left)) ++: astForNode(binaryExpr.json(ParserKeys.Right))
    val cNode =
      createCallNodeForOperator(binaryExpr, operatorName, typeFullName = Some(getTypeFullNameFromAstNode(args)))
    Seq(callAst(cNode, args))
  }

  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    rhsNode.node match
      case _: LiteralExpr => astForLiteralExpression(rhsNode)
      case _              => notHandledYet(rhsNode)
  }

  private def astForInvocationExpression(invocationExpr: DotNetNodeInfo): Seq[Ast] = {
    val dispatchType = DispatchTypes.STATIC_DISPATCH // TODO
    val typeFullName = None                          // TODO
    val arguments    = astForArgumentList(createDotNetNodeInfo(invocationExpr.json(ParserKeys.ArgumentList)))
    val signature = Option(
      s"${typeFullName
          .getOrElse("ANY")}:(${arguments.flatMap(_.root).collect { case x: NewMethodParameterIn => x.typeFullName }.mkString(",")})"
    )

    val expression = createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression))
    val name       = nameFromNode(createDotNetNodeInfo(expression.json(ParserKeys.Name)))

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

  private def astForArgumentList(argumentList: DotNetNodeInfo): Seq[Ast] = {
    argumentList
      .json(ParserKeys.Arguments)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(astForExpression)
      .toSeq
  }

}
