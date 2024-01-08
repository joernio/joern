package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    // TODO: Handle identifiers in operators
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    expressionNode.node match
      case _: UnaryExpr  => astForUnaryExpression(expressionNode)
      case _: BinaryExpr => astForBinaryExpression(expressionNode)
      case _             => notHandledYet(expressionNode)
  }

  private def astForUnaryExpression(unaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = unaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).toString.replaceAll("\"", "")
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

    Seq(callAst(createCallNodeForOperator(unaryExpr, operatorName, typeFullName = Some("")))) // TODO: typeFullName
  }
  private def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = binaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).toString.replaceAll("\"", "")
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
      createCallNodeForOperator(binaryExpr, operatorName, typeFullName = Some("")) // TODO: Resolve typeFullName
    Seq(callAst(cNode, args))
  }

  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    rhsNode.node match
      case _: LiteralExpr => Seq(Ast(literalNode(rhsNode, code(rhsNode), nodeTypeFullName(rhsNode))))
      case _              => notHandledYet(rhsNode)
  }

  private def createCallNodeForOperator(
    node: DotNetNodeInfo,
    operatorMethod: String,
    DispatchType: String = DispatchTypes.STATIC_DISPATCH,
    signature: Option[String] = None,
    typeFullName: Option[String] = None
  ): NewCall = {
    callNode(node, node.code, operatorMethod, operatorMethod, DispatchType, signature, typeFullName)
  }

}
