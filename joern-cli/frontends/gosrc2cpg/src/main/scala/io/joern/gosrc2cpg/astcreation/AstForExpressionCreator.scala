package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst._
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}

trait AstForExpressionCreator { this: AstCreator =>
  def astForExpression(expr: ParserNodeInfo): Ast = {
    expr.node match {
      case BinaryExpr => astForBinaryExpr(expr)
      case StarExpr   => astForStarExpr(expr)
      case UnaryExpr  => astForUnaryExpr(expr)
      case _          => Ast()
    }
  }

  private def astForBinaryExpr(binaryExpr: ParserNodeInfo): Ast = {
    val op = binaryExpr.json(ParserKeys.Op).value match {
      case "*"  => Operators.multiplication
      case "/"  => Operators.division
      case "%"  => Operators.modulo
      case "+"  => Operators.addition
      case "-"  => Operators.subtraction
      case "<<" => Operators.shiftLeft
      case ">>" => Operators.arithmeticShiftRight
      case "<"  => Operators.lessThan
      case ">"  => Operators.greaterThan
      case "<=" => Operators.lessEqualsThan
      case ">=" => Operators.greaterEqualsThan
      case "&"  => Operators.and
      case "^"  => Operators.xor
      case "|"  => Operators.or
      case "&&" => Operators.logicalAnd
      case "||" => Operators.logicalOr
      case "==" => Operators.equals
      case "!=" => Operators.notEquals
      case _    => Operator.unknown
    }
    val cNode     = createCallNodeForOperator(binaryExpr, op)
    val arguments = Seq(astForNode(binaryExpr.json(ParserKeys.X)), astForNode(binaryExpr.json(ParserKeys.Y)))
    callAst(cNode, arguments)
  }

  private def astForStarExpr(starExpr: ParserNodeInfo): Ast = {
    val cNode   = createCallNodeForOperator(starExpr, Operators.indirection)
    val operand = astForNode(starExpr.json(ParserKeys.X))
    callAst(cNode, Seq(operand))
  }
  private def astForUnaryExpr(unaryExpr: ParserNodeInfo): Ast = {
    val operatorMethod = unaryExpr.json(ParserKeys.Op).value match {
      case "+" => Operators.plus
      case "-" => Operators.minus
      case "*" => Operators.indirection
      case "&" => Operators.addressOf
      case "!" => Operators.logicalNot
      case _   => Operator.unknown
    }

    val cNode   = createCallNodeForOperator(unaryExpr, operatorMethod)
    val operand = astForNode(unaryExpr.json(ParserKeys.X))
    callAst(cNode, Seq(operand))
  }

  private def createCallNodeForOperator(
    node: ParserNodeInfo,
    operatorMethod: String,
    DispatchType: String = DispatchTypes.STATIC_DISPATCH
  ): NewCall = {
    callNode(node, node.code, operatorMethod, operatorMethod, DispatchType)
  }
}
