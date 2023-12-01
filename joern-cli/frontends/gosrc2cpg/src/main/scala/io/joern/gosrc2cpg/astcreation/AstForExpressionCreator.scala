package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.Operator
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators, PropertyNames}
import ujson.Value

import scala.collection.immutable.Seq
trait AstForExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  def astsForExpression(expr: ParserNodeInfo): Seq[Ast] = {
    expr.node match {
      case BinaryExpr     => astForBinaryExpr(expr)
      case StarExpr       => astForStarExpr(expr)
      case UnaryExpr      => astForUnaryExpr(expr)
      case ParenExpr      => astsForExpression(createParserNodeInfo(expr.json(ParserKeys.X)))
      case TypeAssertExpr => astForNode(expr.json(ParserKeys.X))
      case CallExpr       => astForCallExpression(expr)
      case SelectorExpr   => astForFieldAccess(expr)
      case KeyValueExpr   => astForNode(createParserNodeInfo(expr.json(ParserKeys.Value)))
      case IndexExpr      => astForIndexExpression(expr)
      case _              => Seq(Ast())
    }
  }

  private def astForBinaryExpr(binaryExpr: ParserNodeInfo): Seq[Ast] = {
    val arguments = astForNode(binaryExpr.json(ParserKeys.X)) ++: astForNode(binaryExpr.json(ParserKeys.Y))
    // Randomly taking first element of the LHS.
    // TODO: We need to create proper unit tests for corner cases to be handled separately
    var typeFullName = getTypeFullNameFromAstNode(arguments)
    val op = binaryExpr.json(ParserKeys.Op).value match {
      case "*"  => Operators.multiplication
      case "/"  => Operators.division
      case "%"  => Operators.modulo
      case "+"  => Operators.addition
      case "-"  => Operators.subtraction
      case "<<" => Operators.shiftLeft
      case ">>" => Operators.arithmeticShiftRight
      case "<" =>
        typeFullName = Defines.Bool
        Operators.lessThan
      case ">" =>
        typeFullName = Defines.Bool
        Operators.greaterThan
      case "<=" =>
        typeFullName = Defines.Bool
        Operators.lessEqualsThan
      case ">=" =>
        typeFullName = Defines.Bool
        Operators.greaterEqualsThan
      case "&" => Operators.and
      case "^" => Operators.xor
      case "|" => Operators.or
      case "&&" =>
        typeFullName = Defines.Bool
        Operators.logicalAnd
      case "||" =>
        typeFullName = Defines.Bool
        Operators.logicalOr
      case "==" =>
        typeFullName = Defines.Bool
        Operators.equals
      case "!=" =>
        typeFullName = Defines.Bool
        Operators.notEquals
      case _ => Operator.unknown
    }
    val cNode = createCallNodeForOperator(binaryExpr, op, typeFullName = Some(typeFullName))

    Seq(callAst(cNode, arguments))
  }

  private def astForStarExpr(starExpr: ParserNodeInfo): Seq[Ast] = {
    val operand      = astForNode(starExpr.json(ParserKeys.X))
    val typeFullName = getTypeFullNameFromAstNode(operand)
    val cNode        = createCallNodeForOperator(starExpr, Operators.indirection, typeFullName = Some(typeFullName))
    Seq(callAst(cNode, operand))
  }

  private def astForUnaryExpr(unaryExpr: ParserNodeInfo): Seq[Ast] = {
    val operand      = astForNode(unaryExpr.json(ParserKeys.X))
    var typeFullName = getTypeFullNameFromAstNode(operand)
    val operatorMethod = unaryExpr.json(ParserKeys.Op).value match {
      case "+" => Operators.plus
      case "-" => Operators.minus
      case "*" =>
        typeFullName = typeFullName.stripPrefix("*")
        Operators.indirection
      case "&" =>
        typeFullName = "*" + typeFullName
        Operators.addressOf
      case "!" =>
        typeFullName = Defines.Bool
        Operators.logicalNot
      case "range" => Operators.range
      case _       => Operator.unknown
    }
    val cNode = createCallNodeForOperator(unaryExpr, operatorMethod, typeFullName = Some(typeFullName))
    Seq(callAst(cNode, operand))
  }

  private def astForIndexExpression(indexNode: ParserNodeInfo): Seq[Ast] = {
    val indexAst                                = astForNode(indexNode.json(ParserKeys.Index))
    val (indexIdentifier, callNodeTypeFullName) = processIndexIdentifier(indexNode.json(ParserKeys.X))
    val callNode =
      createCallNodeForOperator(indexNode, Operators.indexAccess, typeFullName = Some(callNodeTypeFullName))
    Seq(callAst(callNode, indexIdentifier ++ indexAst))
  }

  private def processIndexIdentifier(identNode: Value): (Seq[Ast], String) = {
    val identifierAst = astForNode(identNode)
    val identifierTypeFullName = getTypeFullNameFromAstNode(identifierAst)
      .stripPrefix("*")
      .stripPrefix("[]")
    (identifierAst, identifierTypeFullName)
  }
//
//  private def extractBaseType(input: String): String = {
//    if (input.matches("""(\[\])*(\w|\.)+""")) {
//      input.substring(input.lastIndexOf("]") + 1, input.length)
//    } else if (input.matches("""map\[(.*?)\]""")) {
//      input.stripPrefix("map[").stripSuffix("]")
//    } else {
//      Defines.anyTypeName
//    }
//  }

  private def createCallNodeForOperator(
    node: ParserNodeInfo,
    operatorMethod: String,
    DispatchType: String = DispatchTypes.STATIC_DISPATCH,
    signature: Option[String] = None,
    typeFullName: Option[String] = None
  ): NewCall = {
    callNode(node, node.code, operatorMethod, operatorMethod, DispatchType, signature, typeFullName)
  }
}
