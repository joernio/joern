package io.joern.abap2cpg.astcreation.expressions

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.joern.x2cpg.Ast

/** Main expression dispatcher that delegates to specialized traits */
trait AstForExpressionsCreator extends AstForSimpleExpressionsCreator with AstForCallsCreator {
  this: AstCreator =>

  /** Create an expression AST - dispatcher for all expression types */
  protected def astForExpression(expr: AbapNode, argIndex: Int, className: Option[String] = None): Ast = {
    expr match {
      case ident: IdentifierExpr =>
        astForIdentifier(ident, argIndex)

      case lit: LiteralExpr =>
        astForLiteral(lit, argIndex)

      case callExpr: CallExpr =>
        astForCall(callExpr, argIndex, className)

      case opCall: OperatorCall =>
        astForOperatorCall(opCall, argIndex, className)

      case fieldAccess: FieldAccessExpr =>
        astForFieldAccess(fieldAccess, argIndex)

      case _ =>
        astForUnknown(argIndex)
    }
  }
}
