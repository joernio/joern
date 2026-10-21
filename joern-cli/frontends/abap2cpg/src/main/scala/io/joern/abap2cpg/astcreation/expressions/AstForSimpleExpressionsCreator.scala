package io.joern.abap2cpg.astcreation.expressions

import io.joern.abap2cpg.parser.AbapIntermediateAst.*
import io.joern.abap2cpg.passes.AstCreator
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.nodes.*

/** Methods for creating simple expression nodes (identifiers, literals) */
trait AstForSimpleExpressionsCreator { this: AstCreator =>

  /** Create an identifier expression AST */
  protected def astForIdentifier(ident: IdentifierExpr, argIndex: Int): Ast = {
    val identNode = NewIdentifier()
      .name(ident.name)
      .code(ident.name)
      .typeFullName("ANY")
      .argumentIndex(argIndex)
      .order(argIndex)

    ident.span.start.foreach { pos =>
      identNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    scope.addVariableReference(ident.name, identNode, "ANY", EvaluationStrategies.BY_REFERENCE)
    Ast(identNode)
  }

  /** Create a literal expression AST */
  protected def astForLiteral(lit: LiteralExpr, argIndex: Int): Ast = {
    val litNode = NewLiteral()
      .code(lit.value)
      .typeFullName(lit.literalType)
      .argumentIndex(argIndex)
      .order(argIndex)

    lit.span.start.foreach { pos =>
      litNode.lineNumber(pos.row).columnNumber(pos.col)
    }

    Ast(litNode)
  }

  /** Create a fallback AST for unknown expression types */
  protected def astForUnknown(argIndex: Int): Ast = {
    val unknownNode = NewIdentifier()
      .name("UNKNOWN")
      .code("UNKNOWN")
      .typeFullName("ANY")
      .argumentIndex(argIndex)
      .order(argIndex)
    Ast(unknownNode)
  }
}
