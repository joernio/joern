package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}

trait AstForMethodCallExpressionCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForCallExpression(expr: ParserNodeInfo): Seq[Ast] = {

    Seq.empty
  }
}
