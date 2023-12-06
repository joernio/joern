package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode

trait AstForPatternSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForExpressionPatternSyntax(node: ExpressionPatternSyntax): Ast = notHandledYet(node)

  private def astForIdentifierPatternSyntax(node: IdentifierPatternSyntax): Ast = {
    val name      = code(node.identifier)
    val identNode = identifierNode(node, name)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  private def astForIsTypePatternSyntax(node: IsTypePatternSyntax): Ast             = notHandledYet(node)
  private def astForMissingPatternSyntax(node: MissingPatternSyntax): Ast           = notHandledYet(node)
  private def astForTuplePatternSyntax(node: TuplePatternSyntax): Ast               = notHandledYet(node)
  private def astForValueBindingPatternSyntax(node: ValueBindingPatternSyntax): Ast = notHandledYet(node)
  private def astForWildcardPatternSyntax(node: WildcardPatternSyntax): Ast         = notHandledYet(node)

  protected def astForPatternSyntax(patternSyntax: PatternSyntax): Ast = patternSyntax match {
    case node: ExpressionPatternSyntax   => astForExpressionPatternSyntax(node)
    case node: IdentifierPatternSyntax   => astForIdentifierPatternSyntax(node)
    case node: IsTypePatternSyntax       => astForIsTypePatternSyntax(node)
    case node: MissingPatternSyntax      => astForMissingPatternSyntax(node)
    case node: TuplePatternSyntax        => astForTuplePatternSyntax(node)
    case node: ValueBindingPatternSyntax => astForValueBindingPatternSyntax(node)
    case node: WildcardPatternSyntax     => astForWildcardPatternSyntax(node)
  }
}
