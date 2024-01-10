package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.BlockScope
import io.joern.swiftsrc2cpg.datastructures.MethodScope
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.EdgeTypes

trait AstForPatternSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForExpressionPatternSyntax(node: ExpressionPatternSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForIdentifierPatternSyntax(node: IdentifierPatternSyntax): Ast = {
    val name      = code(node.identifier)
    val identNode = identifierNode(node, name)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  private def astForIsTypePatternSyntax(node: IsTypePatternSyntax): Ast   = notHandledYet(node)
  private def astForMissingPatternSyntax(node: MissingPatternSyntax): Ast = notHandledYet(node)
  private def astForTuplePatternSyntax(node: TuplePatternSyntax): Ast     = notHandledYet(node)

  private def astForValueBindingPatternSyntax(node: ValueBindingPatternSyntax): Ast = {
    val kind = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") {
      BlockScope
    } else {
      MethodScope
    }

    val name = node.pattern match {
      case expr: ExpressionPatternSyntax =>
        notHandledYet(expr)
        code(expr)
      case ident: IdentifierPatternSyntax =>
        code(ident.identifier)
      case isType: IsTypePatternSyntax =>
        notHandledYet(isType)
        code(isType)
      case missing: MissingPatternSyntax =>
        code(missing.placeholder)
      case tuple: TuplePatternSyntax =>
        notHandledYet(tuple)
        code(tuple)
      case valueBinding: ValueBindingPatternSyntax =>
        notHandledYet(valueBinding)
        code(valueBinding)
      case wildcard: WildcardPatternSyntax =>
        notHandledYet(wildcard)
        generateUnusedVariableName(usedVariableNames, "wildcard")
    }
    val typeFullName = Defines.Any
    val nLocalNode   = localNode(node, name, name, typeFullName).order(0)
    scope.addVariable(name, nLocalNode, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    Ast()
  }

  private def astForWildcardPatternSyntax(node: WildcardPatternSyntax): Ast = notHandledYet(node)

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
