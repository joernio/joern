package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.BlockScope
import io.joern.swiftsrc2cpg.datastructures.MethodScope
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators

import scala.annotation.tailrec

trait AstForPatternSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForExpressionPatternSyntax(node: ExpressionPatternSyntax): Ast = {
    astForNodeWithFunctionReference(node.expression)
  }

  private def astForIdentifierPatternSyntax(node: IdentifierPatternSyntax): Ast = {
    val name      = code(node.identifier)
    val identNode = identifierNode(node, name)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }

  private def astForIsTypePatternSyntax(node: IsTypePatternSyntax): Ast = {
    val op      = Operators.instanceOf
    val lhsNode = node.`type`
    val typ     = code(lhsNode)
    registerType(typ)
    val lhsAst    = Ast(literalNode(lhsNode, code(lhsNode), None).dynamicTypeHintFullName(Seq(typ)))
    val callNode_ = callNode(node, code(node), op, DispatchTypes.STATIC_DISPATCH).dynamicTypeHintFullName(Seq(typ))
    callAst(callNode_, Seq(lhsAst))
  }

  private def astForMissingPatternSyntax(node: MissingPatternSyntax): Ast = notHandledYet(node)
  private def astForTuplePatternSyntax(node: TuplePatternSyntax): Ast     = notHandledYet(node)

  private def localForValueBindingPatternSyntax(
    node: ValueBindingPatternSyntax,
    name: String,
    typeFullName: String
  ): Ast = {
    val kind = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") {
      BlockScope
    } else {
      MethodScope
    }
    val nLocalNode = localNode(node, name, name, typeFullName).order(0)
    registerType(typeFullName)
    scope.addVariable(name, nLocalNode, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    Ast()
  }

  @tailrec
  private def astForValueBindingPatternSyntax(node: ValueBindingPatternSyntax): Ast = {
    node.pattern match {
      case expr: ExpressionPatternSyntax if expr.expression.isInstanceOf[AsExprSyntax] =>
        val asExpr = expr.expression.asInstanceOf[AsExprSyntax]
        localForValueBindingPatternSyntax(node, code(asExpr.expression), code(asExpr.`type`))
      case expr: ExpressionPatternSyntax =>
        astForNode(expr)
      case ident: IdentifierPatternSyntax =>
        localForValueBindingPatternSyntax(node, code(ident.identifier), Defines.Any)
      case isType: IsTypePatternSyntax =>
        astForNode(isType)
      case _: MissingPatternSyntax => Ast()
      case tuple: TuplePatternSyntax =>
        astForNode(tuple)
      case valueBinding: ValueBindingPatternSyntax =>
        astForValueBindingPatternSyntax(valueBinding)
      case _: WildcardPatternSyntax => Ast()
    }
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
