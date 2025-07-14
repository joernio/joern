package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

import scala.annotation.{tailrec, unused}

trait AstForPatternSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForExpressionPatternSyntax(node: ExpressionPatternSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForIdentifierPatternSyntax(node: IdentifierPatternSyntax): Ast = {
    astForIdentifier(node)
  }

  private def astForIsTypePatternSyntax(node: IsTypePatternSyntax): Ast = {
    val op      = Operators.instanceOf
    val tpeNode = node.`type`
    val tpeCode = code(tpeNode)
    val tpe     = cleanType(tpeCode)
    registerType(tpe)
    val callNode_    = callNode(node, code(node), op, op, DispatchTypes.STATIC_DISPATCH, None, Some(tpe))
    val typeRefNode_ = typeRefNode(tpeNode, tpeCode, tpe)
    val arg          = Ast(typeRefNode_)
    callAst(callNode_, List(arg))
  }

  private def astForMissingPatternSyntax(@unused node: MissingPatternSyntax): Ast = Ast()

  private def astForTuplePatternSyntax(node: TuplePatternSyntax): Ast = notHandledYet(node)

  private def localForValueBindingPatternSyntax(
    node: ValueBindingPatternSyntax,
    name: String,
    typeFullName: String
  ): Ast = {
    val kind = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") { VariableScopeManager.ScopeType.BlockScope }
    else { VariableScopeManager.ScopeType.MethodScope }
    val nLocalNode = localNode(node, name, name, typeFullName).order(0)
    registerType(typeFullName)
    scope.addVariable(name, nLocalNode, typeFullName, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    Ast()
  }

  @tailrec
  private def astForValueBindingPatternSyntax(node: ValueBindingPatternSyntax): Ast = {
    node.pattern match {
      case expr: ExpressionPatternSyntax if expr.expression.isInstanceOf[AsExprSyntax] =>
        val asExpr = expr.expression.asInstanceOf[AsExprSyntax]
        localForValueBindingPatternSyntax(node, code(asExpr.expression), cleanType(code(asExpr.`type`)))
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
