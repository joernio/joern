package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.{EdgeTypes, Operators}

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
    val op = Operators.instanceOf

    val tpeNode = node.`type`
    val tpe     = simpleTypeNameForTypeSyntax(tpeNode)
    registerType(tpe)

    val callNode_    = createStaticCallNode(node, code(node), op, op, Defines.Bool)
    val typeRefNode_ = typeRefNode(node, code(tpeNode), tpe)

    val argAsts = List(Ast(typeRefNode_))
    callAst(callNode_, argAsts)
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
        val asExpr         = expr.expression.asInstanceOf[AsExprSyntax]
        val tpeFromTypeMap = fullnameProvider.typeFullname(expr.expression)
        localForValueBindingPatternSyntax(
          node,
          code(asExpr.expression),
          tpeFromTypeMap.getOrElse(AstCreatorHelper.cleanType(code(asExpr.`type`)))
        )
      case expr: ExpressionPatternSyntax =>
        astForNode(expr)
      case ident: IdentifierPatternSyntax =>
        val tpeFromTypeMap = fullnameProvider.typeFullname(ident.identifier)
        localForValueBindingPatternSyntax(node, code(ident.identifier), tpeFromTypeMap.getOrElse(Defines.Any))
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
