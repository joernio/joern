package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.ControlStructureTypes
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators

trait AstForExprSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForArrayExprSyntax(node: ArrayExprSyntax): Ast                   = notHandledYet(node)
  private def astForArrowExprSyntax(node: ArrowExprSyntax): Ast                   = notHandledYet(node)
  private def astForAsExprSyntax(node: AsExprSyntax): Ast                         = notHandledYet(node)
  private def astForAssignmentExprSyntax(node: AssignmentExprSyntax): Ast         = notHandledYet(node)
  private def astForAwaitExprSyntax(node: AwaitExprSyntax): Ast                   = notHandledYet(node)
  private def astForBinaryOperatorExprSyntax(node: BinaryOperatorExprSyntax): Ast = notHandledYet(node)

  private def astForBooleanLiteralExprSyntax(node: BooleanLiteralExprSyntax): Ast = {
    astForNode(node.literal)
  }

  private def astForBorrowExprSyntax(node: BorrowExprSyntax): Ast                     = notHandledYet(node)
  private def astForCanImportExprSyntax(node: CanImportExprSyntax): Ast               = notHandledYet(node)
  private def astForCanImportVersionInfoSyntax(node: CanImportVersionInfoSyntax): Ast = notHandledYet(node)
  private def astForClosureExprSyntax(node: ClosureExprSyntax): Ast                   = notHandledYet(node)
  private def astForConsumeExprSyntax(node: ConsumeExprSyntax): Ast                   = notHandledYet(node)
  private def astForCopyExprSyntax(node: CopyExprSyntax): Ast                         = notHandledYet(node)
  private def astForDeclReferenceExprSyntax(node: DeclReferenceExprSyntax): Ast = {
    val name      = code(node)
    val identNode = identifierNode(node, name)
    scope.addVariableReference(name, identNode)
    Ast(identNode)
  }
  private def astForDictionaryExprSyntax(node: DictionaryExprSyntax): Ast                       = notHandledYet(node)
  private def astForDiscardAssignmentExprSyntax(node: DiscardAssignmentExprSyntax): Ast         = notHandledYet(node)
  private def astForDoExprSyntax(node: DoExprSyntax): Ast                                       = notHandledYet(node)
  private def astForEditorPlaceholderExprSyntax(node: EditorPlaceholderExprSyntax): Ast         = notHandledYet(node)
  private def astForFloatLiteralExprSyntax(node: FloatLiteralExprSyntax): Ast                   = notHandledYet(node)
  private def astForForceUnwrapExprSyntax(node: ForceUnwrapExprSyntax): Ast                     = notHandledYet(node)
  private def astForFunctionCallExprSyntax(node: FunctionCallExprSyntax): Ast                   = notHandledYet(node)
  private def astForGenericSpecializationExprSyntax(node: GenericSpecializationExprSyntax): Ast = notHandledYet(node)

  private def astForIfExprSyntax(node: IfExprSyntax): Ast = {
    val code         = this.code(node)
    val ifNode       = controlStructureNode(node, ControlStructureTypes.IF, code)
    val conditionAst = astForNode(node.conditions)
    val thenAst      = astForNode(node.body)
    val elseAst = node.elseBody match {
      case Some(value) => astForNode(value)
      case None        => Ast()
    }
    controlStructureAst(ifNode, Some(conditionAst), Seq(thenAst, elseAst))
  }

  private def astForInOutExprSyntax(node: InOutExprSyntax): Ast = notHandledYet(node)
  private def astForInfixOperatorExprSyntax(node: InfixOperatorExprSyntax): Ast = {
    val op = code(node.operator) match {
      case "="    => Operators.assignment
      case "+="   => Operators.assignmentPlus
      case "-="   => Operators.assignmentMinus
      case "*="   => Operators.assignmentMultiplication
      case "/="   => Operators.assignmentDivision
      case "%="   => Operators.assignmentModulo
      case "**="  => Operators.assignmentExponentiation
      case "&="   => Operators.assignmentAnd
      case "&&="  => Operators.assignmentAnd
      case "|="   => Operators.assignmentOr
      case "||="  => Operators.assignmentOr
      case "^="   => Operators.assignmentXor
      case "<<="  => Operators.assignmentShiftLeft
      case ">>="  => Operators.assignmentArithmeticShiftRight
      case ">>>=" => Operators.assignmentLogicalShiftRight
      case "??="  => Operators.notNullAssert
      case "<="   => Operators.lessEqualsThan
      case ">="   => Operators.greaterEqualsThan
      case "<"    => Operators.lessThan
      case ">"    => Operators.greaterThan
      case "=="   => Operators.equals
      case "+"    => Operators.plus
      case "-"    => Operators.minus
      case "/"    => Operators.division
      case "*"    => Operators.multiplication
      case other =>
        logger.warn(s"Unknown assignment operator: '$other'")
        Operators.assignment
    }

    val lhsAst    = astForNode(node.leftOperand)
    val rhsAst    = astForNodeWithFunctionReference(node.rightOperand)
    val callNode_ = callNode(node, code(node), op, DispatchTypes.STATIC_DISPATCH)
    val argAsts   = List(lhsAst, rhsAst)
    callAst(callNode_, argAsts)
  }

  private def astForIntegerLiteralExprSyntax(node: IntegerLiteralExprSyntax): Ast = {
    astForNode(node.literal)
  }

  private def astForIsExprSyntax(node: IsExprSyntax): Ast                                   = notHandledYet(node)
  private def astForKeyPathExprSyntax(node: KeyPathExprSyntax): Ast                         = notHandledYet(node)
  private def astForMacroExpansionExprSyntax(node: MacroExpansionExprSyntax): Ast           = notHandledYet(node)
  private def astForMemberAccessExprSyntax(node: MemberAccessExprSyntax): Ast               = notHandledYet(node)
  private def astForMissingExprSyntax(node: MissingExprSyntax): Ast                         = notHandledYet(node)
  private def astForNilLiteralExprSyntax(node: NilLiteralExprSyntax): Ast                   = notHandledYet(node)
  private def astForOptionalChainingExprSyntax(node: OptionalChainingExprSyntax): Ast       = notHandledYet(node)
  private def astForPackElementExprSyntax(node: PackElementExprSyntax): Ast                 = notHandledYet(node)
  private def astForPackExpansionExprSyntax(node: PackExpansionExprSyntax): Ast             = notHandledYet(node)
  private def astForPatternExprSyntax(node: PatternExprSyntax): Ast                         = notHandledYet(node)
  private def astForPostfixIfConfigExprSyntax(node: PostfixIfConfigExprSyntax): Ast         = notHandledYet(node)
  private def astForPostfixOperatorExprSyntax(node: PostfixOperatorExprSyntax): Ast         = notHandledYet(node)
  private def astForPrefixOperatorExprSyntax(node: PrefixOperatorExprSyntax): Ast           = notHandledYet(node)
  private def astForRegexLiteralExprSyntax(node: RegexLiteralExprSyntax): Ast               = notHandledYet(node)
  private def astForSequenceExprSyntax(node: SequenceExprSyntax): Ast                       = notHandledYet(node)
  private def astForSimpleStringLiteralExprSyntax(node: SimpleStringLiteralExprSyntax): Ast = notHandledYet(node)
  private def astForStringLiteralExprSyntax(node: StringLiteralExprSyntax): Ast             = notHandledYet(node)
  private def astForSubscriptCallExprSyntax(node: SubscriptCallExprSyntax): Ast             = notHandledYet(node)
  private def astForSuperExprSyntax(node: SuperExprSyntax): Ast                             = notHandledYet(node)
  private def astForSwitchExprSyntax(node: SwitchExprSyntax): Ast                           = notHandledYet(node)

  private def astForTernaryExprSyntax(node: TernaryExprSyntax): Ast = {
    val name = Operators.conditional
    val call = callNode(node, code(node), name, name, DispatchTypes.STATIC_DISPATCH)

    val condAst = astForNodeWithFunctionReference(node.condition)
    val posAst  = astForNodeWithFunctionReference(node.thenExpression)
    val negAst  = astForNodeWithFunctionReference(node.elseExpression)

    val children = List(condAst, posAst, negAst)
    callAst(call, children)
  }

  private def astForTryExprSyntax(node: TryExprSyntax): Ast = notHandledYet(node)

  private def astForTupleExprSyntax(node: TupleExprSyntax): Ast = {
    astForNode(node.elements)
  }

  private def astForTypeExprSyntax(node: TypeExprSyntax): Ast                           = notHandledYet(node)
  private def astForUnresolvedAsExprSyntax(node: UnresolvedAsExprSyntax): Ast           = notHandledYet(node)
  private def astForUnresolvedIsExprSyntax(node: UnresolvedIsExprSyntax): Ast           = notHandledYet(node)
  private def astForUnresolvedTernaryExprSyntax(node: UnresolvedTernaryExprSyntax): Ast = notHandledYet(node)

  protected def astForExprSyntax(exprSyntax: ExprSyntax): Ast = exprSyntax match {
    case node: ArrayExprSyntax                 => astForArrayExprSyntax(node)
    case node: ArrowExprSyntax                 => astForArrowExprSyntax(node)
    case node: AsExprSyntax                    => astForAsExprSyntax(node)
    case node: AssignmentExprSyntax            => astForAssignmentExprSyntax(node)
    case node: AwaitExprSyntax                 => astForAwaitExprSyntax(node)
    case node: BinaryOperatorExprSyntax        => astForBinaryOperatorExprSyntax(node)
    case node: BooleanLiteralExprSyntax        => astForBooleanLiteralExprSyntax(node)
    case node: BorrowExprSyntax                => astForBorrowExprSyntax(node)
    case node: CanImportExprSyntax             => astForCanImportExprSyntax(node)
    case node: CanImportVersionInfoSyntax      => astForCanImportVersionInfoSyntax(node)
    case node: ClosureExprSyntax               => astForClosureExprSyntax(node)
    case node: ConsumeExprSyntax               => astForConsumeExprSyntax(node)
    case node: CopyExprSyntax                  => astForCopyExprSyntax(node)
    case node: DeclReferenceExprSyntax         => astForDeclReferenceExprSyntax(node)
    case node: DictionaryExprSyntax            => astForDictionaryExprSyntax(node)
    case node: DiscardAssignmentExprSyntax     => astForDiscardAssignmentExprSyntax(node)
    case node: DoExprSyntax                    => astForDoExprSyntax(node)
    case node: EditorPlaceholderExprSyntax     => astForEditorPlaceholderExprSyntax(node)
    case node: FloatLiteralExprSyntax          => astForFloatLiteralExprSyntax(node)
    case node: ForceUnwrapExprSyntax           => astForForceUnwrapExprSyntax(node)
    case node: FunctionCallExprSyntax          => astForFunctionCallExprSyntax(node)
    case node: GenericSpecializationExprSyntax => astForGenericSpecializationExprSyntax(node)
    case node: IfExprSyntax                    => astForIfExprSyntax(node)
    case node: InOutExprSyntax                 => astForInOutExprSyntax(node)
    case node: InfixOperatorExprSyntax         => astForInfixOperatorExprSyntax(node)
    case node: IntegerLiteralExprSyntax        => astForIntegerLiteralExprSyntax(node)
    case node: IsExprSyntax                    => astForIsExprSyntax(node)
    case node: KeyPathExprSyntax               => astForKeyPathExprSyntax(node)
    case node: MacroExpansionExprSyntax        => astForMacroExpansionExprSyntax(node)
    case node: MemberAccessExprSyntax          => astForMemberAccessExprSyntax(node)
    case node: MissingExprSyntax               => astForMissingExprSyntax(node)
    case node: NilLiteralExprSyntax            => astForNilLiteralExprSyntax(node)
    case node: OptionalChainingExprSyntax      => astForOptionalChainingExprSyntax(node)
    case node: PackElementExprSyntax           => astForPackElementExprSyntax(node)
    case node: PackExpansionExprSyntax         => astForPackExpansionExprSyntax(node)
    case node: PatternExprSyntax               => astForPatternExprSyntax(node)
    case node: PostfixIfConfigExprSyntax       => astForPostfixIfConfigExprSyntax(node)
    case node: PostfixOperatorExprSyntax       => astForPostfixOperatorExprSyntax(node)
    case node: PrefixOperatorExprSyntax        => astForPrefixOperatorExprSyntax(node)
    case node: RegexLiteralExprSyntax          => astForRegexLiteralExprSyntax(node)
    case node: SequenceExprSyntax              => astForSequenceExprSyntax(node)
    case node: SimpleStringLiteralExprSyntax   => astForSimpleStringLiteralExprSyntax(node)
    case node: StringLiteralExprSyntax         => astForStringLiteralExprSyntax(node)
    case node: SubscriptCallExprSyntax         => astForSubscriptCallExprSyntax(node)
    case node: SuperExprSyntax                 => astForSuperExprSyntax(node)
    case node: SwitchExprSyntax                => astForSwitchExprSyntax(node)
    case node: TernaryExprSyntax               => astForTernaryExprSyntax(node)
    case node: TryExprSyntax                   => astForTryExprSyntax(node)
    case node: TupleExprSyntax                 => astForTupleExprSyntax(node)
    case node: TypeExprSyntax                  => astForTypeExprSyntax(node)
    case node: UnresolvedAsExprSyntax          => astForUnresolvedAsExprSyntax(node)
    case node: UnresolvedIsExprSyntax          => astForUnresolvedIsExprSyntax(node)
    case node: UnresolvedTernaryExprSyntax     => astForUnresolvedTernaryExprSyntax(node)
  }
}
