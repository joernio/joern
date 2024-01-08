package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*

trait AstForSyntaxCollectionCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForListSyntaxChildren(node: SwiftNode, children: Seq[SwiftNode]): Ast = {
    children.toList match {
      case Nil         => Ast()
      case head :: Nil => astForNodeWithFunctionReference(head)
      case elements =>
        val blockNode_ = blockNode(node, "<empty>", Defines.Any)
        scope.pushNewBlockScope(blockNode_)
        localAstParentStack.push(blockNode_)
        val childrenAsts = astsForBlockElements(elements)
        localAstParentStack.pop()
        scope.popScope()
        blockAst(blockNode_, childrenAsts)

    }
  }

  private def astForAccessorDeclListSyntax(node: AccessorDeclListSyntax): Ast                 = notHandledYet(node)
  private def astForArrayElementListSyntax(node: ArrayElementListSyntax): Ast                 = notHandledYet(node)
  private def astForAttributeListSyntax(node: AttributeListSyntax): Ast                       = notHandledYet(node)
  private def astForAvailabilityArgumentListSyntax(node: AvailabilityArgumentListSyntax): Ast = notHandledYet(node)
  private def astForCatchClauseListSyntax(node: CatchClauseListSyntax): Ast                   = notHandledYet(node)
  private def astForCatchItemListSyntax(node: CatchItemListSyntax): Ast                       = notHandledYet(node)
  private def astForClosureCaptureListSyntax(node: ClosureCaptureListSyntax): Ast             = notHandledYet(node)
  private def astForClosureParameterListSyntax(node: ClosureParameterListSyntax): Ast         = notHandledYet(node)
  private def astForClosureShorthandParameterListSyntax(node: ClosureShorthandParameterListSyntax): Ast = notHandledYet(
    node
  )

  private def astForCodeBlockItemListSyntax(node: CodeBlockItemListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForCompositionTypeElementListSyntax(node: CompositionTypeElementListSyntax): Ast = notHandledYet(node)

  private def astForConditionElementListSyntax(node: ConditionElementListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForDeclModifierListSyntax(node: DeclModifierListSyntax): Ast         = notHandledYet(node)
  private def astForDeclNameArgumentListSyntax(node: DeclNameArgumentListSyntax): Ast = notHandledYet(node)
  private def astForDesignatedTypeListSyntax(node: DesignatedTypeListSyntax): Ast     = notHandledYet(node)

  private def astForDictionaryElementListSyntax(node: DictionaryElementListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForDifferentiabilityArgumentListSyntax(node: DifferentiabilityArgumentListSyntax): Ast = notHandledYet(
    node
  )
  private def astForDocumentationAttributeArgumentListSyntax(node: DocumentationAttributeArgumentListSyntax): Ast =
    notHandledYet(node)
  private def astForEffectsAttributeArgumentListSyntax(node: EffectsAttributeArgumentListSyntax): Ast = notHandledYet(
    node
  )
  private def astForEnumCaseElementListSyntax(node: EnumCaseElementListSyntax): Ast         = notHandledYet(node)
  private def astForEnumCaseParameterListSyntax(node: EnumCaseParameterListSyntax): Ast     = notHandledYet(node)
  private def astForExprListSyntax(node: ExprListSyntax): Ast                               = notHandledYet(node)
  private def astForFunctionParameterListSyntax(node: FunctionParameterListSyntax): Ast     = notHandledYet(node)
  private def astForGenericArgumentListSyntax(node: GenericArgumentListSyntax): Ast         = notHandledYet(node)
  private def astForGenericParameterListSyntax(node: GenericParameterListSyntax): Ast       = notHandledYet(node)
  private def astForGenericRequirementListSyntax(node: GenericRequirementListSyntax): Ast   = notHandledYet(node)
  private def astForIfConfigClauseListSyntax(node: IfConfigClauseListSyntax): Ast           = notHandledYet(node)
  private def astForImportPathComponentListSyntax(node: ImportPathComponentListSyntax): Ast = notHandledYet(node)
  private def astForInheritedTypeListSyntax(node: InheritedTypeListSyntax): Ast             = notHandledYet(node)
  private def astForKeyPathComponentListSyntax(node: KeyPathComponentListSyntax): Ast       = notHandledYet(node)

  private def astForLabeledExprListSyntax(node: LabeledExprListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForMemberBlockItemListSyntax(node: MemberBlockItemListSyntax): Ast = notHandledYet(node)

  private def astForMultipleTrailingClosureElementListSyntax(node: MultipleTrailingClosureElementListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForObjCSelectorPieceListSyntax(node: ObjCSelectorPieceListSyntax): Ast     = notHandledYet(node)
  private def astForPatternBindingListSyntax(node: PatternBindingListSyntax): Ast           = notHandledYet(node)
  private def astForPlatformVersionItemListSyntax(node: PlatformVersionItemListSyntax): Ast = notHandledYet(node)
  private def astForPrecedenceGroupAttributeListSyntax(node: PrecedenceGroupAttributeListSyntax): Ast = notHandledYet(
    node
  )
  private def astForPrecedenceGroupNameListSyntax(node: PrecedenceGroupNameListSyntax): Ast     = notHandledYet(node)
  private def astForPrimaryAssociatedTypeListSyntax(node: PrimaryAssociatedTypeListSyntax): Ast = notHandledYet(node)
  private def astForSimpleStringLiteralSegmentListSyntax(node: SimpleStringLiteralSegmentListSyntax): Ast =
    notHandledYet(node)
  private def astForSpecializeAttributeArgumentListSyntax(node: SpecializeAttributeArgumentListSyntax): Ast =
    notHandledYet(node)

  private def astForStringLiteralSegmentListSyntax(node: StringLiteralSegmentListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForSwitchCaseItemListSyntax(node: SwitchCaseItemListSyntax): Ast           = notHandledYet(node)
  private def astForSwitchCaseListSyntax(node: SwitchCaseListSyntax): Ast                   = notHandledYet(node)
  private def astForTuplePatternElementListSyntax(node: TuplePatternElementListSyntax): Ast = notHandledYet(node)
  private def astForTupleTypeElementListSyntax(node: TupleTypeElementListSyntax): Ast       = notHandledYet(node)
  private def astForUnexpectedNodesSyntax(node: UnexpectedNodesSyntax): Ast                 = notHandledYet(node)
  private def astForVersionComponentListSyntax(node: VersionComponentListSyntax): Ast       = notHandledYet(node)
  private def astForYieldedExpressionListSyntax(node: YieldedExpressionListSyntax): Ast     = notHandledYet(node)

  protected def astForSyntaxCollection(syntaxCollection: SyntaxCollection): Ast = syntaxCollection match {
    case node: AccessorDeclListSyntax                   => astForAccessorDeclListSyntax(node)
    case node: ArrayElementListSyntax                   => astForArrayElementListSyntax(node)
    case node: AttributeListSyntax                      => astForAttributeListSyntax(node)
    case node: AvailabilityArgumentListSyntax           => astForAvailabilityArgumentListSyntax(node)
    case node: CatchClauseListSyntax                    => astForCatchClauseListSyntax(node)
    case node: CatchItemListSyntax                      => astForCatchItemListSyntax(node)
    case node: ClosureCaptureListSyntax                 => astForClosureCaptureListSyntax(node)
    case node: ClosureParameterListSyntax               => astForClosureParameterListSyntax(node)
    case node: ClosureShorthandParameterListSyntax      => astForClosureShorthandParameterListSyntax(node)
    case node: CodeBlockItemListSyntax                  => astForCodeBlockItemListSyntax(node)
    case node: CompositionTypeElementListSyntax         => astForCompositionTypeElementListSyntax(node)
    case node: ConditionElementListSyntax               => astForConditionElementListSyntax(node)
    case node: DeclModifierListSyntax                   => astForDeclModifierListSyntax(node)
    case node: DeclNameArgumentListSyntax               => astForDeclNameArgumentListSyntax(node)
    case node: DesignatedTypeListSyntax                 => astForDesignatedTypeListSyntax(node)
    case node: DictionaryElementListSyntax              => astForDictionaryElementListSyntax(node)
    case node: DifferentiabilityArgumentListSyntax      => astForDifferentiabilityArgumentListSyntax(node)
    case node: DocumentationAttributeArgumentListSyntax => astForDocumentationAttributeArgumentListSyntax(node)
    case node: EffectsAttributeArgumentListSyntax       => astForEffectsAttributeArgumentListSyntax(node)
    case node: EnumCaseElementListSyntax                => astForEnumCaseElementListSyntax(node)
    case node: EnumCaseParameterListSyntax              => astForEnumCaseParameterListSyntax(node)
    case node: ExprListSyntax                           => astForExprListSyntax(node)
    case node: FunctionParameterListSyntax              => astForFunctionParameterListSyntax(node)
    case node: GenericArgumentListSyntax                => astForGenericArgumentListSyntax(node)
    case node: GenericParameterListSyntax               => astForGenericParameterListSyntax(node)
    case node: GenericRequirementListSyntax             => astForGenericRequirementListSyntax(node)
    case node: IfConfigClauseListSyntax                 => astForIfConfigClauseListSyntax(node)
    case node: ImportPathComponentListSyntax            => astForImportPathComponentListSyntax(node)
    case node: InheritedTypeListSyntax                  => astForInheritedTypeListSyntax(node)
    case node: KeyPathComponentListSyntax               => astForKeyPathComponentListSyntax(node)
    case node: LabeledExprListSyntax                    => astForLabeledExprListSyntax(node)
    case node: MemberBlockItemListSyntax                => astForMemberBlockItemListSyntax(node)
    case node: MultipleTrailingClosureElementListSyntax => astForMultipleTrailingClosureElementListSyntax(node)
    case node: ObjCSelectorPieceListSyntax              => astForObjCSelectorPieceListSyntax(node)
    case node: PatternBindingListSyntax                 => astForPatternBindingListSyntax(node)
    case node: PlatformVersionItemListSyntax            => astForPlatformVersionItemListSyntax(node)
    case node: PrecedenceGroupAttributeListSyntax       => astForPrecedenceGroupAttributeListSyntax(node)
    case node: PrecedenceGroupNameListSyntax            => astForPrecedenceGroupNameListSyntax(node)
    case node: PrimaryAssociatedTypeListSyntax          => astForPrimaryAssociatedTypeListSyntax(node)
    case node: SimpleStringLiteralSegmentListSyntax     => astForSimpleStringLiteralSegmentListSyntax(node)
    case node: SpecializeAttributeArgumentListSyntax    => astForSpecializeAttributeArgumentListSyntax(node)
    case node: StringLiteralSegmentListSyntax           => astForStringLiteralSegmentListSyntax(node)
    case node: SwitchCaseItemListSyntax                 => astForSwitchCaseItemListSyntax(node)
    case node: SwitchCaseListSyntax                     => astForSwitchCaseListSyntax(node)
    case node: TuplePatternElementListSyntax            => astForTuplePatternElementListSyntax(node)
    case node: TupleTypeElementListSyntax               => astForTupleTypeElementListSyntax(node)
    case node: UnexpectedNodesSyntax                    => astForUnexpectedNodesSyntax(node)
    case node: VersionComponentListSyntax               => astForVersionComponentListSyntax(node)
    case node: YieldedExpressionListSyntax              => astForYieldedExpressionListSyntax(node)
  }

}
