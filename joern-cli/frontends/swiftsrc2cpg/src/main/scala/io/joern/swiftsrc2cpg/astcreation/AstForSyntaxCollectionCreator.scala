package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.File.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators

trait AstForSyntaxCollectionCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  protected def astForListSyntaxChildren(node: SwiftNode, children: Seq[SwiftNode]): Ast = {
    children.toList match {
      case Nil         => Ast()
      case head :: Nil => astForNodeWithFunctionReference(head)
      case elements =>
        val blockNode_ = blockNode(node, PropertyDefaults.Code, Defines.Any)
        scope.pushNewBlockScope(blockNode_)
        localAstParentStack.push(blockNode_)
        val childrenAsts = astsForBlockElements(elements)
        localAstParentStack.pop()
        scope.popScope()
        blockAst(blockNode_, childrenAsts)
    }
  }

  private def astForAccessorDeclListSyntax(node: AccessorDeclListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForArrayElementListSyntax(node: ArrayElementListSyntax): Ast = notHandledYet(node)
  private def astForAttributeListSyntax(node: AttributeListSyntax): Ast       = notHandledYet(node)

  private def astForAvailabilityArgumentListSyntax(node: AvailabilityArgumentListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForCatchClauseListSyntax(node: CatchClauseListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForCatchItemListSyntax(node: CatchItemListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForClosureCaptureListSyntax(node: ClosureCaptureListSyntax): Ast     = notHandledYet(node)
  private def astForClosureParameterListSyntax(node: ClosureParameterListSyntax): Ast = notHandledYet(node)
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
  private def astForDocumentationAttributeArgumentListSyntax(node: DocumentationAttributeArgumentListSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForEffectsAttributeArgumentListSyntax(node: EffectsAttributeArgumentListSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForEnumCaseElementListSyntax(node: EnumCaseElementListSyntax): Ast     = notHandledYet(node)
  private def astForEnumCaseParameterListSyntax(node: EnumCaseParameterListSyntax): Ast = notHandledYet(node)

  private def astForExprListSyntax(node: ExprListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

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

  private def astForLifetimeSpecifierArgumentListSyntax(node: LifetimeSpecifierArgumentListSyntax): Ast = {
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

  private def astForSimpleStringLiteralSegmentListSyntax(node: SimpleStringLiteralSegmentListSyntax): Ast = {
    node.children match {
      case child :: Nil => astForNodeWithFunctionReference(child)
      case children =>
        val stringFormatCall = callNode(node, code(node), Operators.formatString, DispatchTypes.STATIC_DISPATCH)
        val childrenAsts     = children.map(astForNodeWithFunctionReference)
        setArgumentIndices(childrenAsts)
        callAst(stringFormatCall, childrenAsts)
    }
  }

  private def astForSpecializeAttributeArgumentListSyntax(node: SpecializeAttributeArgumentListSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForStringLiteralSegmentListSyntax(node: StringLiteralSegmentListSyntax): Ast = {
    node.children match {
      case child :: Nil => astForNodeWithFunctionReference(child)
      case children =>
        val stringFormatCall = callNode(node, code(node), Operators.formatString, DispatchTypes.STATIC_DISPATCH)
        val childrenAsts     = children.map(astForNodeWithFunctionReference)
        setArgumentIndices(childrenAsts)
        callAst(stringFormatCall, childrenAsts)
    }
  }

  private def astForSwitchCaseItemListSyntax(node: SwitchCaseItemListSyntax): Ast = {
    astForListSyntaxChildren(node, node.children)
  }

  private def astForSwitchCaseListSyntax(node: SwitchCaseListSyntax): Ast = {
    val blockNode_ = blockNode(node, PropertyDefaults.Code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val asts = node.children.toList.flatMap(astsForSwitchCase)
    setArgumentIndices(asts)
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, asts)
  }

  private def astForTuplePatternElementListSyntax(node: TuplePatternElementListSyntax): Ast = notHandledYet(node)
  private def astForTupleTypeElementListSyntax(node: TupleTypeElementListSyntax): Ast       = notHandledYet(node)
  private def astForTypeSpecifierListSyntax(node: TypeSpecifierListSyntax): Ast             = notHandledYet(node)
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
    case node: LifetimeSpecifierArgumentListSyntax      => astForLifetimeSpecifierArgumentListSyntax(node)
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
    case node: TypeSpecifierListSyntax                  => astForTypeSpecifierListSyntax(node)
    case node: UnexpectedNodesSyntax                    => astForUnexpectedNodesSyntax(node)
    case node: VersionComponentListSyntax               => astForVersionComponentListSyntax(node)
    case node: YieldedExpressionListSyntax              => astForYieldedExpressionListSyntax(node)
  }

}
