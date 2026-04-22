package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.swiftsrc2cpg.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*

import scala.annotation.unused

trait AstForSyntaxCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private def astForAccessorBlockSyntax(node: AccessorBlockSyntax): Ast = {
    astForNode(node.accessors)
  }

  private def astForAccessorEffectSpecifiersSyntax(node: AccessorEffectSpecifiersSyntax): Ast = notHandledYet(node)

  private def astForAccessorParametersSyntax(node: AccessorParametersSyntax): Ast = {
    val name = code(node.name).stripSuffix(",")
    val parameterNode =
      parameterInNode(
        node,
        name,
        code(node).stripSuffix(","),
        node.json("index").num.toInt + 1,
        false,
        EvaluationStrategies.BY_VALUE
      )
    scope.addVariable(name, parameterNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)
    Ast(parameterNode)
  }

  private def astForArrayElementSyntax(node: ArrayElementSyntax): Ast = {
    astForNode(node.expression)
  }

  private def astForAttributeSyntax(node: AttributeSyntax): Ast = {
    // TODO: find a way to represent arbitrary attribute arguments in the CPG.
    //  For now, we just create an AnnotationNode for the annotation itself.
    val attributeCode = code(node)
    val name          = code(node.attributeName)
    Ast(annotationNode(node, attributeCode, name, name))
  }

  private def astForAvailabilityArgumentSyntax(node: AvailabilityArgumentSyntax): Ast = {
    Ast(literalNode(node, code(node).stripSuffix(","), Option(Defines.String)))
  }

  private def astForAvailabilityConditionSyntax(node: AvailabilityConditionSyntax): Ast = {
    val callName = code(node.availabilityKeyword)

    val callNode = createStaticCallNode(node, code(node), callName, callName, Defines.Bool)
    val argAsts = node.availabilityArguments.children.map { c =>
      Ast(literalNode(c, code(c).stripSuffix(","), Option(Defines.String)))
    }
    callAst(callNode, argAsts)
  }

  private def astForAvailabilityLabeledArgumentSyntax(node: AvailabilityLabeledArgumentSyntax): Ast = notHandledYet(
    node
  )
  private def astForBackDeployedAttributeArgumentsSyntax(node: BackDeployedAttributeArgumentsSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForCatchClauseSyntax(node: CatchClauseSyntax): Ast = {
    astForListSyntaxChildren(node, List(node.catchItems, node.body))
  }

  private def astForCatchItemSyntax(node: CatchItemSyntax): Ast = {
    astForListSyntaxChildren(node, node.pattern.toList ++ node.whereClause.toList)
  }

  private def astForClosureCaptureClauseSyntax(node: ClosureCaptureClauseSyntax): Ast       = notHandledYet(node)
  private def astForClosureCaptureSpecifierSyntax(node: ClosureCaptureSpecifierSyntax): Ast = notHandledYet(node)
  private def astForClosureCaptureSyntax(node: ClosureCaptureSyntax): Ast                   = notHandledYet(node)
  private def astForClosureParameterClauseSyntax(node: ClosureParameterClauseSyntax): Ast   = notHandledYet(node)

  private def astForClosureParameterSyntax(node: ClosureParameterSyntax): Ast = {
    val name           = node.secondName.fold(code(node.firstName))(code)
    val tpeFromTypeMap = fullnameProvider.typeFullname(node)
    val tpe        = tpeFromTypeMap.getOrElse(node.`type`.fold(Defines.Any)(t => AstCreatorHelper.cleanType(code(t))))
    val isVariadic = node.ellipsis.isDefined
    registerType(tpe)
    val parameterNode =
      parameterInNode(
        node,
        name,
        code(node).stripSuffix(","),
        node.json("index").num.toInt + 1,
        isVariadic,
        EvaluationStrategies.BY_VALUE,
        Option(tpe)
      )
    scope.addVariable(name, parameterNode, tpe, VariableScopeManager.ScopeType.MethodScope)
    Ast(parameterNode)
  }

  private def astForClosureShorthandParameterSyntax(node: ClosureShorthandParameterSyntax): Ast = {
    val name = code(node.name)
    val tpe  = fullnameProvider.typeFullname(node).getOrElse(Defines.Any)
    registerType(tpe)
    val parameterNode =
      parameterInNode(
        node,
        name,
        code(node).stripSuffix(","),
        node.json("index").num.toInt + 1,
        false,
        EvaluationStrategies.BY_VALUE,
        Option(tpe)
      )
    scope.addVariable(name, parameterNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)
    Ast(parameterNode)
  }

  private def astForClosureSignatureSyntax(node: ClosureSignatureSyntax): Ast = notHandledYet(node)

  private def astForCodeBlockItemSyntax(node: CodeBlockItemSyntax): Ast = {
    astForNode(node.item)
  }
  private def astForCodeBlockSyntax(node: CodeBlockSyntax): Ast = {
    astForNode(node.statements)
  }

  private def astForCompositionTypeElementSyntax(node: CompositionTypeElementSyntax): Ast = notHandledYet(node)

  private def astForConditionElementSyntax(node: ConditionElementSyntax): Ast = {
    astForNode(node.condition)
  }

  private def astForConformanceRequirementSyntax(node: ConformanceRequirementSyntax): Ast = notHandledYet(node)
  private def astForDeclModifierDetailSyntax(node: DeclModifierDetailSyntax): Ast         = notHandledYet(node)

  private def astForDeclModifierSyntax(node: DeclModifierSyntax): Ast = {
    val modifierType = code(node.name) match {
      case "final"                       => Option(ModifierTypes.FINAL)
      case "private" | "fileprivate"     => Option(ModifierTypes.PRIVATE)
      case "internal"                    => Option(ModifierTypes.INTERNAL)
      case "public" | "open" | "package" => Option(ModifierTypes.PUBLIC)
      case "static"                      => Option(ModifierTypes.STATIC)
      case _                             => None
    }
    modifierType.fold(Ast())(m => Ast(NewModifier().modifierType(m).order(0)))
  }

  private def astForDeclNameArgumentSyntax(node: DeclNameArgumentSyntax): Ast   = notHandledYet(node)
  private def astForDeclNameArgumentsSyntax(node: DeclNameArgumentsSyntax): Ast = notHandledYet(node)
  private def astForDeinitializerEffectSpecifiersSyntax(node: DeinitializerEffectSpecifiersSyntax): Ast = notHandledYet(
    node
  )

  private def astForDerivativeAttributeArgumentsSyntax(node: DerivativeAttributeArgumentsSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForDesignatedTypeSyntax(node: DesignatedTypeSyntax): Ast = notHandledYet(node)

  private def astForDictionaryElementSyntax(node: DictionaryElementSyntax): Ast =
    Ast() // we handle dictionary elements in astForDictionaryExprSyntax

  private def astForDifferentiabilityArgumentSyntax(node: DifferentiabilityArgumentSyntax): Ast   = notHandledYet(node)
  private def astForDifferentiabilityArgumentsSyntax(node: DifferentiabilityArgumentsSyntax): Ast = notHandledYet(node)
  private def astForDifferentiabilityWithRespectToArgumentSyntax(
    node: DifferentiabilityWithRespectToArgumentSyntax
  ): Ast = notHandledYet(node)

  private def astForDifferentiableAttributeArgumentsSyntax(node: DifferentiableAttributeArgumentsSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForDocumentationAttributeArgumentSyntax(node: DocumentationAttributeArgumentSyntax): Ast =
    notHandledYet(node)
  private def astForDynamicReplacementAttributeArgumentsSyntax(node: DynamicReplacementAttributeArgumentsSyntax): Ast =
    notHandledYet(node)
  private def astForEnumCaseElementSyntax(node: EnumCaseElementSyntax): Ast                 = notHandledYet(node)
  private def astForEnumCaseParameterClauseSyntax(node: EnumCaseParameterClauseSyntax): Ast = notHandledYet(node)
  private def astForEnumCaseParameterSyntax(node: EnumCaseParameterSyntax): Ast             = notHandledYet(node)

  private def astForExpressionSegmentSyntax(node: ExpressionSegmentSyntax): Ast = {
    astForNode(node.expressions)
  }

  private def astForFunctionEffectSpecifiersSyntax(node: FunctionEffectSpecifiersSyntax): Ast = notHandledYet(node)
  private def astForFunctionParameterClauseSyntax(node: FunctionParameterClauseSyntax): Ast   = notHandledYet(node)

  private def astForFunctionParameterSyntax(node: FunctionParameterSyntax): Ast = {
    // TODO: handle attributes
    // TODO: handle modifiers
    // TODO: handle defaultValue

    val parameterName = node.secondName match {
      case Some(name) => code(name)
      case None       => code(node.firstName)
    }
    val tpe = fullnameProvider.typeFullname(node).getOrElse(AstCreatorHelper.cleanType(code(node.`type`)))
    registerType(tpe)

    val isVariadic = node.ellipsis.isDefined
    val parameterNode =
      parameterInNode(
        node,
        parameterName,
        code(node).stripSuffix(","),
        node.json("index").num.toInt + 1,
        isVariadic,
        EvaluationStrategies.BY_VALUE,
        Option(tpe)
      )

    scope.addVariable(parameterName, parameterNode, tpe, VariableScopeManager.ScopeType.MethodScope)
    Ast(parameterNode)
  }

  private def astForFunctionSignatureSyntax(node: FunctionSignatureSyntax): Ast           = notHandledYet(node)
  private def astForGenericArgumentClauseSyntax(node: GenericArgumentClauseSyntax): Ast   = notHandledYet(node)
  private def astForGenericArgumentSyntax(node: GenericArgumentSyntax): Ast               = notHandledYet(node)
  private def astForGenericParameterClauseSyntax(node: GenericParameterClauseSyntax): Ast = notHandledYet(node)
  private def astForGenericParameterSyntax(node: GenericParameterSyntax): Ast             = notHandledYet(node)
  private def astForGenericRequirementSyntax(node: GenericRequirementSyntax): Ast         = notHandledYet(node)
  private def astForGenericWhereClauseSyntax(node: GenericWhereClauseSyntax): Ast         = notHandledYet(node)
  private def astForIfConfigClauseSyntax(node: IfConfigClauseSyntax): Ast                 = notHandledYet(node)

  private def astForImplementsAttributeArgumentsSyntax(node: ImplementsAttributeArgumentsSyntax): Ast = {
    Ast(literalNode(node, code(node), Option(Defines.String)))
  }

  private def astForImportPathComponentSyntax(node: ImportPathComponentSyntax): Ast = notHandledYet(node)
  private def astForInheritanceClauseSyntax(node: InheritanceClauseSyntax): Ast     = notHandledYet(node)
  private def astForInheritedTypeSyntax(node: InheritedTypeSyntax): Ast             = notHandledYet(node)

  private def astForInitializerClauseSyntax(node: InitializerClauseSyntax): Ast = {
    astForNode(node.value)
  }

  private def astForKeyPathComponentSyntax(node: KeyPathComponentSyntax): Ast                   = notHandledYet(node)
  private def astForKeyPathOptionalComponentSyntax(node: KeyPathOptionalComponentSyntax): Ast   = notHandledYet(node)
  private def astForKeyPathPropertyComponentSyntax(node: KeyPathPropertyComponentSyntax): Ast   = notHandledYet(node)
  private def astForKeyPathSubscriptComponentSyntax(node: KeyPathSubscriptComponentSyntax): Ast = notHandledYet(node)

  private def astForLabeledExprSyntax(node: LabeledExprSyntax): Ast = {
    node.label match {
      case Some(label) =>
        val labelName = code(label)
        val ast       = astForNode(node.expression)
        ast.root.collect { case i: ExpressionNew => i.argumentLabel(labelName) }
        ast
      case None => astForNode(node.expression)
    }
  }

  private def astForLabeledSpecializeArgumentSyntax(node: LabeledSpecializeArgumentSyntax): Ast = notHandledYet(node)
  private def astForLayoutRequirementSyntax(node: LayoutRequirementSyntax): Ast                 = notHandledYet(node)

  private def astForMatchingPatternConditionSyntax(node: MatchingPatternConditionSyntax): Ast = {
    val initValue = node.initializer.value

    // Detect tuple pattern variants
    val maybeTuplePattern: Option[TuplePatternSyntax] = node.pattern match {
      case tp: TuplePatternSyntax => Some(tp)
      case vb: ValueBindingPatternSyntax =>
        vb.pattern match {
          case tp: TuplePatternSyntax => Some(tp)
          case _                      => None
        }
      case _ => None
    }

    val maybeTupleExpr: Option[TupleExprSyntax] = node.pattern match {
      case ep: ExpressionPatternSyntax =>
        ep.expression match {
          case te: TupleExprSyntax => Some(te)
          case _                   => None
        }
      case vb: ValueBindingPatternSyntax =>
        vb.pattern match {
          case ep: ExpressionPatternSyntax =>
            ep.expression match {
              case te: TupleExprSyntax => Some(te)
              case _                   => None
            }
          case _ => None
        }
      case _ => None
    }

    (maybeTuplePattern, maybeTupleExpr) match {
      case (Some(tuplePat), _) =>
        astForTuplePatternInConditionContext(node, tuplePat, initValue)
      case (_, Some(tupleExpr)) =>
        astForTupleExprInConditionContext(node, tupleExpr, initValue)
      case _ =>
        // Original behavior for non-tuple patterns
        val lhsAst    = astForNode(node.pattern)
        val rhsAst    = astForNode(initValue)
        val tpe       = Defines.Bool
        val op        = Operators.equals
        val callNode_ = createStaticCallNode(node, code(node), op, op, tpe)
        val argAsts   = List(lhsAst, rhsAst)
        callAst(callNode_, argAsts)
    }
  }

  /** De-sugars a tuple in an if-case / guard-case condition.
    *
    * Creates a block: { <tmp> = initValue; ...desugarAsts... } where the de-sugaring is provided by `desugarFn`.
    *
    * Locals are added to `localAstParentStack.head`. For `if` conditions this is the IF control structure node. For
    * `guard` conditions this is the enclosing method/block scope (matching existing `guard let` behavior).
    */
  private def astForTupleInConditionContext(
    node: SwiftNode,
    initValue: SwiftNode,
    arity: Int,
    desugarFn: String => List[Ast]
  ): Ast = {
    val blockNode_ = blockNode(node)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = scopeLocalUniqueName("tmp")
    val tmpLocalNode = localNode(node, tmpName, tmpName, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, tmpLocalNode, EdgeTypes.AST)
    scope.addVariable(tmpName, tmpLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)

    val tmpIdentNode = identifierNode(node, tmpName, tmpName, Defines.Any)
    scope.addVariableReference(tmpName, tmpIdentNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    val initAst   = astForNode(initValue)
    val assignAst = createAssignmentCallAst(node, Ast(tmpIdentNode), initAst, s"$tmpName = ${code(initValue)}")

    val desugarAsts = desugarFn(tmpName)

    // Emit an isTupleN check as the final expression in the condition block
    val op               = Defines.createIsTupleOperator(arity)
    val isTupleCode      = s"$op($tmpName)"
    val isTupleNode      = createStaticCallNode(node, isTupleCode, op, op, Defines.Bool)
    val tmpIdentForCheck = identifierNode(node, tmpName, tmpName, Defines.Any)
    scope.addVariableReference(tmpName, tmpIdentForCheck, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    val isTupleAst = callAst(isTupleNode, List(Ast(tmpIdentForCheck)))

    scope.popScope()
    localAstParentStack.pop()

    blockAst(blockNode_, (assignAst +: desugarAsts) :+ isTupleAst)
  }

  private def astForTuplePatternInConditionContext(
    node: SwiftNode,
    tuplePat: TuplePatternSyntax,
    initValue: SwiftNode
  ): Ast = {
    val arity = tuplePat.elements.children.size
    astForTupleInConditionContext(
      node,
      initValue,
      arity,
      tmpName => astsForBindingTuplePattern(tuplePat, tmpName, List.empty, node)
    )
  }

  private def astForTupleExprInConditionContext(
    node: SwiftNode,
    tupleExpr: TupleExprSyntax,
    initValue: SwiftNode
  ): Ast = {
    val arity = tupleExpr.elements.children.size
    astForTupleInConditionContext(
      node,
      initValue,
      arity,
      tmpName =>
        if (isBindingTupleExpr(tupleExpr)) {
          astsForBindingTupleExpr(tupleExpr, tmpName, List.empty, node)
        } else {
          List(astForExpressionTuplePattern(tupleExpr, tmpName, List.empty, node))
        }
    )
  }

  private def astForMemberBlockItemSyntax(node: MemberBlockItemSyntax): Ast = notHandledYet(node)
  private def astForMemberBlockSyntax(node: MemberBlockSyntax): Ast         = notHandledYet(node)

  private def astForMissingSyntax(@unused node: MissingSyntax): Ast = Ast()

  private def astForMultipleTrailingClosureElementSyntax(node: MultipleTrailingClosureElementSyntax): Ast =
    notHandledYet(node)

  private def astForObjCSelectorPieceSyntax(node: ObjCSelectorPieceSyntax): Ast =
    Ast(literalNode(node, code(node), Option(Defines.String)))

  private def astForOperatorPrecedenceAndTypesSyntax(node: OperatorPrecedenceAndTypesSyntax): Ast = notHandledYet(node)

  private def localForOptionalBindingConditionSyntax(
    node: OptionalBindingConditionSyntax,
    name: String,
    typeFullName: String
  ): Unit = {
    val kind = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") { VariableScopeManager.ScopeType.BlockScope }
    else { VariableScopeManager.ScopeType.MethodScope }
    val nLocalNode = localNode(node, name, name, typeFullName).order(0)
    registerType(typeFullName)
    scope.addVariable(name, nLocalNode, typeFullName, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
  }

  private def astForOptionalBindingConditionSyntax(node: OptionalBindingConditionSyntax): Ast = {
    val typeFullName = node.typeAnnotation.fold(Defines.Any)(t => AstCreatorHelper.cleanType(code(t.`type`)))

    // Detect tuple pattern: Swift parser may produce TuplePatternSyntax or
    // ExpressionPatternSyntax wrapping TupleExprSyntax for `if let (a, b) = x`
    val maybeTupleExpr: Option[TupleExprSyntax] = node.pattern match {
      case tp: TuplePatternSyntax =>
        // Direct tuple pattern — use TuplePatternInConditionContext
        return node.initializer match {
          case Some(init) => astForTuplePatternInConditionContext(node, tp, init.value)
          case None       => Ast()
        }
      case ep: ExpressionPatternSyntax =>
        ep.expression match {
          case te: TupleExprSyntax => Some(te)
          case _                   => None
        }
      case _ => None
    }

    maybeTupleExpr match {
      case Some(tupleExpr) =>
        node.initializer match {
          case Some(init) => astForTupleExprInConditionContext(node, tupleExpr, init.value)
          case None       => Ast()
        }
      case None =>
        // Original behavior for non-tuple patterns
        node.pattern match {
          case ident: IdentifierPatternSyntax =>
            val tpeFromTypeMap = fullnameProvider.typeFullname(ident)
            localForOptionalBindingConditionSyntax(node, code(ident.identifier), tpeFromTypeMap.getOrElse(typeFullName))
          case _ => // do nothing
        }

        val initAst = node.initializer.map(astForNode)
        if (initAst.isEmpty) {
          Ast()
        } else {
          val patternAst = astForNode(node.pattern)
          val tpe        = fullnameProvider.typeFullname(node.pattern).getOrElse(typeFullName)
          registerType(tpe)
          patternAst.root
            .collect { case i: NewIdentifier => i }
            .foreach(_.typeFullName(tpe))
          createAssignmentCallAst(node, patternAst, initAst.head, code(node))
        }
    }
  }

  private def astForOriginallyDefinedInAttributeArgumentsSyntax(
    node: OriginallyDefinedInAttributeArgumentsSyntax
  ): Ast = notHandledYet(node)
  private def astForPatternBindingSyntax(node: PatternBindingSyntax): Ast           = notHandledYet(node)
  private def astForPlatformVersionItemSyntax(node: PlatformVersionItemSyntax): Ast = notHandledYet(node)
  private def astForPlatformVersionSyntax(node: PlatformVersionSyntax): Ast         = notHandledYet(node)
  private def astForPoundSourceLocationArgumentsSyntax(node: PoundSourceLocationArgumentsSyntax): Ast = notHandledYet(
    node
  )
  private def astForPrecedenceGroupAssignmentSyntax(node: PrecedenceGroupAssignmentSyntax): Ast = notHandledYet(node)
  private def astForPrecedenceGroupAssociativitySyntax(node: PrecedenceGroupAssociativitySyntax): Ast = notHandledYet(
    node
  )
  private def astForPrecedenceGroupNameSyntax(node: PrecedenceGroupNameSyntax): Ast         = notHandledYet(node)
  private def astForPrecedenceGroupRelationSyntax(node: PrecedenceGroupRelationSyntax): Ast = notHandledYet(node)
  private def astForPrimaryAssociatedTypeClauseSyntax(node: PrimaryAssociatedTypeClauseSyntax): Ast = notHandledYet(
    node
  )
  private def astForPrimaryAssociatedTypeSyntax(node: PrimaryAssociatedTypeSyntax): Ast = notHandledYet(node)
  private def astForReturnClauseSyntax(node: ReturnClauseSyntax): Ast                   = notHandledYet(node)
  private def astForSameTypeRequirementSyntax(node: SameTypeRequirementSyntax): Ast     = notHandledYet(node)

  private def astForSourceFileSyntax(node: SourceFileSyntax): Ast = {
    node.statements.children.toList match {
      case Nil =>
        val blockNode_ = blockNode(node, PropertyDefaults.Code, Defines.Any)
        blockAst(blockNode_, List.empty)
      case head :: Nil =>
        val blockNode_ = blockNode(node, PropertyDefaults.Code, Defines.Any)
        scope.pushNewBlockScope(blockNode_)
        localAstParentStack.push(blockNode_)
        val childrenAst = astForNode(head)
        localAstParentStack.pop()
        scope.popScope()
        blockAst(blockNode_, List(childrenAst))
      case _ => astForNode(node.statements)
    }
  }

  private def astForSpecializeAvailabilityArgumentSyntax(node: SpecializeAvailabilityArgumentSyntax): Ast =
    notHandledYet(node)
  private def astForSpecializeTargetFunctionArgumentSyntax(node: SpecializeTargetFunctionArgumentSyntax): Ast =
    notHandledYet(node)

  private def astForStringSegmentSyntax(node: StringSegmentSyntax): Ast = {
    Ast(literalNode(node, s"\"${code(node)}\"", Option(Defines.String)))
  }

  private def astForSwitchCaseItemSyntax(node: SwitchCaseItemSyntax): Ast   = notHandledYet(node)
  private def astForSwitchCaseLabelSyntax(node: SwitchCaseLabelSyntax): Ast = notHandledYet(node)
  private def astForSwitchCaseSyntax(node: SwitchCaseSyntax): Ast           = notHandledYet(node)

  private def astForSwitchDefaultLabelSyntax(@unused node: SwitchDefaultLabelSyntax): Ast = Ast()

  private def astForTuplePatternElementSyntax(node: TuplePatternElementSyntax): Ast   = notHandledYet(node)
  private def astForTupleTypeElementSyntax(node: TupleTypeElementSyntax): Ast         = notHandledYet(node)
  private def astForTypeAnnotationSyntax(node: TypeAnnotationSyntax): Ast             = notHandledYet(node)
  private def astForTypeEffectSpecifiersSyntax(node: TypeEffectSpecifiersSyntax): Ast = notHandledYet(node)

  private def astForTypeInitializerClauseSyntax(node: TypeInitializerClauseSyntax): Ast = {
    astForTypeSyntax(node.value)
  }

  private def astForVersionComponentSyntax(node: VersionComponentSyntax): Ast = notHandledYet(node)
  private def astForVersionTupleSyntax(node: VersionTupleSyntax): Ast         = notHandledYet(node)

  private def astForWhereClauseSyntax(node: WhereClauseSyntax): Ast = {
    astForNode(node.condition)
  }

  private def astForYieldedExpressionSyntax(node: YieldedExpressionSyntax): Ast               = notHandledYet(node)
  private def astForYieldedExpressionsClauseSyntax(node: YieldedExpressionsClauseSyntax): Ast = notHandledYet(node)

  protected def astForSyntax(syntax: Syntax): Ast = syntax match {
    case node: AccessorBlockSyntax                          => astForAccessorBlockSyntax(node)
    case node: AccessorEffectSpecifiersSyntax               => astForAccessorEffectSpecifiersSyntax(node)
    case node: AccessorParametersSyntax                     => astForAccessorParametersSyntax(node)
    case node: ArrayElementSyntax                           => astForArrayElementSyntax(node)
    case node: AttributeSyntax                              => astForAttributeSyntax(node)
    case node: AvailabilityArgumentSyntax                   => astForAvailabilityArgumentSyntax(node)
    case node: AvailabilityConditionSyntax                  => astForAvailabilityConditionSyntax(node)
    case node: AvailabilityLabeledArgumentSyntax            => astForAvailabilityLabeledArgumentSyntax(node)
    case node: BackDeployedAttributeArgumentsSyntax         => astForBackDeployedAttributeArgumentsSyntax(node)
    case node: CatchClauseSyntax                            => astForCatchClauseSyntax(node)
    case node: CatchItemSyntax                              => astForCatchItemSyntax(node)
    case node: ClosureCaptureClauseSyntax                   => astForClosureCaptureClauseSyntax(node)
    case node: ClosureCaptureSpecifierSyntax                => astForClosureCaptureSpecifierSyntax(node)
    case node: ClosureCaptureSyntax                         => astForClosureCaptureSyntax(node)
    case node: ClosureParameterClauseSyntax                 => astForClosureParameterClauseSyntax(node)
    case node: ClosureParameterSyntax                       => astForClosureParameterSyntax(node)
    case node: ClosureShorthandParameterSyntax              => astForClosureShorthandParameterSyntax(node)
    case node: ClosureSignatureSyntax                       => astForClosureSignatureSyntax(node)
    case node: CodeBlockItemSyntax                          => astForCodeBlockItemSyntax(node)
    case node: CodeBlockSyntax                              => astForCodeBlockSyntax(node)
    case node: CompositionTypeElementSyntax                 => astForCompositionTypeElementSyntax(node)
    case node: ConditionElementSyntax                       => astForConditionElementSyntax(node)
    case node: ConformanceRequirementSyntax                 => astForConformanceRequirementSyntax(node)
    case node: DeclModifierDetailSyntax                     => astForDeclModifierDetailSyntax(node)
    case node: DeclModifierSyntax                           => astForDeclModifierSyntax(node)
    case node: DeclNameArgumentSyntax                       => astForDeclNameArgumentSyntax(node)
    case node: DeclNameArgumentsSyntax                      => astForDeclNameArgumentsSyntax(node)
    case node: DeinitializerEffectSpecifiersSyntax          => astForDeinitializerEffectSpecifiersSyntax(node)
    case node: DerivativeAttributeArgumentsSyntax           => astForDerivativeAttributeArgumentsSyntax(node)
    case node: DesignatedTypeSyntax                         => astForDesignatedTypeSyntax(node)
    case node: DictionaryElementSyntax                      => astForDictionaryElementSyntax(node)
    case node: DifferentiabilityArgumentSyntax              => astForDifferentiabilityArgumentSyntax(node)
    case node: DifferentiabilityArgumentsSyntax             => astForDifferentiabilityArgumentsSyntax(node)
    case node: DifferentiabilityWithRespectToArgumentSyntax => astForDifferentiabilityWithRespectToArgumentSyntax(node)
    case node: DifferentiableAttributeArgumentsSyntax       => astForDifferentiableAttributeArgumentsSyntax(node)
    case node: DocumentationAttributeArgumentSyntax         => astForDocumentationAttributeArgumentSyntax(node)
    case node: DynamicReplacementAttributeArgumentsSyntax   => astForDynamicReplacementAttributeArgumentsSyntax(node)
    case node: EnumCaseElementSyntax                        => astForEnumCaseElementSyntax(node)
    case node: EnumCaseParameterClauseSyntax                => astForEnumCaseParameterClauseSyntax(node)
    case node: EnumCaseParameterSyntax                      => astForEnumCaseParameterSyntax(node)
    case node: ExpressionSegmentSyntax                      => astForExpressionSegmentSyntax(node)
    case node: FunctionEffectSpecifiersSyntax               => astForFunctionEffectSpecifiersSyntax(node)
    case node: FunctionParameterClauseSyntax                => astForFunctionParameterClauseSyntax(node)
    case node: FunctionParameterSyntax                      => astForFunctionParameterSyntax(node)
    case node: FunctionSignatureSyntax                      => astForFunctionSignatureSyntax(node)
    case node: GenericArgumentClauseSyntax                  => astForGenericArgumentClauseSyntax(node)
    case node: GenericArgumentSyntax                        => astForGenericArgumentSyntax(node)
    case node: GenericParameterClauseSyntax                 => astForGenericParameterClauseSyntax(node)
    case node: GenericParameterSyntax                       => astForGenericParameterSyntax(node)
    case node: GenericRequirementSyntax                     => astForGenericRequirementSyntax(node)
    case node: GenericWhereClauseSyntax                     => astForGenericWhereClauseSyntax(node)
    case node: IfConfigClauseSyntax                         => astForIfConfigClauseSyntax(node)
    case node: ImplementsAttributeArgumentsSyntax           => astForImplementsAttributeArgumentsSyntax(node)
    case node: ImportPathComponentSyntax                    => astForImportPathComponentSyntax(node)
    case node: InheritanceClauseSyntax                      => astForInheritanceClauseSyntax(node)
    case node: InheritedTypeSyntax                          => astForInheritedTypeSyntax(node)
    case node: InitializerClauseSyntax                      => astForInitializerClauseSyntax(node)
    case node: KeyPathComponentSyntax                       => astForKeyPathComponentSyntax(node)
    case node: KeyPathOptionalComponentSyntax               => astForKeyPathOptionalComponentSyntax(node)
    case node: KeyPathPropertyComponentSyntax               => astForKeyPathPropertyComponentSyntax(node)
    case node: KeyPathSubscriptComponentSyntax              => astForKeyPathSubscriptComponentSyntax(node)
    case node: LabeledExprSyntax                            => astForLabeledExprSyntax(node)
    case node: LabeledSpecializeArgumentSyntax              => astForLabeledSpecializeArgumentSyntax(node)
    case node: LayoutRequirementSyntax                      => astForLayoutRequirementSyntax(node)
    case node: MatchingPatternConditionSyntax               => astForMatchingPatternConditionSyntax(node)
    case node: MemberBlockItemSyntax                        => astForMemberBlockItemSyntax(node)
    case node: MemberBlockSyntax                            => astForMemberBlockSyntax(node)
    case node: MissingSyntax                                => astForMissingSyntax(node)
    case node: MultipleTrailingClosureElementSyntax         => astForMultipleTrailingClosureElementSyntax(node)
    case node: ObjCSelectorPieceSyntax                      => astForObjCSelectorPieceSyntax(node)
    case node: OperatorPrecedenceAndTypesSyntax             => astForOperatorPrecedenceAndTypesSyntax(node)
    case node: OptionalBindingConditionSyntax               => astForOptionalBindingConditionSyntax(node)
    case node: OriginallyDefinedInAttributeArgumentsSyntax  => astForOriginallyDefinedInAttributeArgumentsSyntax(node)
    case node: PatternBindingSyntax                         => astForPatternBindingSyntax(node)
    case node: PlatformVersionItemSyntax                    => astForPlatformVersionItemSyntax(node)
    case node: PlatformVersionSyntax                        => astForPlatformVersionSyntax(node)
    case node: PoundSourceLocationArgumentsSyntax           => astForPoundSourceLocationArgumentsSyntax(node)
    case node: PrecedenceGroupAssignmentSyntax              => astForPrecedenceGroupAssignmentSyntax(node)
    case node: PrecedenceGroupAssociativitySyntax           => astForPrecedenceGroupAssociativitySyntax(node)
    case node: PrecedenceGroupNameSyntax                    => astForPrecedenceGroupNameSyntax(node)
    case node: PrecedenceGroupRelationSyntax                => astForPrecedenceGroupRelationSyntax(node)
    case node: PrimaryAssociatedTypeClauseSyntax            => astForPrimaryAssociatedTypeClauseSyntax(node)
    case node: PrimaryAssociatedTypeSyntax                  => astForPrimaryAssociatedTypeSyntax(node)
    case node: ReturnClauseSyntax                           => astForReturnClauseSyntax(node)
    case node: SameTypeRequirementSyntax                    => astForSameTypeRequirementSyntax(node)
    case node: SourceFileSyntax                             => astForSourceFileSyntax(node)
    case node: SpecializeAvailabilityArgumentSyntax         => astForSpecializeAvailabilityArgumentSyntax(node)
    case node: SpecializeTargetFunctionArgumentSyntax       => astForSpecializeTargetFunctionArgumentSyntax(node)
    case node: StringSegmentSyntax                          => astForStringSegmentSyntax(node)
    case node: SwitchCaseItemSyntax                         => astForSwitchCaseItemSyntax(node)
    case node: SwitchCaseLabelSyntax                        => astForSwitchCaseLabelSyntax(node)
    case node: SwitchCaseSyntax                             => astForSwitchCaseSyntax(node)
    case node: SwitchDefaultLabelSyntax                     => astForSwitchDefaultLabelSyntax(node)
    case node: TuplePatternElementSyntax                    => astForTuplePatternElementSyntax(node)
    case node: TupleTypeElementSyntax                       => astForTupleTypeElementSyntax(node)
    case node: TypeAnnotationSyntax                         => astForTypeAnnotationSyntax(node)
    case node: TypeEffectSpecifiersSyntax                   => astForTypeEffectSpecifiersSyntax(node)
    case node: TypeInitializerClauseSyntax                  => astForTypeInitializerClauseSyntax(node)
    case node: VersionComponentSyntax                       => astForVersionComponentSyntax(node)
    case node: VersionTupleSyntax                           => astForVersionTupleSyntax(node)
    case node: WhereClauseSyntax                            => astForWhereClauseSyntax(node)
    case node: YieldedExpressionSyntax                      => astForYieldedExpressionSyntax(node)
    case node: YieldedExpressionsClauseSyntax               => astForYieldedExpressionsClauseSyntax(node)
    case _                                                  => notHandledYet(syntax)
  }

}
