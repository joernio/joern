package io.joern.swiftsrc2cpg.astcreation

import io.joern.swiftsrc2cpg.datastructures.BlockScope
import io.joern.swiftsrc2cpg.datastructures.MethodScope
import io.joern.swiftsrc2cpg.parser.SwiftNodeSyntax.*
import io.joern.swiftsrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes

trait AstForDeclSyntaxCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def astForAccessorDeclSyntax(node: AccessorDeclSyntax): Ast                   = notHandledYet(node)
  private def astForActorDeclSyntax(node: ActorDeclSyntax): Ast                         = notHandledYet(node)
  private def astForAssociatedTypeDeclSyntax(node: AssociatedTypeDeclSyntax): Ast       = notHandledYet(node)
  private def astForClassDeclSyntax(node: ClassDeclSyntax): Ast                         = notHandledYet(node)
  private def astForDeinitializerDeclSyntax(node: DeinitializerDeclSyntax): Ast         = notHandledYet(node)
  private def astForEditorPlaceholderDeclSyntax(node: EditorPlaceholderDeclSyntax): Ast = notHandledYet(node)
  private def astForEnumCaseDeclSyntax(node: EnumCaseDeclSyntax): Ast                   = notHandledYet(node)
  private def astForEnumDeclSyntax(node: EnumDeclSyntax): Ast                           = notHandledYet(node)
  private def astForExtensionDeclSyntax(node: ExtensionDeclSyntax): Ast                 = notHandledYet(node)

  private def modifiersForFunctionLike(node: FunctionDeclSyntax): Seq[NewModifier] = {
    val virtualModifier = NewModifier().modifierType(ModifierTypes.VIRTUAL)
    val modifiers       = node.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    (virtualModifier +: modifiers).zipWithIndex.map { case (m, index) =>
      m.order(index)
    }
  }

  protected def astForFunctionLike(
    node: FunctionDeclSyntax,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = {
    // TODO: handle genericParameterClause
    // TODO: handle genericWhereClause
    val attributes                   = node.attributes.children.map(astForNode)
    val modifiers                    = modifiersForFunctionLike(node)
    val (methodName, methodFullName) = calcMethodNameAndFullName(node)
    val filename                     = parserResult.filename

    val methodRefNode_ = if (!shouldCreateFunctionReference) {
      None
    } else {
      Option(methodRefNode(node, methodName, methodFullName, methodFullName))
    }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = identifierNode(node, methodName)
      val idLocal = localNode(node, methodName, methodName, methodFullName).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      scope.addVariableReference(methodName, idNode)
      val assignmentCode = s"func $methodName = ${code(node)}"
      val assignment     = createAssignmentCallAst(idNode, methodRefNode_.get, assignmentCode, line(node), column(node))
      assignment
    } else {
      Ast()
    }

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode_
      } else {
        typeRefIdStack.headOption
      }

    val returnType = node.signature.returnClause.map(c => code(c.`type`)).getOrElse(Defines.Any)
    registerType(returnType, returnType)

    val signature = s"$returnType $methodFullName ${code(node.signature.parameterClause)}"

    val codeString  = code(node)
    val methodNode_ = methodNode(node, methodName, codeString, methodFullName, Some(signature), filename)
    val block       = blockNode(node, "<empty>", Defines.Any)
    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(methodFullName, methodName, block, capturingRefNode)
    localAstParentStack.push(block)

    val parameterAsts = node.signature.parameterClause.parameters.children.map(astForNode)

    val bodyStmtAsts = node.body match {
      case Some(bodyNode) =>
        bodyNode.statements.children.toList match {
          case Nil => List.empty[Ast]
          case head :: Nil if head.item.isInstanceOf[ArrowExprSyntax] =>
            val retCode = code(head)
            List(returnAst(returnNode(head, retCode), List(astForNodeWithFunctionReference(head.item))))
          case children =>
            val childrenAsts = children.map(astForNode)
            setArgumentIndices(childrenAsts)
            childrenAsts
        }
      case None =>
        List.empty[Ast]
    }

    val methodReturnNode =
      newMethodReturnNode(returnType, dynamicTypeHintFullName = None, line = line(node), column = column(node))

    val astForMethod =
      methodAstWithAnnotations(
        methodNode_,
        parameterAsts,
        blockAst(block, bodyStmtAsts),
        methodReturnNode,
        modifiers = modifiers,
        annotations = attributes
      )

    scope.popScope()
    localAstParentStack.pop()
    methodAstParentStack.pop()

    val typeDeclAst = createFunctionTypeAndTypeDeclAst(node, methodNode_, methodName, methodFullName)
    Ast.storeInDiffGraph(astForMethod, diffGraph)
    Ast.storeInDiffGraph(typeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodRefNode_ match {
      case Some(ref) if callAst.nodes.isEmpty => Ast(ref)
      case _                                  => callAst
    }
  }

  private def astForFunctionDeclSyntax(node: FunctionDeclSyntax): Ast = {
    astForFunctionLike(node)
  }

  private def astForIfConfigDeclSyntax(node: IfConfigDeclSyntax): Ast               = notHandledYet(node)
  private def astForImportDeclSyntax(node: ImportDeclSyntax): Ast                   = notHandledYet(node)
  private def astForInitializerDeclSyntax(node: InitializerDeclSyntax): Ast         = notHandledYet(node)
  private def astForMacroDeclSyntax(node: MacroDeclSyntax): Ast                     = notHandledYet(node)
  private def astForMacroExpansionDeclSyntax(node: MacroExpansionDeclSyntax): Ast   = notHandledYet(node)
  private def astForMissingDeclSyntax(node: MissingDeclSyntax): Ast                 = notHandledYet(node)
  private def astForOperatorDeclSyntax(node: OperatorDeclSyntax): Ast               = notHandledYet(node)
  private def astForPoundSourceLocationSyntax(node: PoundSourceLocationSyntax): Ast = notHandledYet(node)
  private def astForPrecedenceGroupDeclSyntax(node: PrecedenceGroupDeclSyntax): Ast = notHandledYet(node)
  private def astForProtocolDeclSyntax(node: ProtocolDeclSyntax): Ast               = notHandledYet(node)
  private def astForStructDeclSyntax(node: StructDeclSyntax): Ast                   = notHandledYet(node)
  private def astForSubscriptDeclSyntax(node: SubscriptDeclSyntax): Ast             = notHandledYet(node)
  private def astForTypeAliasDeclSyntax(node: TypeAliasDeclSyntax): Ast             = notHandledYet(node)

  private def astForVariableDeclSyntax(node: VariableDeclSyntax): Ast = {
    val attributeAsts = node.attributes.children.map(astForNode)
    val modifiers     = node.modifiers.children.flatMap(c => astForNode(c).root.map(_.asInstanceOf[NewModifier]))
    val kind          = code(node.bindingSpecifier)
    val scopeType = if (kind == "let") {
      BlockScope
    } else {
      MethodScope
    }

    val bindingAsts = node.bindings.children.map { binding =>
      val name = binding.pattern match {
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
      val typeFullName = binding.typeAnnotation.map(code).getOrElse(Defines.Any)
      val nLocalNode   = localNode(binding, name, name, typeFullName).order(0)
      scope.addVariable(name, nLocalNode, scopeType)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val initAsts = binding.initializer.map(astForNode) ++ binding.accessorBlock.map(astForNode)
      if (initAsts.isEmpty) {
        Ast()
      } else {
        val patternAst = astForNode(binding.pattern)
        modifiers.foreach { mod =>
          patternAst.root.foreach { r => diffGraph.addEdge(r, mod, EdgeTypes.AST) }
        }
        attributeAsts.foreach { attrAst =>
          patternAst.root.foreach { r => attrAst.root.foreach { attr => diffGraph.addEdge(r, attr, EdgeTypes.AST) } }
        }
        createAssignmentCallAst(
          patternAst,
          initAsts.head,
          s"$kind ${code(binding)}",
          line = line(binding),
          column = column(binding)
        )
      }
    }

    bindingAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case _ =>
        val block = blockNode(node, code(node), Defines.Any)
        setArgumentIndices(bindingAsts)
        blockAst(block, bindingAsts.toList)
    }
  }

  protected def astForDeclSyntax(declSyntax: DeclSyntax): Ast = declSyntax match {
    case node: AccessorDeclSyntax          => astForAccessorDeclSyntax(node)
    case node: ActorDeclSyntax             => astForActorDeclSyntax(node)
    case node: AssociatedTypeDeclSyntax    => astForAssociatedTypeDeclSyntax(node)
    case node: ClassDeclSyntax             => astForClassDeclSyntax(node)
    case node: DeinitializerDeclSyntax     => astForDeinitializerDeclSyntax(node)
    case node: EditorPlaceholderDeclSyntax => astForEditorPlaceholderDeclSyntax(node)
    case node: EnumCaseDeclSyntax          => astForEnumCaseDeclSyntax(node)
    case node: EnumDeclSyntax              => astForEnumDeclSyntax(node)
    case node: ExtensionDeclSyntax         => astForExtensionDeclSyntax(node)
    case node: FunctionDeclSyntax          => astForFunctionDeclSyntax(node)
    case node: IfConfigDeclSyntax          => astForIfConfigDeclSyntax(node)
    case node: ImportDeclSyntax            => astForImportDeclSyntax(node)
    case node: InitializerDeclSyntax       => astForInitializerDeclSyntax(node)
    case node: MacroDeclSyntax             => astForMacroDeclSyntax(node)
    case node: MacroExpansionDeclSyntax    => astForMacroExpansionDeclSyntax(node)
    case node: MissingDeclSyntax           => astForMissingDeclSyntax(node)
    case node: OperatorDeclSyntax          => astForOperatorDeclSyntax(node)
    case node: PoundSourceLocationSyntax   => astForPoundSourceLocationSyntax(node)
    case node: PrecedenceGroupDeclSyntax   => astForPrecedenceGroupDeclSyntax(node)
    case node: ProtocolDeclSyntax          => astForProtocolDeclSyntax(node)
    case node: StructDeclSyntax            => astForStructDeclSyntax(node)
    case node: SubscriptDeclSyntax         => astForSubscriptDeclSyntax(node)
    case node: TypeAliasDeclSyntax         => astForTypeAliasDeclSyntax(node)
    case node: VariableDeclSyntax          => astForVariableDeclSyntax(node)
  }

}
