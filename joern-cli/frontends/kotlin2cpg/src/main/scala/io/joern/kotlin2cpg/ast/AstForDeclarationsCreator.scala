package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.psi.PsiUtils
import io.joern.kotlin2cpg.psi.PsiUtils.nonUnderscoreDestructuringEntries
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newIdentifierNode
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.semanticcpg.language.*
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.psi.*

import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.Random

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  import AstCreator.AnonymousObjectContext
  import AstCreator.BindingInfo

  def astsForClassOrObject(
    ktClass: KtClassOrObject,
    ctx: Option[AnonymousObjectContext] = None,
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Seq[Ast] = {
    val className = ctx match {
      case Some(_) => "anonymous_obj"
      case None    => ktClass.getName
    }

    val classDesc = bindingUtils.getClassDesc(ktClass)

    val classFullName =
      nameRenderer.descFullName(classDesc).getOrElse {
        val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
        s"$fqName.$className"
      }
    registerType(classFullName)

    val baseTypeFullNames =
      ktClass.getSuperTypeListEntries.asScala
        .flatMap { superTypeEntry =>
          val typeRef = superTypeEntry.getTypeReference
          val superType = bindingUtils
            .getTypeRefType(typeRef)
            .flatMap(nameRenderer.typeFullName)

          superType.orElse {
            fullNameByImportPath(typeRef, ktClass.getContainingKtFile)
          }
        }
        .to(mutable.ArrayBuffer)

    if (baseTypeFullNames.isEmpty) {
      baseTypeFullNames.append(TypeConstants.JavaLangObject)
    }

    baseTypeFullNames.foreach(registerType)
    val typeDecl = typeDeclNode(ktClass, className, classFullName, relativizedPath, baseTypeFullNames.toSeq, None)
    scope.pushNewScope(typeDecl)
    methodAstParentStack.push(typeDecl)

    val primaryCtor       = ktClass.getPrimaryConstructor
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList

    val (fullName, signature) =
      if (primaryCtor != null) {
        val constructorDesc = bindingUtils.getConstructorDesc(primaryCtor)
        val descFullName = nameRenderer
          .descFullName(constructorDesc)
          .getOrElse(s"$classFullName.${Defines.ConstructorMethodName}")
        val signature = nameRenderer
          .funcDescSignature(constructorDesc)
          .getOrElse(s"${Defines.UnresolvedSignature}(${primaryCtor.getValueParameters.size()})")
        val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)
        (fullName, signature)
      } else {
        val descFullName = s"$classFullName.${Defines.ConstructorMethodName}"
        val signature    = s"${TypeConstants.Void}()"
        val fullName     = nameRenderer.combineFunctionFullName(descFullName, signature)
        (fullName, signature)
      }
    val primaryCtorMethodNode =
      methodNode(primaryCtor, Defines.ConstructorMethodName, fullName, signature, relativizedPath)
    val ctorThisParam =
      NodeBuilders.newThisParameterNode(typeFullName = classFullName, dynamicTypeHintFullName = Seq(classFullName))
    scope.addToScope(Constants.ThisName, ctorThisParam)

    val constructorParamsAsts = Seq(Ast(ctorThisParam)) ++ withIndex(constructorParams) { (p, idx) =>
      astForParameter(p, idx)
    }

    val memberSetCalls = constructorParams.collect {
      case ctorParam if ctorParam.hasValOrVar =>
        memberSetCallAst(ctorParam, classFullName)
    }

    val classDeclarations = Option(ktClass.getBody)
      .map(_.getDeclarations.asScala.filterNot(_.isInstanceOf[KtNamedFunction]))
      .getOrElse(List())

    val memberInitializerSetCalls =
      classDeclarations.collectAll[KtProperty].filter(_.getInitializer != null).map { decl =>
        val initializerAsts = astsForExpression(decl.getInitializer, None)
        val rhsAst =
          if (initializerAsts.size == 1) initializerAsts.head
          else Ast(unknownNode(decl, "<empty>"))

        val thisIdentifier = newIdentifierNode(Constants.ThisName, classFullName, Seq(classFullName))
        val thisAst        = astWithRefEdgeMaybe(Constants.ThisName, thisIdentifier)

        val fieldIdentifier = fieldIdentifierNode(decl, decl.getName, decl.getName)
        val fieldAccessCall = NodeBuilders
          .newOperatorCallNode(Operators.fieldAccess, s"${Constants.ThisName}.${fieldIdentifier.canonicalName}", None)
        val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))

        val assignmentNode = NodeBuilders
          .newOperatorCallNode(Operators.assignment, s"${fieldAccessCall.code} = ${decl.getInitializer.getText}")
        callAst(assignmentNode, List(fieldAccessCallAst, rhsAst))
      }

    val anonymousInitExpressions = ktClass.getAnonymousInitializers.asScala
    val anonymousInitAsts        = anonymousInitExpressions.flatMap(astsForExpression(_, None))

    val constructorMethodReturn = newMethodReturnNode(
      TypeConstants.Void,
      None,
      line(ktClass.getPrimaryConstructor),
      column(ktClass.getPrimaryConstructor)
    )
    val constructorAst = methodAst(
      primaryCtorMethodNode,
      constructorParamsAsts,
      blockAst(
        blockNode(ktClass, "", TypeConstants.Void),
        memberSetCalls ++ memberInitializerSetCalls ++ anonymousInitAsts
      ),
      constructorMethodReturn,
      Seq(newModifierNode(ModifierTypes.CONSTRUCTOR))
    )
    val node =
      newBindingNode(primaryCtorMethodNode.name, primaryCtorMethodNode.signature, primaryCtorMethodNode.fullName)
    val ctorBindingInfo =
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, primaryCtorMethodNode, EdgeTypes.REF)))

    val membersFromPrimaryCtorAsts = ktClass.getPrimaryConstructorParameters.asScala.toList.collect {
      case param if param.hasValOrVar =>
        val typeFullName = registerType(
          nameRenderer.typeFullName(bindingUtils.getVariableDesc(param).get.getType).getOrElse(TypeConstants.Any)
        )
        val memberNode_ = memberNode(param, param.getName, param.getName, typeFullName)
        scope.addToScope(param.getName, memberNode_)
        Ast(memberNode_)
    }

    val primaryCtorCall =
      callNode(
        ktClass.getPrimaryConstructor,
        Defines.ConstructorMethodName,
        primaryCtorMethodNode.name,
        primaryCtorMethodNode.fullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(primaryCtorMethodNode.signature),
        Some(constructorMethodReturn.typeFullName)
      )

    val secondaryConstructorAsts =
      secondaryCtorAsts(ktClass.getSecondaryConstructors.asScala.toSeq, classFullName, primaryCtorCall)
    val _componentNMethodAsts = ktClass match {
      case typedExpr: KtClass if typedExpr.isData =>
        componentNMethodAsts(typeDecl, ktClass.getPrimaryConstructor.getValueParameters.asScala.toSeq)
      case _ => Seq()
    }
    val componentNBindingsInfo = _componentNMethodAsts.flatMap(_.root.collectAll[NewMethod]).map { methodNode =>
      val node = newBindingNode(methodNode.name, methodNode.signature, methodNode.fullName)
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, methodNode, EdgeTypes.REF)))
    }

    val memberAsts = classDeclarations.toSeq.map(astForMember)
    val innerTypeDeclAsts =
      classDeclarations.toSeq
        .collectAll[KtClassOrObject]
        .filterNot(desc => bindingUtils.getClassDesc(desc).isCompanionObject)
        .flatMap(astsForDeclaration(_))

    val classFunctions = Option(ktClass.getBody)
      .map(_.getFunctions.asScala.collect { case f: KtNamedFunction => f })
      .getOrElse(List())
    val methodAsts = classFunctions.toSeq.flatMap { classFn =>
      astsForMethod(classFn, withVirtualModifier = true)
    }
    val bindingsInfo = methodAsts.flatMap(_.root.collectAll[NewMethod]).map { _methodNode =>
      val node = newBindingNode(_methodNode.name, _methodNode.signature, _methodNode.fullName)
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, _methodNode, EdgeTypes.REF)))
    }

    val annotationAsts = ktClass.getAnnotationEntries.asScala.map(astForAnnotationEntry).toSeq

    val modifiers = if (classDesc.getModality == Modality.ABSTRACT) {
      List(Ast(NodeBuilders.newModifierNode(ModifierTypes.ABSTRACT)))
    } else {
      Nil
    }

    val children = methodAsts ++ List(constructorAst) ++ membersFromPrimaryCtorAsts ++ secondaryConstructorAsts ++
      _componentNMethodAsts.toList ++ memberAsts ++ annotationAsts ++ modifiers
    val ast = Ast(typeDecl).withChildren(children)

    (List(ctorBindingInfo) ++ bindingsInfo ++ componentNBindingsInfo).foreach(bindingInfoQueue.prepend)

    val finalAst = if (classDesc.isCompanionObject) {
      val companionMemberTypeFullName = ktClass.getParent.getParent match {
        case c: KtClassOrObject =>
          nameRenderer.descFullName(bindingUtils.getClassDesc(c)).getOrElse(TypeConstants.Any)
        case _ => TypeConstants.Any
      }
      registerType(companionMemberTypeFullName)

      val companionObjectMember = memberNode(
        ktClass,
        Constants.CompanionObjectMemberName,
        Constants.CompanionObjectMemberName,
        companionMemberTypeFullName
      )
      ast.withChild(Ast(companionObjectMember))
    } else {
      ast
    }
    val companionObjectAsts = ktClass.getCompanionObjects.asScala.flatMap(astsForClassOrObject(_, None))
    methodAstParentStack.pop()
    scope.popScope()

    Seq(finalAst.withChildren(annotations.map(astForAnnotationEntry))) ++ companionObjectAsts ++ innerTypeDeclAsts
  }

  private def memberSetCallAst(param: KtParameter, classFullName: String): Ast = {
    val typeFullName = registerType(
      bindingUtils
        .getVariableDesc(param)
        .flatMap(desc => nameRenderer.typeFullName(desc.getType))
        .getOrElse(TypeConstants.Any)
    )
    val paramName          = param.getName
    val paramIdentifier    = identifierNode(param, paramName, paramName, typeFullName)
    val paramIdentifierAst = astWithRefEdgeMaybe(paramName, paramIdentifier)
    val thisIdentifier     = newIdentifierNode(Constants.ThisName, classFullName, Seq(classFullName))
    val thisAst            = astWithRefEdgeMaybe(Constants.ThisName, thisIdentifier)

    val fieldIdentifier = fieldIdentifierNode(param, paramName, paramName)
    val fieldAccessCall =
      NodeBuilders.newOperatorCallNode(Operators.fieldAccess, s"${Constants.ThisName}.$paramName", Option(typeFullName))
    val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))

    val assignmentNode =
      NodeBuilders.newOperatorCallNode(Operators.assignment, s"${fieldAccessCall.code} = ${paramIdentifier.code}")
    callAst(assignmentNode, List(fieldAccessCallAst, paramIdentifierAst))
  }

  private def astsForDestructuringDeclarationWithRHS(expr: KtDestructuringDeclaration): Seq[Ast] = {
    val typedInit = Option(expr.getInitializer).collect {
      case c: KtCallExpression           => c
      case dqe: KtDotQualifiedExpression => dqe
      case ac: KtArrayAccessExpression   => ac
      case pf: KtPostfixExpression       => pf
      case w: KtWhenExpression           => w
      case i: KtIfExpression             => i
    }
    if (typedInit.isEmpty) {
      logger.warn(
        s"Unhandled case for destructuring declaration: `${expr.getText}`; type: `${expr.getInitializer.getClass}` in this file `$relativizedPath`."
      )
      return Seq()
    }
    val rhsCall             = typedInit.get
    val callRhsTypeFullName = registerType(exprTypeFullName(rhsCall).getOrElse(TypeConstants.Any))

    val localsForEntries = localsForDestructuringEntries(expr)

    val isCtor = expr.getInitializer match {
      case _: KtCallExpression => typeInfoProvider.isConstructorCall(rhsCall).getOrElse(false)
      case _                   => false
    }
    val tmpName         = s"${Constants.TmpLocalPrefix}${tmpKeyPool.next}"
    val localForTmpNode = localNode(expr, tmpName, tmpName, callRhsTypeFullName)
    scope.addToScope(localForTmpNode.name, localForTmpNode)
    val localForTmpAst = Ast(localForTmpNode)

    val assignmentLhsNode = identifierNode(expr, tmpName, tmpName, localForTmpNode.typeFullName)
    val assignmentLhsAst  = Ast(assignmentLhsNode).withRefEdge(assignmentLhsNode, localForTmpNode)
    val tmpAssignmentAst =
      if (isCtor) {
        val assignmentRhsNode = NodeBuilders.newOperatorCallNode(
          Operators.alloc,
          Constants.Alloc,
          Option(localForTmpNode.typeFullName),
          line(expr),
          column(expr)
        )
        val assignmentNode =
          NodeBuilders.newOperatorCallNode(Operators.assignment, s"$tmpName  = ${Constants.Alloc}", None)
        callAst(assignmentNode, List(assignmentLhsAst, Ast(assignmentRhsNode)))
      } else {
        expr.getInitializer match {
          case accessExpression: KtArrayAccessExpression =>
            astForArrayAccess(accessExpression, None, None)
          case expression: KtPostfixExpression =>
            astForPostfixExpression(expression, None, None)
          case expression: KtWhenExpression =>
            astForWhenAsExpression(expression, None, None)
          case expression: KtIfExpression =>
            astForIfAsExpression(expression, None, None)
          case _ =>
            val assignmentNode =
              NodeBuilders.newOperatorCallNode(Operators.assignment, s"$tmpName = ${rhsCall.getText}", None)
            val assignmentRhsAst =
              astsForExpression(rhsCall, None).headOption.getOrElse(Ast(unknownNode(rhsCall, Constants.Empty)))
            callAst(assignmentNode, List(assignmentLhsAst, assignmentRhsAst))
        }
      }
    val tmpAssignmentPrologue = rhsCall match {
      case call: KtCallExpression if isCtor =>
        val initReceiverNode =
          identifierNode(expr, tmpName, tmpName, localForTmpNode.typeFullName).argumentIndex(0)
        val initReceiverAst = Ast(initReceiverNode).withRefEdge(initReceiverNode, localForTmpNode)
        val argAsts         = astsForKtCallExpressionArguments(call)
        val (fullName, signature) =
          calleeFullnameAndSignature(
            getCalleeExpr(rhsCall),
            Defines.UnresolvedNamespace,
            s"${Defines.UnresolvedSignature}(${call.getValueArguments.size()})"
          )
        registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
        val initCallNode = callNode(
          expr,
          Defines.ConstructorMethodName,
          Defines.ConstructorMethodName,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(TypeConstants.Void)
        )
        Seq(callAst(initCallNode, argAsts, Some(initReceiverAst), None))
      case _ => Seq()
    }

    val assignmentsForEntries = nonUnderscoreDestructuringEntries(expr).zipWithIndex.map { case (entry, idx) =>
      val rhsBaseAst =
        astWithRefEdgeMaybe(
          localForTmpNode.name,
          identifierNode(entry, localForTmpNode.name, localForTmpNode.name, localForTmpNode.typeFullName)
            .argumentIndex(0)
        )
      assignmentAstForDestructuringEntry(entry, rhsBaseAst, idx + 1)
    }
    localsForEntries ++ Seq(localForTmpAst) ++
      Seq(tmpAssignmentAst) ++ tmpAssignmentPrologue ++ assignmentsForEntries

  }

  /*
   _______ example lowering _________
  | val (one, two) = person
  |
  | -> LOCAL one
  | -> LOCAL two
  | -> CALL one = person.component1()
  | -> CALL two = person.component2()
  |__________________________________
   */
  private def astsForDestructuringDeclarationWithVarRHS(expr: KtDestructuringDeclaration): Seq[Ast] = {
    val typedInit = Option(expr.getInitializer).collect { case e: KtNameReferenceExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}` in this file `$relativizedPath`.")
      return Seq()
    }

    val assignmentsForEntries =
      nonUnderscoreDestructuringEntries(expr).zipWithIndex.map { case (entry, idx) =>
        val rhsBaseAst = astForNameReference(typedInit.get, Some(1), None)
        assignmentAstForDestructuringEntry(entry, rhsBaseAst, idx + 1)
      }
    val localsForEntries = localsForDestructuringEntries(expr)
    localsForEntries ++ assignmentsForEntries
  }

  def localsForDestructuringEntries(destructuring: KtDestructuringDeclaration): Seq[Ast] = {
    destructuring.getEntries.asScala
      .filterNot(_.getText == Constants.UnusedDestructuringEntryText)
      .map { entry =>
        val entryTypeFullName = registerType(
          bindingUtils
            .getVariableDesc(entry)
            .flatMap(desc => nameRenderer.typeFullName(desc.getType))
            .getOrElse(TypeConstants.Any)
        )
        val entryName = entry.getText
        val node      = localNode(entry, entryName, entryName, entryTypeFullName)
        scope.addToScope(entryName, node)
        Ast(node)
      }
      .toSeq
  }

  def astsForDestructuringDeclaration(expr: KtDestructuringDeclaration): Seq[Ast] = {
    val hasNonRefExprRHS = expr.getInitializer match {
      case _: KtNameReferenceExpression => false
      case _: KtExpression              => true
      case null                         => false
    }
    if (hasNonRefExprRHS) astsForDestructuringDeclarationWithRHS(expr)
    else astsForDestructuringDeclarationWithVarRHS(expr)
  }

  private def componentNMethodAsts(typeDecl: NewTypeDecl, parameters: Seq[KtParameter]): Seq[Ast] = {
    parameters.zipWithIndex.map { case (valueParam, idx) =>
      val typeFullName = registerType(
        bindingUtils
          .getVariableDesc(valueParam)
          .flatMap(desc => nameRenderer.typeFullName(desc.getType))
          .getOrElse(TypeConstants.Any)
      )

      val thisParam =
        NodeBuilders.newThisParameterNode(typeFullName = typeDecl.fullName, dynamicTypeHintFullName = Seq())
      val thisIdentifier = newIdentifierNode(Constants.ThisName, typeDecl.fullName, Seq(typeDecl.fullName))
      val thisAst        = Ast(thisIdentifier).withRefEdge(thisIdentifier, thisParam)

      val fieldIdentifier = fieldIdentifierNode(valueParam, valueParam.getName, valueParam.getName)
      val fieldAccessCall = NodeBuilders.newOperatorCallNode(
        Operators.fieldAccess,
        s"${Constants.ThisName}.${valueParam.getName}",
        Option(typeFullName)
      )
      val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))
      val methodBlockAst = blockAst(
        blockNode(valueParam, fieldAccessCall.code, typeFullName),
        List(returnAst(returnNode(valueParam, Constants.RetCode), List(fieldAccessCallAst)))
      )

      val componentIdx  = idx + 1
      val componentName = s"${Constants.ComponentNPrefix}$componentIdx"
      val signature     = s"$typeFullName()"
      val fullName      = s"${typeDecl.fullName}.$componentName:$signature"
      methodAst(
        methodNode(valueParam, componentName, fullName, signature, relativizedPath),
        Seq(Ast(thisParam)),
        methodBlockAst,
        newMethodReturnNode(typeFullName, None, None, None)
      )
    }
  }

  private def secondaryCtorAsts(
    ctors: Seq[KtSecondaryConstructor],
    classFullName: String,
    primaryCtorCall: NewCall
  ): Seq[Ast] = {
    ctors.map { ctor =>
      val primaryCtorCallAst = List(Ast(primaryCtorCall.copy))
      val constructorParams  = ctor.getValueParameters.asScala.toList

      val constructorDesc = bindingUtils.getConstructorDesc(ctor)
      val descFullName = nameRenderer
        .descFullName(constructorDesc)
        .getOrElse(s"$classFullName.${Defines.ConstructorMethodName}")
      val signature = nameRenderer
        .funcDescSignature(constructorDesc)
        .getOrElse(s"${Defines.UnresolvedSignature}(${ctor.getValueParameters.size()})")
      val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)
      val secondaryCtorMethodNode =
        methodNode(ctor, Defines.ConstructorMethodName, fullName, signature, relativizedPath)
      scope.pushNewScope(secondaryCtorMethodNode)

      val ctorThisParam =
        NodeBuilders.newThisParameterNode(typeFullName = classFullName, dynamicTypeHintFullName = Seq(classFullName))
      scope.addToScope(Constants.ThisName, ctorThisParam)

      val constructorParamsAsts = Seq(Ast(ctorThisParam)) ++
        withIndex(constructorParams) { (p, idx) => astForParameter(p, idx) }

      val ctorMethodBlockAsts =
        ctor.getBodyExpression match {
          case b: KtBlockExpression =>
            astsForBlock(b, None, None, preStatements = Option(primaryCtorCallAst))
          case null =>
            val node = NewBlock().code(Constants.Empty).typeFullName(TypeConstants.Any)
            Seq(blockAst(node, primaryCtorCallAst))
        }
      scope.popScope()

      val ctorMethodReturnNode =
        newMethodReturnNode(TypeConstants.Void, None, line(ctor), column(ctor))

      // TODO: see if necessary to take the other asts for the ctorMethodBlock
      methodAst(
        secondaryCtorMethodNode,
        constructorParamsAsts,
        ctorMethodBlockAsts.headOption.getOrElse(Ast(unknownNode(ctor.getBodyExpression, Constants.Empty))),
        ctorMethodReturnNode,
        Seq(newModifierNode(ModifierTypes.CONSTRUCTOR))
      )
    }
  }

  protected def astForObjectLiteralExpr(
    expr: KtObjectLiteralExpression,
    argIdxMaybe: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val parentFn = KtPsiUtil.getTopmostParentOfTypes(expr, classOf[KtNamedFunction])
    val ctx =
      Option(parentFn)
        .collect { case namedFn: KtNamedFunction => namedFn }
        .map(AnonymousObjectContext(_))
        .getOrElse(AnonymousObjectContext(expr.getContainingKtFile))

    val idxOpt  = PsiUtils.objectIdxMaybe(expr.getObjectDeclaration, parentFn)
    val idx     = idxOpt.getOrElse(Random.nextInt())
    val tmpName = s"tmp_obj_$idx"

    val typeDeclAsts = astsForClassOrObject(expr.getObjectDeclaration, Some(ctx))
    val typeDeclAst  = typeDeclAsts.headOption.getOrElse(Ast(unknownNode(expr.getObjectDeclaration, Constants.Empty)))
    val typeDeclFullName = typeDeclAst.root.get.asInstanceOf[NewTypeDecl].fullName

    val localForTmp = localNode(expr, tmpName, tmpName, typeDeclFullName)
    scope.addToScope(tmpName, localForTmp)
    val localAst = Ast(localForTmp)

    val rhsAst = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, None))

    val identifier    = identifierNode(expr, tmpName, tmpName, localForTmp.typeFullName)
    val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

    val assignmentNode = NodeBuilders.newOperatorCallNode(
      Operators.assignment,
      s"${identifier.name} = <alloc>",
      None,
      line(expr),
      column(expr)
    )
    val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))
    val initSignature     = s"${TypeConstants.Void}()"
    val initFullName      = s"$typeDeclFullName.${Defines.ConstructorMethodName}:$initSignature"
    val initCallNode =
      callNode(
        expr,
        Defines.ConstructorMethodName,
        Defines.ConstructorMethodName,
        initFullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(initSignature),
        Some(TypeConstants.Void)
      )

    val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
    val initReceiverAst =
      Ast(initReceiverNode).withRefEdge(initReceiverNode, localForTmp)
    val initAst = callAst(initCallNode, Seq(), Option(initReceiverAst))

    val refTmpNode = identifierNode(expr, tmpName, tmpName, localForTmp.typeFullName)
    val refTmpAst  = astWithRefEdgeMaybe(refTmpNode.name, refTmpNode)

    val blockNode_ =
      withArgumentIndex(blockNode(expr, expr.getText, TypeConstants.Any), argIdxMaybe)
        .argumentName(argNameMaybe)
    blockAst(blockNode_, Seq(typeDeclAst, localAst, assignmentCallAst, initAst, refTmpAst).toList)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astsForProperty(expr: KtProperty, annotations: Seq[KtAnnotationEntry] = Seq()): Seq[Ast] = {
    val explicitTypeName = Option(expr.getTypeReference).map(_.getText).getOrElse(TypeConstants.Any)
    val elem             = expr.getIdentifyingElement

    val hasRHSCtorCall =
      Option(expr.getDelegateExpressionOrInitializer).flatMap(typeInfoProvider.isConstructorCall).getOrElse(false)
    val ctorCallExprMaybe =
      if (hasRHSCtorCall) {
        expr.getDelegateExpressionOrInitializer match {
          case c: KtCallExpression => Some(c)
          case q: KtQualifiedExpression =>
            q.getSelectorExpression match {
              case qc: KtCallExpression => Some(qc)
              case _                    => None
            }
          case _ => None
        }
      } else None
    val hasRHSObjectLiteral = expr.getDelegateExpressionOrInitializer match {
      case _: KtObjectLiteralExpression => true
      case _                            => false
    }
    if (ctorCallExprMaybe.nonEmpty) {
      val callExpr = ctorCallExprMaybe.get
      val localTypeFullName =
        bindingUtils
          .getVariableDesc(expr)
          .flatMap(desc => nameRenderer.typeFullName(desc.getType))
          .orElse(fullNameByImportPath(expr.getTypeReference, expr.getContainingKtFile))
          .getOrElse(explicitTypeName)
      registerType(localTypeFullName)
      val local = localNode(expr, expr.getName, expr.getName, localTypeFullName)
      scope.addToScope(expr.getName, local)
      val localAst = Ast(local)

      val typeFullName = registerType(
        exprTypeFullName(expr.getDelegateExpressionOrInitializer).getOrElse(Defines.UnresolvedNamespace)
      )
      val rhsAst = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, Option(typeFullName)))

      val identifier    = identifierNode(elem, elem.getText, elem.getText, local.typeFullName)
      val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

      val assignmentNode =
        NodeBuilders.newOperatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
      val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))

      val (fullName, signature) =
        calleeFullnameAndSignature(
          getCalleeExpr(callExpr),
          Defines.UnresolvedNamespace,
          s"${Defines.UnresolvedSignature}(${callExpr.getValueArguments.size()})"
        )
      val initCallNode = callNode(
        callExpr,
        callExpr.getText,
        Defines.ConstructorMethodName,
        fullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(signature),
        Some(TypeConstants.Void)
      )
      val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
      val initReceiverAst  = Ast(initReceiverNode).withRefEdge(initReceiverNode, local)
      val argAsts          = astsForKtCallExpressionArguments(callExpr)

      val initAst =
        callAst(initCallNode, argAsts, Option(initReceiverAst))
          .withChildren(annotations.map(astForAnnotationEntry))
      Seq(localAst, assignmentCallAst, initAst)
    } else if (hasRHSObjectLiteral) {
      val typedExpr = expr.getDelegateExpressionOrInitializer.asInstanceOf[KtObjectLiteralExpression]
      val parentFn  = KtPsiUtil.getTopmostParentOfTypes(expr, classOf[KtNamedFunction])
      val ctx =
        Option(parentFn)
          .collect { case namedFn: KtNamedFunction => namedFn }
          .map(AnonymousObjectContext(_))
          .getOrElse(AnonymousObjectContext(expr.getContainingKtFile))

      val typeDeclAsts = astsForClassOrObject(typedExpr.getObjectDeclaration, Some(ctx))
      val typeDeclAst =
        typeDeclAsts.headOption.getOrElse(Ast(unknownNode(typedExpr.getObjectDeclaration, Constants.Empty)))
      val typeDeclFullName = typeDeclAst.root.get.asInstanceOf[NewTypeDecl].fullName

      val node = localNode(expr, expr.getName, expr.getName, typeDeclFullName)
      scope.addToScope(expr.getName, node)
      val localAst = Ast(node)

      val typeFullName = registerType(
        exprTypeFullName(expr.getDelegateExpressionOrInitializer).getOrElse(Defines.UnresolvedNamespace)
      )
      val rhsAst = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, None))

      val identifier    = identifierNode(elem, elem.getText, elem.getText, node.typeFullName)
      val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

      val assignmentNode =
        NodeBuilders.newOperatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
      val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))
      val initSignature     = s"${TypeConstants.Void}()"
      val initFullName      = s"$typeFullName${Defines.ConstructorMethodName}:$initSignature"
      val initCallNode = callNode(
        expr,
        Defines.ConstructorMethodName,
        Defines.ConstructorMethodName,
        initFullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(initSignature),
        Some(TypeConstants.Void)
      )
      val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
      val initReceiverAst  = Ast(initReceiverNode).withRefEdge(initReceiverNode, node)

      val initAst =
        callAst(initCallNode, Seq(), Option(initReceiverAst))
          .withChildren(annotations.map(astForAnnotationEntry))
      Seq(typeDeclAst, localAst, assignmentCallAst, initAst)
    } else {
      val typeFullName = bindingUtils
        .getVariableDesc(expr)
        .flatMap(desc => nameRenderer.typeFullName(desc.getType))
        .orElse(fullNameByImportPath(expr.getTypeReference, expr.getContainingKtFile))
        .getOrElse(explicitTypeName)
      registerType(typeFullName)
      val node = localNode(expr, expr.getName, expr.getName, typeFullName)
      scope.addToScope(expr.getName, node)
      val localAst = Ast(node)

      val rhsAsts       = astsForExpression(expr.getDelegateExpressionOrInitializer, Some(2))
      val identifier    = identifierNode(elem, elem.getText, elem.getText, typeFullName)
      val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)
      val assignmentNode =
        NodeBuilders.newOperatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
      val call =
        callAst(assignmentNode, List(identifierAst) ++ rhsAsts)
          .withChildren(annotations.map(astForAnnotationEntry))
      Seq(localAst, call)
    }
  }

  private def astForMember(decl: KtDeclaration): Ast = {
    val name = Option(decl.getName).getOrElse(TypeConstants.Any)
    val explicitTypeName = decl.getOriginalElement match {
      case p: KtProperty if p.getTypeReference != null => p.getTypeReference.getText
      case _                                           => TypeConstants.Any
    }
    val typeFullName = decl match {
      case typed: KtProperty =>
        bindingUtils
          .getVariableDesc(typed)
          .flatMap(desc => nameRenderer.typeFullName(desc.getType))
          .orElse(fullNameByImportPath(typed.getTypeReference, typed.getContainingKtFile))
          .getOrElse(explicitTypeName)
      case _ => explicitTypeName
    }
    registerType(typeFullName)

    val node = memberNode(decl, name, name, typeFullName)
    scope.addToScope(name, node)
    Ast(node)
  }

}
