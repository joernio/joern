package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.psi.PsiUtils
import io.joern.kotlin2cpg.psi.PsiUtils.nonUnderscoreDestructuringEntries
import io.joern.kotlin2cpg.types.AnonymousObjectContext
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.kotlin2cpg.types.TypeInfoProvider
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

import scala.jdk.CollectionConverters.*
import scala.util.Random

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def isAbstract(ktClass: KtClassOrObject)(implicit typeInfoProvider: TypeInfoProvider): Boolean = {
    typeInfoProvider.modality(ktClass).contains(Modality.ABSTRACT)
  }

  def astsForClassOrObject(
    ktClass: KtClassOrObject,
    ctx: Option[AnonymousObjectContext] = None,
    annotations: Seq[KtAnnotationEntry] = Seq()
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val className = ctx match {
      case Some(_) => "anonymous_obj"
      case None    => ktClass.getName
    }

    val explicitFullName = {
      val fqName = ktClass.getContainingKtFile.getPackageFqName.toString
      s"$fqName.$className"
    }
    val classFullName = registerType(typeInfoProvider.fullName(ktClass, explicitFullName, ctx))
    val explicitBaseTypeFullNames = ktClass.getSuperTypeListEntries.asScala
      .map(_.getTypeAsUserType)
      .collect { case t if t != null => t.getText }
      .map { typ => typeInfoProvider.typeFromImports(typ, ktClass.getContainingKtFile).getOrElse(typ) }
      .toList

    val baseTypeFullNames = typeInfoProvider.inheritanceTypes(ktClass, explicitBaseTypeFullNames)
    baseTypeFullNames.foreach(registerType)
    val outBaseTypeFullNames = Option(baseTypeFullNames).filter(_.nonEmpty).getOrElse(Seq(TypeConstants.javaLangObject))
    val typeDecl = typeDeclNode(ktClass, className, classFullName, relativizedPath, outBaseTypeFullNames, None)
    scope.pushNewScope(typeDecl)
    methodAstParentStack.push(typeDecl)

    val primaryCtor       = ktClass.getPrimaryConstructor
    val constructorParams = ktClass.getPrimaryConstructorParameters.asScala.toList
    val defaultSignature = Option(primaryCtor)
      .map { _ => typeInfoProvider.anySignature(constructorParams) }
      .getOrElse(s"${TypeConstants.void}()")
    val defaultFullName       = s"$classFullName.${TypeConstants.initPrefix}:$defaultSignature"
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(primaryCtor, (defaultFullName, defaultSignature))
    val primaryCtorMethodNode = methodNode(primaryCtor, TypeConstants.initPrefix, fullName, signature, relativizedPath)
    val ctorThisParam =
      NodeBuilders.newThisParameterNode(typeFullName = classFullName, dynamicTypeHintFullName = Seq(classFullName))
    scope.addToScope(Constants.this_, ctorThisParam)

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

        val thisIdentifier = newIdentifierNode(Constants.this_, classFullName, Seq(classFullName))
        val thisAst        = astWithRefEdgeMaybe(Constants.this_, thisIdentifier)

        val fieldIdentifier = fieldIdentifierNode(decl, decl.getName, decl.getName)
        val fieldAccessCall = NodeBuilders
          .newOperatorCallNode(Operators.fieldAccess, s"${Constants.this_}.${fieldIdentifier.canonicalName}", None)
        val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))

        val assignmentNode = NodeBuilders
          .newOperatorCallNode(Operators.assignment, s"${fieldAccessCall.code} = ${decl.getInitializer.getText}")
        callAst(assignmentNode, List(fieldAccessCallAst, rhsAst))
      }

    val anonymousInitExpressions = ktClass.getAnonymousInitializers.asScala
    val anonymousInitAsts        = anonymousInitExpressions.flatMap(astsForExpression(_, None))

    val constructorMethodReturn = newMethodReturnNode(
      TypeConstants.void,
      None,
      line(ktClass.getPrimaryConstructor),
      column(ktClass.getPrimaryConstructor)
    )
    val constructorAst = methodAst(
      primaryCtorMethodNode,
      constructorParamsAsts,
      blockAst(
        blockNode(ktClass, "", TypeConstants.void),
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
        val typeFullName = registerType(typeInfoProvider.parameterType(param, TypeConstants.any))
        val memberNode_  = memberNode(param, param.getName, param.getName, typeFullName)
        scope.addToScope(param.getName, memberNode_)
        Ast(memberNode_)
    }

    val primaryCtorCall =
      callNode(
        ktClass.getPrimaryConstructor,
        TypeConstants.initPrefix,
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
        .filterNot(typeInfoProvider.isCompanionObject)
        .flatMap(astsForDeclaration(_))

    val classFunctions = Option(ktClass.getBody)
      .map(_.getFunctions.asScala.collect { case f: KtNamedFunction => f })
      .getOrElse(List())
    val methodAsts = classFunctions.toSeq.flatMap { classFn =>
      astsForMethod(classFn, needsThisParameter = true, withVirtualModifier = true)
    }
    val bindingsInfo = methodAsts.flatMap(_.root.collectAll[NewMethod]).map { _methodNode =>
      val node = newBindingNode(_methodNode.name, _methodNode.signature, _methodNode.fullName)
      BindingInfo(node, List((typeDecl, node, EdgeTypes.BINDS), (node, _methodNode, EdgeTypes.REF)))
    }

    val annotationAsts = ktClass.getAnnotationEntries.asScala.map(astForAnnotationEntry).toSeq

    val modifiers = if (isAbstract(ktClass)) List(Ast(NodeBuilders.newModifierNode(ModifierTypes.ABSTRACT))) else Nil

    val children = methodAsts ++ List(constructorAst) ++ membersFromPrimaryCtorAsts ++ secondaryConstructorAsts ++
      _componentNMethodAsts.toList ++ memberAsts ++ annotationAsts ++ modifiers
    val ast = Ast(typeDecl).withChildren(children)

    (List(ctorBindingInfo) ++ bindingsInfo ++ componentNBindingsInfo).foreach(bindingInfoQueue.prepend)

    val finalAst = if (typeInfoProvider.isCompanionObject(ktClass)) {
      val companionMemberTypeFullName = ktClass.getParent.getParent match {
        case c: KtClassOrObject => typeInfoProvider.typeFullName(c, TypeConstants.any)
        case _                  => TypeConstants.any
      }
      registerType(companionMemberTypeFullName)

      val companionObjectMember = memberNode(
        ktClass,
        Constants.companionObjectMemberName,
        Constants.companionObjectMemberName,
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

  private def memberSetCallAst(param: KtParameter, classFullName: String)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Ast = {
    val typeFullName       = registerType(typeInfoProvider.typeFullName(param, TypeConstants.any))
    val paramName          = param.getName
    val paramIdentifier    = identifierNode(param, paramName, paramName, typeFullName)
    val paramIdentifierAst = astWithRefEdgeMaybe(paramName, paramIdentifier)
    val thisIdentifier     = newIdentifierNode(Constants.this_, classFullName, Seq(classFullName))
    val thisAst            = astWithRefEdgeMaybe(Constants.this_, thisIdentifier)

    val fieldIdentifier = fieldIdentifierNode(param, paramName, paramName)
    val fieldAccessCall =
      NodeBuilders.newOperatorCallNode(Operators.fieldAccess, s"${Constants.this_}.$paramName", Option(typeFullName))
    val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))

    val assignmentNode =
      NodeBuilders.newOperatorCallNode(Operators.assignment, s"${fieldAccessCall.code} = ${paramIdentifier.code}")
    callAst(assignmentNode, List(fieldAccessCallAst, paramIdentifierAst))
  }

  private def astsForDestructuringDeclarationWithRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
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
    val callRhsTypeFullName = registerType(typeInfoProvider.expressionType(rhsCall, TypeConstants.any))

    val destructuringEntries = nonUnderscoreDestructuringEntries(expr)
    val localsForEntries = destructuringEntries.map { entry =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val node         = localNode(entry, entry.getName, entry.getName, typeFullName)
      scope.addToScope(node.name, node)
      Ast(node)
    }

    val isCtor = expr.getInitializer match {
      case _: KtCallExpression => typeInfoProvider.isConstructorCall(rhsCall).getOrElse(false)
      case _                   => false
    }
    val tmpName         = s"${Constants.tmpLocalPrefix}${tmpKeyPool.next}"
    val localForTmpNode = localNode(expr, tmpName, tmpName, callRhsTypeFullName)
    scope.addToScope(localForTmpNode.name, localForTmpNode)
    val localForTmpAst = Ast(localForTmpNode)

    val assignmentLhsNode = identifierNode(expr, tmpName, tmpName, localForTmpNode.typeFullName)
    val assignmentLhsAst  = Ast(assignmentLhsNode).withRefEdge(assignmentLhsNode, localForTmpNode)
    val tmpAssignmentAst =
      if (isCtor) {
        val assignmentRhsNode = NodeBuilders.newOperatorCallNode(
          Operators.alloc,
          Constants.alloc,
          Option(localForTmpNode.typeFullName),
          line(expr),
          column(expr)
        )
        val assignmentNode =
          NodeBuilders.newOperatorCallNode(Operators.assignment, s"$tmpName  = ${Constants.alloc}", None)
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
              astsForExpression(rhsCall, None).headOption.getOrElse(Ast(unknownNode(rhsCall, Constants.empty)))
            callAst(assignmentNode, List(assignmentLhsAst, assignmentRhsAst))
        }
      }
    val tmpAssignmentPrologue = rhsCall match {
      case call: KtCallExpression if isCtor =>
        val initReceiverNode =
          identifierNode(expr, tmpName, tmpName, localForTmpNode.typeFullName).argumentIndex(0)
        val initReceiverAst = Ast(initReceiverNode).withRefEdge(initReceiverNode, localForTmpNode)

        val argAsts = withIndex(call.getValueArguments.asScala.toSeq) { case (arg, idx) =>
          astsForExpression(arg.getArgumentExpression, Some(idx))
        }.flatten

        val (fullName, signature) = typeInfoProvider.fullNameWithSignature(call, (TypeConstants.any, TypeConstants.any))
        registerType(typeInfoProvider.expressionType(expr, TypeConstants.any))
        val initCallNode = callNode(
          expr,
          Constants.init,
          Constants.init,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(TypeConstants.void)
        )
        Seq(callAst(initCallNode, argAsts, Some(initReceiverAst), None))
      case _ => Seq()
    }

    val assignmentsForEntries = destructuringEntries.zipWithIndex.map { case (entry, idx) =>
      assignmentAstForDestructuringEntry(entry, localForTmpNode.name, localForTmpNode.typeFullName, idx + 1)
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
  private def astsForDestructuringDeclarationWithVarRHS(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val typedInit = Option(expr.getInitializer).collect { case e: KtNameReferenceExpression => e }
    if (typedInit.isEmpty) {
      logger.warn(s"Unhandled case for destructuring declaration: `${expr.getText}` in this file `$relativizedPath`.")
      return Seq()
    }
    val destructuringRHS = typedInit.get

    val initTypeFullName = registerType(typeInfoProvider.typeFullName(typedInit.get, TypeConstants.any))
    val assignmentsForEntries =
      nonUnderscoreDestructuringEntries(expr).zipWithIndex.map { case (entry, idx) =>
        assignmentAstForDestructuringEntry(entry, destructuringRHS.getText, initTypeFullName, idx + 1)
      }
    val localsForEntries = nonUnderscoreDestructuringEntries(expr).map { entry =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(entry, TypeConstants.any))
      val node         = localNode(entry, entry.getName, entry.getName, typeFullName)
      scope.addToScope(node.name, node)
      Ast(node)
    }
    localsForEntries ++ assignmentsForEntries
  }

  def astsForDestructuringDeclaration(
    expr: KtDestructuringDeclaration
  )(implicit typeInfoProvider: TypeInfoProvider): Seq[Ast] = {
    val hasNonRefExprRHS = expr.getInitializer match {
      case _: KtNameReferenceExpression => false
      case _: KtExpression              => true
      case null                         => false
    }
    if (hasNonRefExprRHS) astsForDestructuringDeclarationWithRHS(expr)
    else astsForDestructuringDeclarationWithVarRHS(expr)
  }

  private def componentNMethodAsts(typeDecl: NewTypeDecl, parameters: Seq[KtParameter])(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    parameters.zipWithIndex.map { case (valueParam, idx) =>
      val typeFullName = registerType(typeInfoProvider.typeFullName(valueParam, TypeConstants.any))

      val thisParam =
        NodeBuilders.newThisParameterNode(typeFullName = typeDecl.fullName, dynamicTypeHintFullName = Seq())
      val thisIdentifier = newIdentifierNode(Constants.this_, typeDecl.fullName, Seq(typeDecl.fullName))
      val thisAst        = Ast(thisIdentifier).withRefEdge(thisIdentifier, thisParam)

      val fieldIdentifier = fieldIdentifierNode(valueParam, valueParam.getName, valueParam.getName)
      val fieldAccessCall = NodeBuilders.newOperatorCallNode(
        Operators.fieldAccess,
        s"${Constants.this_}.${valueParam.getName}",
        Option(typeFullName)
      )
      val fieldAccessCallAst = callAst(fieldAccessCall, List(thisAst, Ast(fieldIdentifier)))
      val methodBlockAst = blockAst(
        blockNode(valueParam, fieldAccessCall.code, typeFullName),
        List(returnAst(returnNode(valueParam, Constants.ret), List(fieldAccessCallAst)))
      )

      val componentIdx  = idx + 1
      val componentName = s"${Constants.componentNPrefix}$componentIdx"
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

  private def secondaryCtorAsts(ctors: Seq[KtSecondaryConstructor], classFullName: String, primaryCtorCall: NewCall)(
    implicit typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    ctors.map { ctor =>
      val primaryCtorCallAst    = List(Ast(primaryCtorCall.copy))
      val constructorParams     = ctor.getValueParameters.asScala.toList
      val defaultSignature      = typeInfoProvider.anySignature(constructorParams)
      val defaultFullName       = s"$classFullName.${TypeConstants.initPrefix}:$defaultSignature"
      val (fullName, signature) = typeInfoProvider.fullNameWithSignature(ctor, (defaultFullName, defaultSignature))
      val secondaryCtorMethodNode =
        methodNode(ctor, Constants.init, fullName, signature, relativizedPath)
      scope.pushNewScope(secondaryCtorMethodNode)

      val ctorThisParam =
        NodeBuilders.newThisParameterNode(typeFullName = classFullName, dynamicTypeHintFullName = Seq(classFullName))
      scope.addToScope(Constants.this_, ctorThisParam)

      val constructorParamsAsts = Seq(Ast(ctorThisParam)) ++
        withIndex(constructorParams) { (p, idx) => astForParameter(p, idx) }

      val ctorMethodBlockAsts =
        ctor.getBodyExpression match {
          case b: KtBlockExpression =>
            astsForBlock(b, None, None, preStatements = Option(primaryCtorCallAst))
          case null =>
            val node = NewBlock().code(Constants.empty).typeFullName(TypeConstants.any)
            Seq(blockAst(node, primaryCtorCallAst))
        }
      scope.popScope()

      val ctorMethodReturnNode =
        newMethodReturnNode(TypeConstants.void, None, line(ctor), column(ctor))

      // TODO: see if necessary to take the other asts for the ctorMethodBlock
      methodAst(
        secondaryCtorMethodNode,
        constructorParamsAsts,
        ctorMethodBlockAsts.headOption.getOrElse(Ast(unknownNode(ctor.getBodyExpression, Constants.empty))),
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
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
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
    val typeDeclAst  = typeDeclAsts.headOption.getOrElse(Ast(unknownNode(expr.getObjectDeclaration, Constants.empty)))
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
    val initSignature     = s"${TypeConstants.void}()"
    val initFullName      = s"$typeDeclFullName.${TypeConstants.initPrefix}:$initSignature"
    val initCallNode =
      callNode(
        expr,
        Constants.init,
        Constants.init,
        initFullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(initSignature),
        Some(TypeConstants.void)
      )

    val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
    val initReceiverAst =
      Ast(initReceiverNode).withRefEdge(initReceiverNode, localForTmp)
    val initAst = callAst(initCallNode, Seq(), Option(initReceiverAst))

    val refTmpNode = identifierNode(expr, tmpName, tmpName, localForTmp.typeFullName)
    val refTmpAst  = astWithRefEdgeMaybe(refTmpNode.name, refTmpNode)

    val blockNode_ =
      withArgumentIndex(blockNode(expr, expr.getText, TypeConstants.any), argIdxMaybe)
        .argumentName(argNameMaybe)
    blockAst(blockNode_, Seq(typeDeclAst, localAst, assignmentCallAst, initAst, refTmpAst).toList)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astsForProperty(expr: KtProperty, annotations: Seq[KtAnnotationEntry] = Seq())(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    val explicitTypeName = Option(expr.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
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
      val callExpr          = ctorCallExprMaybe.get
      val localTypeFullName = registerType(typeInfoProvider.propertyType(expr, explicitTypeName))
      val local             = localNode(expr, expr.getName, expr.getName, localTypeFullName)
      scope.addToScope(expr.getName, local)
      val localAst = Ast(local)

      val typeFullName = registerType(
        typeInfoProvider.expressionType(expr.getDelegateExpressionOrInitializer, Defines.UnresolvedNamespace)
      )
      val rhsAst = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, Option(typeFullName)))

      val identifier    = identifierNode(elem, elem.getText, elem.getText, local.typeFullName)
      val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

      val assignmentNode =
        NodeBuilders.newOperatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
      val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))

      val (fullName, signature) =
        typeInfoProvider.fullNameWithSignature(callExpr, (TypeConstants.any, TypeConstants.any))
      val initCallNode = callNode(
        callExpr,
        callExpr.getText,
        Constants.init,
        fullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(signature),
        Some(TypeConstants.void)
      )
      val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
      val initReceiverAst  = Ast(initReceiverNode).withRefEdge(initReceiverNode, local)

      val argAsts = withIndex(callExpr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
        val argNameOpt = if (arg.isNamed) Option(arg.getArgumentName.getAsName.toString) else None
        astsForExpression(arg.getArgumentExpression, Option(idx), argNameOpt)
      }.flatten

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
        typeDeclAsts.headOption.getOrElse(Ast(unknownNode(typedExpr.getObjectDeclaration, Constants.empty)))
      val typeDeclFullName = typeDeclAst.root.get.asInstanceOf[NewTypeDecl].fullName

      val node = localNode(expr, expr.getName, expr.getName, typeDeclFullName)
      scope.addToScope(expr.getName, node)
      val localAst = Ast(node)

      val typeFullName = registerType(
        typeInfoProvider.expressionType(expr.getDelegateExpressionOrInitializer, Defines.UnresolvedNamespace)
      )
      val rhsAst = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, None))

      val identifier    = identifierNode(elem, elem.getText, elem.getText, node.typeFullName)
      val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

      val assignmentNode =
        NodeBuilders.newOperatorCallNode(Operators.assignment, expr.getText, None, line(expr), column(expr))
      val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))
      val initSignature     = s"${TypeConstants.void}()"
      val initFullName      = s"$typeFullName${TypeConstants.initPrefix}:$initSignature"
      val initCallNode = callNode(
        expr,
        Constants.init,
        Constants.init,
        initFullName,
        DispatchTypes.STATIC_DISPATCH,
        Some(initSignature),
        Some(TypeConstants.void)
      )
      val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
      val initReceiverAst  = Ast(initReceiverNode).withRefEdge(initReceiverNode, node)

      val initAst =
        callAst(initCallNode, Seq(), Option(initReceiverAst))
          .withChildren(annotations.map(astForAnnotationEntry))
      Seq(typeDeclAst, localAst, assignmentCallAst, initAst)
    } else {
      val typeFullName = registerType(typeInfoProvider.propertyType(expr, explicitTypeName))
      val node         = localNode(expr, expr.getName, expr.getName, typeFullName)
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

  private def astForMember(decl: KtDeclaration)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name = Option(decl.getName).getOrElse(TypeConstants.any)
    val explicitTypeName = decl.getOriginalElement match {
      case p: KtProperty if p.getTypeReference != null => p.getTypeReference.getText
      case _                                           => TypeConstants.any
    }
    val typeFullName = decl match {
      case typed: KtProperty => typeInfoProvider.propertyType(typed, explicitTypeName)
      case _                 => explicitTypeName
    }
    registerType(typeFullName)

    val node = memberNode(decl, name, name, typeFullName)
    scope.addToScope(name, node)
    Ast(node)
  }

}
