package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.AstNodeBuilder.{bindingNode, closureBindingNode}
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.descriptors.ParameterDescriptor
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.calls.model.KotlinCallArgument
import org.jetbrains.kotlin.resolve.calls.tower.NewAbstractResolvedCall
import org.jetbrains.kotlin.resolve.calls.tower.PSIFunctionKotlinCallArgument
import org.jetbrains.kotlin.resolve.sam.SamConstructorDescriptor
import org.jetbrains.kotlin.resolve.sam.SamConversionResolverImplKt
import org.jetbrains.kotlin.resolve.source.KotlinSourceElement

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  import AstCreator.ClosureBindingDef

  private def createFunctionTypeAndTypeDeclAst(
    node: KtNamedFunction,
    methodNode: NewMethod,
    parentNode: NewNode,
    methodName: String,
    methodFullName: String,
    signature: String,
    filename: String
  ): Ast = {
    val astParentType     = parentNode.label
    val astParentName     = parentNode.properties(Properties.Name.name).toString
    val astParentFullName = parentNode.properties(Properties.FullName.name).toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        Seq(TypeConstants.KotlinFunctionPrefix)
      )
    if (astParentName == NamespaceTraversal.globalNamespaceName || astParentType == Method.Label) {
      // Bindings for others (classes, interfaces, and such) are already created in their respective CPG creation functions
      val bindingNode = NewBinding().name(methodName).methodFullName(methodFullName).signature(signature)
      Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
    } else {
      Ast(functionTypeDeclNode)
    }
  }

  def astForMethod(ktFn: KtNamedFunction, withVirtualModifier: Boolean = false): Ast = {
    val funcDesc = bindingUtils.getFunctionDesc(ktFn)
    val descFullName = nameRenderer
      .descFullName(funcDesc)
      .getOrElse(s"${Defines.UnresolvedNamespace}.${ktFn.getName}")
    val signature = nameRenderer
      .funcDescSignature(funcDesc)
      .getOrElse(s"${Defines.UnresolvedSignature}(${ktFn.getValueParameters.size()})")
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val _methodNode = methodNode(ktFn, ktFn.getName, fullName, signature, relativizedPath)
    scope.pushNewScope(_methodNode)
    methodAstParentStack.push(_methodNode)

    val isExtensionMethod = funcDesc.getExtensionReceiverParameter != null

    val needsThisParameter = funcDesc.getDispatchReceiverParameter != null ||
      isExtensionMethod

    val thisParameterAsts = if (needsThisParameter) {
      val typeDeclFullName =
        if (funcDesc.getDispatchReceiverParameter != null) {
          nameRenderer.typeFullName(funcDesc.getDispatchReceiverParameter.getType).getOrElse(TypeConstants.Any)
        } else {
          nameRenderer.typeFullName(funcDesc.getExtensionReceiverParameter.getType).getOrElse(TypeConstants.Any)
        }

      registerType(typeDeclFullName)

      val thisParam = parameterInNode(
        ktFn,
        Constants.ThisName,
        Constants.ThisName,
        index = 0,
        isVariadic = false,
        typeFullName = Option(typeDeclFullName),
        dynamicTypeHintFullName = Seq(typeDeclFullName),
        evaluationStrategy = EvaluationStrategies.BY_SHARING
      )
      if (isExtensionMethod) {
        thisParam.order(1)
        thisParam.index(1)
      }
      scope.addToScope(Constants.ThisName, thisParam)
      List(Ast(thisParam))
    } else {
      List.empty
    }

    val valueParamStartIndex =
      if (isExtensionMethod) {
        2
      } else {
        1
      }

    val methodParametersAsts =
      withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, idx) =>
        astForParameter(p, valueParamStartIndex + idx - 1)
      }
    val bodyAst = Option(ktFn.getBodyBlockExpression) match {
      case Some(bodyBlockExpression) => astForBlock(bodyBlockExpression, None, None)
      case None =>
        Option(ktFn.getBodyExpression)
          .map { expr =>
            val bodyBlock = blockNode(expr, expr.getText, TypeConstants.Any)
            val asts      = astsForExpression(expr, Some(1))
            val blockChildAsts =
              if (asts.nonEmpty) {
                val allStatementsButLast = asts.dropRight(1)
                val lastStatementAst     = asts.lastOption.getOrElse(Ast(unknownNode(expr, Constants.Empty)))
                val returnAst_           = returnAst(returnNode(expr, Constants.RetCode), Seq(lastStatementAst))
                (allStatementsButLast ++ Seq(returnAst_)).toList
              } else List()
            blockAst(bodyBlock, blockChildAsts)
          }
          .getOrElse {
            val bodyBlock = blockNode(ktFn, "<empty>", TypeConstants.Any)
            blockAst(bodyBlock, List[Ast]())
          }
    }
    methodAstParentStack.pop()
    scope.popScope()

    val explicitTypeName  = Option(ktFn.getTypeReference).map(_.getText).getOrElse(TypeConstants.Any)
    val typeFullName      = registerType(nameRenderer.typeFullName(funcDesc.getReturnType).getOrElse(explicitTypeName))
    val _methodReturnNode = methodReturnNode(ktFn, typeFullName)

    val visibilityModifierType =
      modifierTypeForVisibility(funcDesc.getVisibility)
    val visibilityModifier = modifierNode(ktFn, visibilityModifierType)

    val modifierNodes =
      if (withVirtualModifier) Seq(modifierNode(ktFn, ModifierTypes.VIRTUAL))
      else Seq()

    val modifiers = if (funcDesc.getModality == Modality.ABSTRACT) {
      List(visibilityModifier) ++ modifierNodes :+ modifierNode(ktFn, ModifierTypes.ABSTRACT)
    } else {
      List(visibilityModifier) ++ modifierNodes
    }

    val functionTypeAndTypeDeclAst = createFunctionTypeAndTypeDeclAst(
      ktFn,
      _methodNode,
      methodAstParentStack.head,
      ktFn.getName,
      fullName,
      signature,
      relativizedPath
    )
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)

    val annotationEntries = ktFn.getAnnotationEntries.asScala.map(astForAnnotationEntry).toSeq
    methodAst(_methodNode, thisParameterAsts ++ methodParametersAsts, bodyAst, _methodReturnNode, modifiers)
      .withChildren(annotationEntries)
  }

  private def astsForDestructuring(param: KtParameter): Seq[Ast] = {
    val decl             = param.getDestructuringDeclaration
    val tmpName          = s"${Constants.TmpLocalPrefix}${tmpKeyPool.next}"
    var localForTmp      = Option.empty[NewLocal]
    val additionalLocals = mutable.ArrayBuffer.empty[Ast]

    val initCallAst = if (decl.hasInitializer) {
      val init = decl.getInitializer
      val asts = astsForExpression(init, Some(2))
      val initAst =
        if (asts.size == 1) { asts.head }
        else {
          val block = blockNode(init, "", "")
          blockAst(block, asts.toList)
        }
      val local = localNode(decl, tmpName, tmpName, TypeConstants.Any)
      localForTmp = Some(local)
      scope.addToScope(tmpName, local)
      val tmpIdentifier      = identifierNode(param, tmpName, tmpName, TypeConstants.Any)
      val tmpIdentifierAst   = Ast(tmpIdentifier).withRefEdge(tmpIdentifier, local)
      val assignmentCallNode = operatorCallNode(init, s"$tmpName = ${init.getText}", Operators.assignment, None)
      callAst(assignmentCallNode, List(tmpIdentifierAst, initAst))
    } else {
      val explicitTypeName = Option(param.getTypeReference)
        .map(typeRef => fullNameByImportPath(typeRef, param.getContainingKtFile).getOrElse(typeRef.getText))
        .getOrElse(TypeConstants.Any)
      val typeFullName = registerType(
        nameRenderer.typeFullName(bindingUtils.getVariableDesc(param).get.getType).getOrElse(explicitTypeName)
      )
      val localForIt = localNode(decl, "it", "it", typeFullName)
      additionalLocals.addOne(Ast(localForIt))
      val identifierForIt = identifierNode(param, "it", "it", typeFullName)
      val initAst         = Ast(identifierForIt).withRefEdge(identifierForIt, localForIt)
      val tmpIdentifier   = identifierNode(param, tmpName, tmpName, typeFullName)
      val local           = localNode(decl, tmpName, tmpName, typeFullName)
      localForTmp = Some(local)
      scope.addToScope(tmpName, local)
      val tmpIdentifierAst = Ast(tmpIdentifier).withRefEdge(tmpIdentifier, local)
      val assignmentCallNode =
        operatorCallNode(decl, s"$tmpName = it", Operators.assignment, None)
      callAst(assignmentCallNode, List(tmpIdentifierAst, initAst))
    }

    val localsForDestructuringVars = localsForDestructuringEntries(decl)
    val assignmentsForEntries =
      decl.getEntries.asScala.filterNot(_.getText == Constants.UnusedDestructuringEntryText).zipWithIndex.map {
        case (entry, idx) =>
          val rhsBaseAst = astWithRefEdgeMaybe(
            tmpName,
            identifierNode(entry, tmpName, tmpName, localForTmp.map(_.typeFullName).getOrElse(TypeConstants.Any))
          )
          assignmentAstForDestructuringEntry(entry, rhsBaseAst, idx + 1)
      }

    localForTmp
      .map(l => Ast(l))
      .toSeq ++ additionalLocals ++ localsForDestructuringVars ++ (initCallAst +: assignmentsForEntries)
  }

  def astForParameter(param: KtParameter, order: Int): Ast = {
    val name = if (param.getDestructuringDeclaration != null) {
      s"${Constants.DestructedParamNamePrefix}${destructedParamKeyPool.next}"
    } else {
      param.getName
    }

    val explicitTypeName = Option(param.getTypeReference)
      .map(typeRef =>
        fullNameByImportPath(typeRef, param.getContainingKtFile)
          .getOrElse(typeRef.getText)
      )
      .getOrElse(TypeConstants.Any)
    val typeFullName = registerType(
      nameRenderer.typeFullName(bindingUtils.getVariableDesc(param).get.getType).getOrElse(explicitTypeName)
    )
    val node = parameterInNode(param, name, name, order, false, EvaluationStrategies.BY_VALUE, typeFullName)
    scope.addToScope(name, node)

    val annotations = param.getAnnotationEntries.asScala.map(astForAnnotationEntry).toSeq
    Ast(node).withChildren(annotations)
  }

  private case class NodeContext(node: NewNode, name: String, typeFullName: String)

  def astForAnonymousFunction(
    fn: KtNamedFunction,
    argIdxMaybe: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val funcDesc = bindingUtils.getFunctionDesc(fn)
    val name     = nameRenderer.descName(funcDesc)
    val descFullName = nameRenderer
      .descFullName(funcDesc)
      .getOrElse(s"${Defines.UnresolvedNamespace}.$name")
    val signature = nameRenderer
      .funcDescSignature(funcDesc)
      .getOrElse(s"${Defines.UnresolvedSignature}(${fn.getValueParameters.size()})")
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val lambdaMethodNode = methodNode(fn, name, fullName, signature, relativizedPath)

    val closureBindingEntriesForCaptured = scope
      .pushClosureScope(lambdaMethodNode)
      .collect {
        case node: NewMethodParameterIn => NodeContext(node, node.name, node.typeFullName)
        case node: NewLocal             => NodeContext(node, node.name, node.typeFullName)
      }
      .map { capturedNodeContext =>
        val closureBindingId = s"$descFullName.${capturedNodeContext.name}"
        val closureBinding   = closureBindingNode(closureBindingId, EvaluationStrategies.BY_REFERENCE)
        (closureBinding, capturedNodeContext)
      }

    val localsForCaptured = closureBindingEntriesForCaptured.map { case (closureBinding, capturedNodeContext) =>
      val node =
        localNode(
          fn,
          capturedNodeContext.name,
          capturedNodeContext.name,
          capturedNodeContext.typeFullName,
          closureBinding.closureBindingId
        )
      scope.addToScope(capturedNodeContext.name, node)
      node
    }
    val parametersAsts =
      withIndex(fn.getValueParameters.asScala.toSeq) { (p, idx) =>
        astForParameter(p, idx)
      }
    val bodyAst = Option(fn.getBodyBlockExpression) match {
      case Some(bodyBlockExpression) =>
        astForBlock(bodyBlockExpression, None, None, localsForCaptures = localsForCaptured)
      case None =>
        Option(fn.getBodyExpression)
          .map { expr =>
            val bodyBlock  = blockNode(expr, expr.getText, TypeConstants.Any)
            val returnAst_ = returnAst(returnNode(expr, Constants.RetCode), astsForExpression(expr, Some(1)))
            blockAst(bodyBlock, localsForCaptured.map(Ast(_)) ++ List(returnAst_))
          }
          .getOrElse {
            val bodyBlock = blockNode(fn, "<empty>", TypeConstants.Any)
            blockAst(bodyBlock, List[Ast]())
          }
    }

    val returnTypeFullName     = TypeConstants.JavaLangObject
    val lambdaTypeDeclFullName = fullName.split(":").head

    val lambdaMethodAst = methodAst(
      lambdaMethodNode,
      parametersAsts,
      bodyAst,
      methodReturnNode(fn, returnTypeFullName),
      modifierNode(fn, ModifierTypes.VIRTUAL) :: modifierNode(fn, ModifierTypes.LAMBDA) :: Nil
    )

    val _methodRefNode =
      withArgumentIndex(methodRefNode(fn, fn.getText, fullName, lambdaTypeDeclFullName), argIdxMaybe)
        .argumentName(argNameMaybe)

    val samInterface = getSamInterface(fn)

    val baseClassFullName = samInterface.flatMap(nameRenderer.descFullName).getOrElse(Constants.UnknownLambdaBaseClass)

    val lambdaTypeDecl = typeDeclNode(
      fn,
      Constants.LambdaTypeDeclName,
      lambdaTypeDeclFullName,
      relativizedPath,
      Seq(registerType(baseClassFullName)),
      None
    )

    createLambdaBindings(lambdaMethodNode, lambdaTypeDecl, samInterface)

    scope.popScope()
    val closureBindingDefs = closureBindingEntriesForCaptured.collect { case (closureBinding, node) =>
      ClosureBindingDef(closureBinding, _methodRefNode, node.node)
    }
    closureBindingDefs.foreach(closureBindingDefQueue.prepend)
    lambdaAstQueue.prepend(lambdaMethodAst)
    Ast(_methodRefNode)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForLambda(
    expr: KtLambdaExpression,
    argIdxMaybe: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val funcDesc = bindingUtils.getFunctionDesc(expr.getFunctionLiteral)
    val name     = nameRenderer.descName(funcDesc)
    val descFullName = nameRenderer
      .descFullName(funcDesc)
      .getOrElse(s"${Defines.UnresolvedNamespace}.$name")
    val signature = nameRenderer
      .funcDescSignature(funcDesc)
      .getOrElse(s"${Defines.UnresolvedSignature}(${expr.getFunctionLiteral.getValueParameters.size()})")
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val lambdaMethodNode = methodNode(expr, name, fullName, signature, relativizedPath)

    val closureBindingEntriesForCaptured = scope
      .pushClosureScope(lambdaMethodNode)
      .collect {
        case node: NewMethodParameterIn => NodeContext(node, node.name, node.typeFullName)
        case node: NewLocal             => NodeContext(node, node.name, node.typeFullName)
      }
      .map { capturedNodeContext =>
        val closureBindingId = s"$descFullName.${capturedNodeContext.name}"
        val closureBinding   = closureBindingNode(closureBindingId, EvaluationStrategies.BY_REFERENCE)
        (closureBinding, capturedNodeContext)
      }

    val localsForCaptured = closureBindingEntriesForCaptured.map { case (closureBinding, capturedNodeContext) =>
      val node =
        localNode(
          expr,
          capturedNodeContext.name,
          capturedNodeContext.name,
          capturedNodeContext.typeFullName,
          closureBinding.closureBindingId
        )
      scope.addToScope(capturedNodeContext.name, node)
      node
    }

    val paramAsts           = mutable.ArrayBuffer.empty[Ast]
    val destructedParamAsts = mutable.ArrayBuffer.empty[Ast]
    val valueParamStartIndex =
      if (funcDesc.getExtensionReceiverParameter != null) {
        // Lambdas which are arguments to function parameters defined
        // like `func: extendedType.(argTypes) -> returnType` have an implicit extension receiver parameter
        // which can be accessed as `this`
        paramAsts.append(createImplicitParamNode(expr, funcDesc.getExtensionReceiverParameter, "this", 1))
        2
      } else {
        1
      }

    funcDesc.getValueParameters.asScala match {
      case parameters if parameters.size == 1 && !parameters.head.getSource.isInstanceOf[KotlinSourceElement] =>
        // Here we handle the implicit `it` parameter.
        paramAsts.append(createImplicitParamNode(expr, parameters.head, "it", valueParamStartIndex))
      case parameters =>
        parameters.zipWithIndex.foreach { (paramDesc, idx) =>
          val param = paramDesc.getSource.asInstanceOf[KotlinSourceElement].getPsi.asInstanceOf[KtParameter]
          paramAsts.append(astForParameter(param, valueParamStartIndex + idx))
          if (param.getDestructuringDeclaration != null) {
            destructedParamAsts.appendAll(astsForDestructuring(param))
          }
        }
    }

    val lastChildNotReturnExpression = !expr.getBodyExpression.getLastChild.isInstanceOf[KtReturnExpression]
    val needsReturnExpression        = lastChildNotReturnExpression
    val bodyAst = Option(expr.getBodyExpression)
      .map(
        astForBlock(
          _,
          None,
          None,
          pushToScope = false,
          localsForCaptured,
          implicitReturnAroundLastStatement = needsReturnExpression,
          Some(destructedParamAsts.toSeq)
        )
      )
      .getOrElse(Ast(blockNode(expr)))

    val returnTypeFullName = registerType(
      nameRenderer.typeFullName(funcDesc.getReturnType).getOrElse(TypeConstants.JavaLangObject)
    )
    val lambdaTypeDeclFullName = fullName.split(":").head
    val lambdaMethodAst = methodAst(
      lambdaMethodNode,
      paramAsts.toSeq,
      bodyAst,
      methodReturnNode(expr, returnTypeFullName),
      modifierNode(expr, ModifierTypes.VIRTUAL) :: modifierNode(expr, ModifierTypes.LAMBDA) :: Nil
    )

    val _methodRefNode =
      withArgumentIndex(methodRefNode(expr, expr.getText, fullName, lambdaTypeDeclFullName), argIdxMaybe)
        .argumentName(argNameMaybe)

    val samInterface = getSamInterface(expr)

    val baseClassFullName = samInterface.flatMap(nameRenderer.descFullName).getOrElse(Constants.UnknownLambdaBaseClass)

    val lambdaTypeDecl = typeDeclNode(
      expr,
      Constants.LambdaTypeDeclName,
      lambdaTypeDeclFullName,
      relativizedPath,
      Seq(registerType(baseClassFullName)),
      None
    )

    createLambdaBindings(lambdaMethodNode, lambdaTypeDecl, samInterface)

    scope.popScope()
    val closureBindingDefs = closureBindingEntriesForCaptured.collect { case (closureBinding, node) =>
      ClosureBindingDef(closureBinding, _methodRefNode, node.node)
    }
    closureBindingDefs.foreach(closureBindingDefQueue.prepend)
    lambdaAstQueue.prepend(lambdaMethodAst)
    Ast(_methodRefNode).withChildren(annotations.map(astForAnnotationEntry))
  }

  private def createImplicitParamNode(
    expr: KtLambdaExpression,
    paramDesc: ParameterDescriptor,
    paramName: String,
    index: Int
  ): Ast = {
    val node = parameterInNode(
      expr,
      paramName,
      paramName,
      index,
      false,
      EvaluationStrategies.BY_REFERENCE,
      nameRenderer.typeFullName(paramDesc.getType).getOrElse(TypeConstants.Any)
    )
    scope.addToScope(paramName, node)
    Ast(node)
  }

  // SAM stands for: single abstraction method
  private def getSamInterface(expr: KtLambdaExpression | KtNamedFunction): Option[ClassDescriptor] = {
    val res = getSurroundingCallTarget(expr) match {
      case Some(callTarget) =>
        val resolvedCallAtom = bindingUtils
          .getResolvedCallDesc(callTarget)
          .collect { case call: NewAbstractResolvedCall[?] =>
            call.getResolvedCallAtom
          }

        resolvedCallAtom.flatMap { callAtom =>
          callAtom.getCandidateDescriptor match {
            case samConstructorDesc: SamConstructorDescriptor =>
              // Lambda is wrapped e.g. `SomeInterface { obj -> obj }`
              Some(samConstructorDesc.getBaseDescriptorForSynthetic)
            case _ =>
              // Lambda/anon function is directly used as call argument e.g. `someCall(obj -> obj)`
              val samInterfaceViaConversion = callAtom.getArgumentsWithConversion.asScala.collectFirst {
                case (arg, samConversion) if argMatchesExpr(arg, expr) =>
                  samConversion.getOriginalParameterType.getConstructor.getDeclarationDescriptor
                    .asInstanceOf[ClassDescriptor]
              }

              val samInterface = samInterfaceViaConversion.orElse {
                bindingUtils.getExpectedExprType(expr).map { expectedExprType =>
                  expectedExprType.getConstructor.getDeclarationDescriptor.asInstanceOf[ClassDescriptor]
                }
              }

              samInterface
          }
        }
      case None =>
        // Lambda/anon function is directly assigned to a variable.
        // E.g. `val l = { i: Int -> i }`
        val lambdaExprType = bindingUtils.getExprType(expr)
        lambdaExprType.map(_.getConstructor.getDeclarationDescriptor.asInstanceOf[ClassDescriptor])
    }
    // if there type info fails, we may otherwise end up with Some(null), which wreaks havoc
    Option(res.orNull)
  }

  private def argMatchesExpr(arg: KotlinCallArgument, expr: KtElement): Boolean = {
    arg match {
      case arg: PSIFunctionKotlinCallArgument =>
        arg.getExpression == expr
      case _ =>
        false
    }
  }

  private def getSurroundingCallTarget(element: KtElement): Option[KtExpression] = {
    var context: PsiElement = element.getContext
    while (
      context != null &&
      !context.isInstanceOf[KtCallExpression] &&
      !context.isInstanceOf[KtBinaryExpression]
    ) {
      context = context.getContext
    }
    context match {
      case callExpr: KtCallExpression =>
        Some(callExpr.getCalleeExpression)
      case binaryExpr: KtBinaryExpression =>
        Some(binaryExpr.getOperationReference)
      case null =>
        None
    }
  }

  private def safeFunctionDescriptorName(f: FunctionDescriptor): Option[String] = {
    // if some type info fails, we may otherwise end up with some NPE at the getName call
    scala.util.Try(f.getName.toString).toOption
  }

  private def safeFunctionDescriptorSignature(f: FunctionDescriptor): Option[String] = {
    // if some type info fails, we may otherwise end up with some NPE in funcDescSignature
    scala.util.Try(nameRenderer.funcDescSignature(f)).toOption.flatten
  }

  private def createLambdaBindings(
    lambdaMethodNode: NewMethod,
    lambdaTypeDecl: NewTypeDecl,
    samInterface: Option[ClassDescriptor]
  ): Unit = {
    val samMethod          = samInterface.map(SamConversionResolverImplKt.getSingleAbstractMethodOrNull)
    val samMethodName      = samMethod.flatMap(safeFunctionDescriptorName).getOrElse(Constants.UnknownLambdaBindingName)
    val samMethodSignature = samMethod.flatMap(safeFunctionDescriptorSignature)

    val nativeLambdaBinding = bindingNode(samMethodName, lambdaMethodNode.signature, lambdaMethodNode.fullName)

    samMethodSignature match {
      case Some(signature) =>
        val interfaceLambdaBinding = bindingNode(samMethodName, signature, lambdaMethodNode.fullName)
        addToLambdaBindingInfoQueue(interfaceLambdaBinding, lambdaTypeDecl, lambdaMethodNode)

        if (signature != lambdaMethodNode.signature) {
          addToLambdaBindingInfoQueue(nativeLambdaBinding, lambdaTypeDecl, lambdaMethodNode)
        }
      case None =>
        addToLambdaBindingInfoQueue(nativeLambdaBinding, lambdaTypeDecl, lambdaMethodNode)
    }
  }

  def astForReturnExpression(expr: KtReturnExpression): Ast = {
    val returnedExpr =
      if (expr.getReturnedExpression != null) {
        astsForExpression(expr.getReturnedExpression, None)
      } else {
        Nil
      }
    returnAst(returnNode(expr, expr.getText), returnedExpr)
  }
}
