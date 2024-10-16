package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.{TypeConstants, TypeInfoProvider}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newClosureBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.com.intellij.psi.PsiElement
import org.jetbrains.kotlin.descriptors.{ClassDescriptor, DescriptorVisibilities, FunctionDescriptor, Modality}
import org.jetbrains.kotlin.psi.*
import org.jetbrains.kotlin.resolve.calls.model.ResolvedCallArgument
import org.jetbrains.kotlin.resolve.calls.tower.{NewAbstractResolvedCall, PSIFunctionKotlinCallArgument}
import org.jetbrains.kotlin.resolve.sam.{SamConstructorDescriptor, SamConversionResolverImplKt}
import org.jetbrains.kotlin.resolve.DescriptorUtils

import java.util.UUID.nameUUIDFromBytes
import scala.jdk.CollectionConverters.*

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

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
    val astParentName     = parentNode.properties(TypeDecl.Properties.Name.name).toString
    val astParentFullName = parentNode.properties(TypeDecl.Properties.FullName.name).toString
    val functionTypeDeclNode =
      typeDeclNode(
        node,
        methodName,
        methodFullName,
        filename,
        methodName,
        astParentType = astParentType,
        astParentFullName = astParentFullName,
        Seq(TypeConstants.kotlinFunctionXPrefix)
      )
    if (astParentName == NamespaceTraversal.globalNamespaceName || astParentType == Method.Label) {
      // Bindings for others (classes, interfaces, and such) are already created in their respective CPG creation functions
      val bindingNode = NewBinding().name(methodName).methodFullName(methodFullName).signature(signature)
      Ast(functionTypeDeclNode).withBindsEdge(functionTypeDeclNode, bindingNode).withRefEdge(bindingNode, methodNode)
    } else {
      Ast(functionTypeDeclNode)
    }
  }

  def astsForMethod(ktFn: KtNamedFunction, withVirtualModifier: Boolean = false)(implicit
    typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    val funcDesc = bindingUtils.getFunctionDesc(ktFn)
    val descFullName = funcDesc
      .flatMap(nameRenderer.descFullName)
      .getOrElse(s"${Defines.UnresolvedNamespace}.${ktFn.getName}")
    val signature = funcDesc
      .flatMap(nameRenderer.funcDescSignature)
      .getOrElse(s"${Defines.UnresolvedSignature}(${ktFn.getValueParameters.size()})")
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val _methodNode = methodNode(ktFn, ktFn.getName, fullName, signature, relativizedPath)
    scope.pushNewScope(_methodNode)
    methodAstParentStack.push(_methodNode)

    val needsThisParameter = funcDesc.get.getDispatchReceiverParameter != null ||
      DescriptorUtils.isExtension(funcDesc.get)

    val thisParameterAsts = if (needsThisParameter) {
      val typeDeclFullName = registerType(typeInfoProvider.containingTypeDeclFullName(ktFn, TypeConstants.any))
      val thisParam = NodeBuilders.newThisParameterNode(
        typeFullName = typeDeclFullName,
        dynamicTypeHintFullName = Seq(typeDeclFullName)
      )
      if (DescriptorUtils.isExtension(funcDesc.get)) {
        thisParam.order(1)
        thisParam.index(1)
      }
      scope.addToScope(Constants.this_, thisParam)
      List(Ast(thisParam))
    } else {
      List.empty
    }

    val valueParamStartIndex =
      if (DescriptorUtils.isExtension(funcDesc.get)) {
        2
      } else {
        1
      }

    val methodParametersAsts =
      withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, idx) =>
        astForParameter(p, valueParamStartIndex + idx - 1)
      }
    val bodyAsts = Option(ktFn.getBodyBlockExpression) match {
      case Some(bodyBlockExpression) => astsForBlock(bodyBlockExpression, None, None)
      case None =>
        Option(ktFn.getBodyExpression)
          .map { expr =>
            val bodyBlock = blockNode(expr, expr.getText, TypeConstants.any)
            val asts      = astsForExpression(expr, Some(1))
            val blockChildAsts =
              if (asts.nonEmpty) {
                val allStatementsButLast = asts.dropRight(1)
                val lastStatementAst     = asts.lastOption.getOrElse(Ast(unknownNode(expr, Constants.empty)))
                val returnAst_           = returnAst(returnNode(expr, Constants.retCode), Seq(lastStatementAst))
                (allStatementsButLast ++ Seq(returnAst_)).toList
              } else List()
            Seq(blockAst(bodyBlock, blockChildAsts))
          }
          .getOrElse {
            val bodyBlock = blockNode(ktFn, "<empty>", TypeConstants.any)
            Seq(blockAst(bodyBlock, List[Ast]()))
          }
    }
    methodAstParentStack.pop()
    scope.popScope()

    val bodyAst          = bodyAsts.headOption.getOrElse(Ast(unknownNode(ktFn, Constants.empty)))
    val otherBodyAsts    = bodyAsts.drop(1)
    val explicitTypeName = Option(ktFn.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val typeFullName = registerType(nameRenderer.typeFullName(funcDesc.get.getReturnType).getOrElse(explicitTypeName))
    val _methodReturnNode = newMethodReturnNode(typeFullName, None, line(ktFn), column(ktFn))

    val visibilityModifierType =
      modifierTypeForVisibility(funcDesc.get.getVisibility)
    val visibilityModifier = NodeBuilders.newModifierNode(visibilityModifierType)

    val modifierNodes =
      if (withVirtualModifier) Seq(NodeBuilders.newModifierNode(ModifierTypes.VIRTUAL))
      else Seq()

    val modifiers = if (funcDesc.get.getModality == Modality.ABSTRACT) {
      List(visibilityModifier) ++ modifierNodes :+ NodeBuilders.newModifierNode(ModifierTypes.ABSTRACT)
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
    Seq(
      methodAst(_methodNode, thisParameterAsts ++ methodParametersAsts, bodyAst, _methodReturnNode, modifiers)
        .withChildren(otherBodyAsts)
        .withChildren(annotationEntries)
    )
  }

  def astForParameter(param: KtParameter, order: Int)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val name = if (param.getDestructuringDeclaration != null) {
      Constants.paramNameLambdaDestructureDecl
    } else {
      param.getName
    }

    val explicitTypeName = Option(param.getTypeReference)
      .map(typeRef =>
        typeInfoProvider
          .typeFromImports(typeRef.getText, param.getContainingKtFile)
          .getOrElse(typeRef.getText)
      )
      .getOrElse(TypeConstants.any)
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
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val funcDesc = bindingUtils.getFunctionDesc(fn)
    val name     = nameRenderer.descName(funcDesc.get)
    val descFullName = funcDesc
      .flatMap(nameRenderer.descFullName)
      .getOrElse(s"${Defines.UnresolvedNamespace}.$name")
    val signature = funcDesc
      .flatMap(nameRenderer.funcDescSignature)
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
        val uuidBytes        = stringForUUID(fn, capturedNodeContext.name, capturedNodeContext.typeFullName)
        val closureBindingId = nameUUIDFromBytes(uuidBytes.getBytes).toString
        val closureBindingNode =
          newClosureBindingNode(closureBindingId, capturedNodeContext.name, EvaluationStrategies.BY_REFERENCE)
        (closureBindingNode, capturedNodeContext)
      }

    val localsForCaptured = closureBindingEntriesForCaptured.map { case (closureBindingNode, capturedNodeContext) =>
      val node =
        localNode(
          fn,
          capturedNodeContext.name,
          capturedNodeContext.name,
          capturedNodeContext.typeFullName,
          closureBindingNode.closureBindingId
        )
      scope.addToScope(capturedNodeContext.name, node)
      node
    }
    val parametersAsts =
      withIndex(fn.getValueParameters.asScala.toSeq) { (p, idx) =>
        astForParameter(p, idx)
      }
    val bodyAsts = Option(fn.getBodyBlockExpression) match {
      case Some(bodyBlockExpression) =>
        astsForBlock(bodyBlockExpression, None, None, localsForCaptures = localsForCaptured)
      case None =>
        Option(fn.getBodyExpression)
          .map { expr =>
            val bodyBlock  = blockNode(expr, expr.getText, TypeConstants.any)
            val returnAst_ = returnAst(returnNode(expr, Constants.retCode), astsForExpression(expr, Some(1)))
            Seq(blockAst(bodyBlock, localsForCaptured.map(Ast(_)) ++ List(returnAst_)))
          }
          .getOrElse {
            val bodyBlock = blockNode(fn, "<empty>", TypeConstants.any)
            Seq(blockAst(bodyBlock, List[Ast]()))
          }
    }

    val returnTypeFullName     = TypeConstants.javaLangObject
    val lambdaTypeDeclFullName = fullName.split(":").head

    val bodyAst = bodyAsts.headOption.getOrElse(Ast(unknownNode(fn, Constants.empty)))
    val lambdaMethodAst = methodAst(
      lambdaMethodNode,
      parametersAsts,
      bodyAst,
      newMethodReturnNode(returnTypeFullName, None, line(fn), column(fn)),
      NodeBuilders.newModifierNode(ModifierTypes.VIRTUAL) :: NodeBuilders.newModifierNode(ModifierTypes.LAMBDA) :: Nil
    )

    val _methodRefNode =
      withArgumentIndex(methodRefNode(fn, fn.getText, fullName, lambdaTypeDeclFullName), argIdxMaybe)
        .argumentName(argNameMaybe)

    val samInterface = getSamInterface(fn)

    val baseClassFullName = samInterface.flatMap(nameRenderer.descFullName).getOrElse(Constants.unknownLambdaBaseClass)

    val lambdaTypeDecl = typeDeclNode(
      fn,
      Constants.lambdaTypeDeclName,
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
  )(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val funcDesc = bindingUtils.getFunctionDesc(expr.getFunctionLiteral)
    val name     = nameRenderer.descName(funcDesc.get)
    val descFullName = funcDesc
      .flatMap(nameRenderer.descFullName)
      .getOrElse(s"${Defines.UnresolvedNamespace}.$name")
    val signature = funcDesc
      .flatMap(nameRenderer.funcDescSignature)
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
        val uuidBytes        = stringForUUID(expr, capturedNodeContext.name, capturedNodeContext.typeFullName)
        val closureBindingId = nameUUIDFromBytes(uuidBytes.getBytes).toString
        val closureBindingNode =
          newClosureBindingNode(closureBindingId, capturedNodeContext.name, EvaluationStrategies.BY_REFERENCE)
        (closureBindingNode, capturedNodeContext)
      }

    val localsForCaptured = closureBindingEntriesForCaptured.map { case (closureBindingNode, capturedNodeContext) =>
      val node =
        localNode(
          expr,
          capturedNodeContext.name,
          capturedNodeContext.name,
          capturedNodeContext.typeFullName,
          closureBindingNode.closureBindingId
        )
      scope.addToScope(capturedNodeContext.name, node)
      node
    }
    val parametersAsts = typeInfoProvider.implicitParameterName(expr) match {
      case Some(implicitParamName) =>
        val node = parameterInNode(
          expr,
          implicitParamName,
          implicitParamName,
          1,
          false,
          EvaluationStrategies.BY_REFERENCE,
          TypeConstants.any
        )
        scope.addToScope(implicitParamName, node)
        Seq(Ast(node))
      case None =>
        withIndex(expr.getValueParameters.asScala.toSeq) { (p, idx) =>
          val destructuringEntries =
            Option(p.getDestructuringDeclaration)
              .map(_.getEntries.asScala)
              .getOrElse(Seq())
          if (destructuringEntries.nonEmpty)
            destructuringEntries.filterNot(_.getText == Constants.unusedDestructuringEntryText).zipWithIndex.map {
              case (entry, innerIdx) =>
                val name             = entry.getName
                val explicitTypeName = Option(entry.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
                val typeFullName     = registerType(typeInfoProvider.destructuringEntryType(entry, explicitTypeName))
                val node =
                  parameterInNode(entry, name, name, innerIdx + idx, false, EvaluationStrategies.BY_VALUE, typeFullName)
                scope.addToScope(name, node)
                Ast(node)
            }
          else Seq(astForParameter(p, idx))
        }.flatten
    }

    val lastChildNotReturnExpression = !expr.getBodyExpression.getLastChild.isInstanceOf[KtReturnExpression]
    val needsReturnExpression =
      lastChildNotReturnExpression && !typeInfoProvider.hasApplyOrAlsoScopeFunctionParent(expr)
    val bodyAsts = Option(expr.getBodyExpression)
      .map(
        astsForBlock(
          _,
          None,
          None,
          pushToScope = false,
          localsForCaptured,
          implicitReturnAroundLastStatement = needsReturnExpression
        )
      )
      .getOrElse(Seq(Ast(NewBlock())))

    val returnTypeFullName = registerType(
      nameRenderer.typeFullName(funcDesc.get.getReturnType).getOrElse(TypeConstants.javaLangObject)
    )
    val lambdaTypeDeclFullName = fullName.split(":").head

    val (bodyAst, nestedLambdaDecls) = bodyAsts.toList match {
      case body :: nestedLambdaDecls =>
        if (nestedLambdaDecls.exists(_.root.exists(x => !x.isInstanceOf[NewMethod])))
          logger.warn("Detected non-method related AST nodes under lambda expression. This is unexpected.")
        body -> nestedLambdaDecls
      case Nil => Ast(unknownNode(expr, Constants.empty)) -> Nil
    }
    val lambdaMethodAst = methodAst(
      lambdaMethodNode,
      parametersAsts,
      bodyAst,
      newMethodReturnNode(returnTypeFullName, None, line(expr), column(expr)),
      newModifierNode(ModifierTypes.VIRTUAL) :: newModifierNode(ModifierTypes.LAMBDA) :: Nil
    )

    val _methodRefNode =
      withArgumentIndex(methodRefNode(expr, expr.getText, fullName, lambdaTypeDeclFullName), argIdxMaybe)
        .argumentName(argNameMaybe)

    val samInterface = getSamInterface(expr)

    val baseClassFullName = samInterface.flatMap(nameRenderer.descFullName).getOrElse(Constants.unknownLambdaBaseClass)

    val lambdaTypeDecl = typeDeclNode(
      expr,
      Constants.lambdaTypeDeclName,
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
    nestedLambdaDecls.foreach(lambdaAstQueue.prepend)
    Ast(_methodRefNode)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  // SAM stands for: single abstraction method
  private def getSamInterface(expr: KtLambdaExpression | KtNamedFunction): Option[ClassDescriptor] = {
    getSurroundingCallTarget(expr) match {
      case Some(callTarget) =>
        val resolvedCallAtom = bindingUtils
          .getResolvedCallDesc(callTarget)
          .collect { case call: NewAbstractResolvedCall[?] =>
            call.getResolvedCallAtom
          }

        resolvedCallAtom.map { callAtom =>
          callAtom.getCandidateDescriptor match {
            case samConstructorDesc: SamConstructorDescriptor =>
              // Lambda is wrapped e.g. `SomeInterface { obj -> obj }`
              samConstructorDesc.getBaseDescriptorForSynthetic
            case _ =>
              // Lambda/anan function is directly used as call argument e.g. `someCall(obj -> obj)`
              callAtom.getArgumentMappingByOriginal.asScala.collectFirst {
                case (paramDesc, resolvedArgument) if isExprIncluded(resolvedArgument, expr) =>
                  paramDesc.getType.getConstructor.getDeclarationDescriptor.asInstanceOf[ClassDescriptor]
              }.get
          }
        }
      case None =>
        // Lambda/anon function is directly assigned to a variable.
        // E.g. `val l = { i: Int -> i }`
        val lambdaExprType = bindingUtils.getExprType(expr)
        lambdaExprType.map(_.getConstructor.getDeclarationDescriptor.asInstanceOf[ClassDescriptor])
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

  private def isExprIncluded(resolvedArgument: ResolvedCallArgument, expr: KtExpression): Boolean = {
    resolvedArgument.getArguments.asScala.exists {
      case psi: PSIFunctionKotlinCallArgument =>
        psi.getExpression == expr
      case _ =>
        false
    }
  }

  private def createLambdaBindings(
    lambdaMethodNode: NewMethod,
    lambdaTypeDecl: NewTypeDecl,
    samInterface: Option[ClassDescriptor]
  ): Unit = {
    val samMethod          = samInterface.map(SamConversionResolverImplKt.getSingleAbstractMethodOrNull)
    val samMethodName      = samMethod.map(_.getName.toString).getOrElse(Constants.unknownLambdaBindingName)
    val samMethodSignature = samMethod.flatMap(nameRenderer.funcDescSignature)

    if (samMethodSignature.isDefined) {
      val interfaceLambdaBinding = newBindingNode(samMethodName, samMethodSignature.get, lambdaMethodNode.fullName)
      addToLambdaBindingInfoQueue(interfaceLambdaBinding, lambdaTypeDecl, lambdaMethodNode)
    }

    val nativeLambdaBinding = newBindingNode(samMethodName, lambdaMethodNode.signature, lambdaMethodNode.fullName)
    addToLambdaBindingInfoQueue(nativeLambdaBinding, lambdaTypeDecl, lambdaMethodNode)
  }

  def astForReturnExpression(expr: KtReturnExpression)(implicit typeInfoProvider: TypeInfoProvider): Ast = {
    val returnedExpr =
      if (expr.getReturnedExpression != null) {
        astsForExpression(expr.getReturnedExpression, None)
      } else {
        Nil
      }
    returnAst(returnNode(expr, expr.getText), returnedExpr)
  }
}
