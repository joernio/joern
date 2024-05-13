package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.kotlin2cpg.types.TypeInfoProvider
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newClosureBindingNode
import io.joern.x2cpg.utils.NodeBuilders.newMethodReturnNode
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.semanticcpg.language.types.structure.NamespaceTraversal
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.Modality
import org.jetbrains.kotlin.psi.*

import java.util.UUID.nameUUIDFromBytes
import scala.jdk.CollectionConverters.*

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  private def isAbstract(ktFn: KtNamedFunction)(implicit typeInfoProvider: TypeInfoProvider): Boolean = {
    typeInfoProvider.modality(ktFn).contains(Modality.ABSTRACT)
  }

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

  def astsForMethod(ktFn: KtNamedFunction, needsThisParameter: Boolean = false, withVirtualModifier: Boolean = false)(
    implicit typeInfoProvider: TypeInfoProvider
  ): Seq[Ast] = {
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(ktFn, ("", ""))
    val _methodNode           = methodNode(ktFn, ktFn.getName, fullName, signature, relativizedPath)
    scope.pushNewScope(_methodNode)
    methodAstParentStack.push(_methodNode)

    val thisParameterMaybe = if (needsThisParameter) {
      val typeDeclFullName = registerType(typeInfoProvider.containingTypeDeclFullName(ktFn, TypeConstants.any))
      val node = NodeBuilders.newThisParameterNode(
        typeFullName = typeDeclFullName,
        dynamicTypeHintFullName = Seq(typeDeclFullName)
      )
      scope.addToScope(Constants.this_, node)
      Option(node)
    } else None

    val thisParameterAsts = thisParameterMaybe.map(List(_)).getOrElse(List()).map(Ast(_))
    val methodParametersAsts =
      withIndex(ktFn.getValueParameters.asScala.toSeq) { (p, idx) => astForParameter(p, idx) }
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

    val bodyAst           = bodyAsts.headOption.getOrElse(Ast(unknownNode(ktFn, Constants.empty)))
    val otherBodyAsts     = bodyAsts.drop(1)
    val explicitTypeName  = Option(ktFn.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val typeFullName      = registerType(typeInfoProvider.returnType(ktFn, explicitTypeName))
    val _methodReturnNode = newMethodReturnNode(typeFullName, None, line(ktFn), column(ktFn))

    val visibility = typeInfoProvider.visibility(ktFn)
    val visibilityModifierType =
      modifierTypeForVisibility(visibility.getOrElse(DescriptorVisibilities.UNKNOWN))
    val visibilityModifier = NodeBuilders.newModifierNode(visibilityModifierType)

    val modifierNodes =
      if (withVirtualModifier) Seq(NodeBuilders.newModifierNode(ModifierTypes.VIRTUAL))
      else Seq()

    val modifiers = if (isAbstract(ktFn)) {
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

    val explicitTypeName = Option(param.getTypeReference).map(_.getText).getOrElse(TypeConstants.any)
    val typeFullName     = registerType(typeInfoProvider.parameterType(param, explicitTypeName))
    val node             = parameterInNode(param, name, name, order, false, EvaluationStrategies.BY_VALUE, typeFullName)
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
    val name                  = nextClosureName()
    val (fullName, signature) = typeInfoProvider.fullNameWithSignatureAsLambda(fn, name)
    val lambdaMethodNode      = methodNode(fn, name, fullName, signature, relativizedPath)

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

    val lambdaTypeDecl = typeDeclNode(
      fn,
      Constants.lambdaTypeDeclName,
      lambdaTypeDeclFullName,
      relativizedPath,
      Seq(registerType(s"${TypeConstants.kotlinFunctionXPrefix}${fn.getValueParameters.size}")),
      None
    )

    val lambdaBinding = newBindingNode(Constants.lambdaBindingName, signature, lambdaMethodNode.fullName)
    val bindingInfo = BindingInfo(
      lambdaBinding,
      Seq((lambdaTypeDecl, lambdaBinding, EdgeTypes.BINDS), (lambdaBinding, lambdaMethodNode, EdgeTypes.REF))
    )
    scope.popScope()
    val closureBindingDefs = closureBindingEntriesForCaptured.collect { case (closureBinding, node) =>
      ClosureBindingDef(closureBinding, _methodRefNode, node.node)
    }
    closureBindingDefs.foreach(closureBindingDefQueue.prepend)
    lambdaBindingInfoQueue.prepend(bindingInfo)
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
    val name                  = nextClosureName()
    val (fullName, signature) = typeInfoProvider.fullNameWithSignature(expr, name)
    val lambdaMethodNode      = methodNode(expr, name, fullName, signature, relativizedPath)

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

    val returnTypeFullName     = registerType(typeInfoProvider.returnTypeFullName(expr))
    val lambdaTypeDeclFullName = fullName.split(":").head

    val bodyAst = bodyAsts.headOption.getOrElse(Ast(unknownNode(expr, Constants.empty)))
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

    val lambdaTypeDecl = typeDeclNode(
      expr,
      Constants.lambdaTypeDeclName,
      lambdaTypeDeclFullName,
      relativizedPath,
      Seq(registerType(s"${TypeConstants.kotlinFunctionXPrefix}${expr.getValueParameters.size}")),
      None
    )

    val lambdaBinding = newBindingNode(Constants.lambdaBindingName, signature, lambdaMethodNode.fullName)
    val bindingInfo = BindingInfo(
      lambdaBinding,
      Seq((lambdaTypeDecl, lambdaBinding, EdgeTypes.BINDS), (lambdaBinding, lambdaMethodNode, EdgeTypes.REF))
    )
    scope.popScope()
    val closureBindingDefs = closureBindingEntriesForCaptured.collect { case (closureBinding, node) =>
      ClosureBindingDef(closureBinding, _methodRefNode, node.node)
    }
    closureBindingDefs.foreach(closureBindingDefQueue.prepend)
    lambdaBindingInfoQueue.prepend(bindingInfo)
    lambdaAstQueue.prepend(lambdaMethodAst)
    Ast(_methodRefNode)
      .withChildren(annotations.map(astForAnnotationEntry))
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
