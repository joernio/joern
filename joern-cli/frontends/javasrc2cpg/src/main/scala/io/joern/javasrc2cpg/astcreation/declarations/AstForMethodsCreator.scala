package io.joern.javasrc2cpg.astcreation.declarations

import io.joern.x2cpg.Ast
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.x2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.javasrc2cpg.util.Util.*
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.ValidationMode
import com.github.javaparser.ast.body.ConstructorDeclaration
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.body.CallableDeclaration

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import com.github.javaparser.resolution.declarations.ResolvedMethodLikeDeclaration
import scala.util.Try
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.Parameter
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.resolution.declarations.ResolvedMethodDeclaration
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodReturn
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.astcreation.ExpectedType

private[declarations] trait AstForMethodsCreator { this: AstCreator =>
  def astForMethod(methodDeclaration: MethodDeclaration): Ast = {
    val methodNode = createPartialMethod(methodDeclaration)

    val typeParameters = getIdentifiersForTypeParameters(methodDeclaration)

    val maybeResolved      = tryWithSafeStackOverflow(methodDeclaration.resolve())
    val expectedReturnType = Try(symbolSolver.toResolvedType(methodDeclaration.getType, classOf[ResolvedType])).toOption
    val simpleMethodReturnType = methodDeclaration.getTypeAsString()
    val returnTypeFullName = expectedReturnType
      .flatMap(typeInfoCalc.fullName)
      .orElse(scope.lookupType(simpleMethodReturnType))
      .orElse(typeParameters.find(_.name == simpleMethodReturnType).map(_.typeFullName))

    scope.pushMethodScope(methodNode, ExpectedType(returnTypeFullName, expectedReturnType))
    typeParameters.foreach { typeParameter => scope.addType(typeParameter.name, typeParameter.typeFullName) }

    val parameterAsts  = astsForParameterList(methodDeclaration.getParameters)
    val parameterTypes = argumentTypesForMethodLike(maybeResolved)
    val signature      = composeSignature(returnTypeFullName, parameterTypes, parameterAsts.size)
    val namespaceName  = scope.enclosingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace)
    val methodFullName = composeMethodFullName(namespaceName, methodDeclaration.getNameAsString, signature)

    methodNode
      .fullName(methodFullName)
      .signature(signature)

    val thisNode = Option.when(!methodDeclaration.isStatic) {
      val typeFullName = scope.enclosingTypeDeclFullName
      thisNodeForMethod(typeFullName, line(methodDeclaration))
    }
    val thisAst = thisNode.map(Ast(_)).toList

    thisNode.foreach { node =>
      scope.addParameter(node)
    }

    val bodyAst = methodDeclaration.getBody.toScala.map(astForBlockStatement(_)).getOrElse(Ast(NewBlock()))
    val methodReturn = newMethodReturnNode(
      returnTypeFullName.getOrElse(TypeConstants.Any),
      None,
      line(methodDeclaration.getType),
      column(methodDeclaration.getType)
    )

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    val modifiers = modifiersForMethod(methodDeclaration)

    scope.popScope()

    methodAstWithAnnotations(methodNode, thisAst ++ parameterAsts, bodyAst, methodReturn, modifiers, annotationAsts)
  }

  private def abstractModifierForCallable(
    callableDeclaration: CallableDeclaration[_],
    isInterfaceMethod: Boolean
  ): Option[NewModifier] = {
    callableDeclaration match {
      case methodDeclaration: MethodDeclaration =>
        Option.when(methodDeclaration.isAbstract || (isInterfaceMethod && !methodDeclaration.isDefault)) {
          newModifierNode(ModifierTypes.ABSTRACT)
        }

      case _ => None
    }
  }

  private def modifiersForMethod(methodDeclaration: CallableDeclaration[_]): List[NewModifier] = {
    val isInterfaceMethod = scope.enclosingTypeDecl.exists(_.code.contains("interface "))

    val abstractModifier = abstractModifierForCallable(methodDeclaration, isInterfaceMethod)

    val staticVirtualModifierType = if (methodDeclaration.isStatic) ModifierTypes.STATIC else ModifierTypes.VIRTUAL
    val staticVirtualModifier     = Some(newModifierNode(staticVirtualModifierType))

    val accessModifierType = if (methodDeclaration.isPublic) {
      Some(ModifierTypes.PUBLIC)
    } else if (methodDeclaration.isPrivate) {
      Some(ModifierTypes.PRIVATE)
    } else if (methodDeclaration.isProtected) {
      Some(ModifierTypes.PROTECTED)
    } else if (isInterfaceMethod) {
      // TODO: more robust interface check
      Some(ModifierTypes.PUBLIC)
    } else {
      None
    }
    val accessModifier = accessModifierType.map(newModifierNode)

    List(accessModifier, abstractModifier, staticVirtualModifier).flatten
  }

  private def getIdentifiersForTypeParameters(methodDeclaration: MethodDeclaration): List[NewIdentifier] = {
    methodDeclaration.getTypeParameters.asScala.map { typeParameter =>
      val name = typeParameter.getNameAsString
      val typeFullName = typeParameter.getTypeBound.asScala.headOption
        .flatMap(typeInfoCalc.fullName)
        .getOrElse(TypeConstants.Object)
      typeInfoCalc.registerType(typeFullName)

      NewIdentifier().name(name).typeFullName(typeFullName)
    }.toList
  }

  def clinitAstFromStaticInits(staticInits: Seq[Ast]): Option[Ast] = {
    Option.when(staticInits.nonEmpty) {
      val signature         = composeMethodLikeSignature(TypeConstants.Void, Nil)
      val enclosingDeclName = scope.enclosingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace)
      val fullName          = composeMethodFullName(enclosingDeclName, Defines.StaticInitMethodName, signature)
      staticInitMethodAst(staticInits.toList, fullName, Some(signature), TypeConstants.Void)
    }
  }

  def astForDefaultConstructor(): Ast = {
    val typeFullName = scope.enclosingTypeDeclFullName
    val signature    = s"${TypeConstants.Void}()"
    val fullName = composeMethodFullName(
      typeFullName.getOrElse(Defines.UnresolvedNamespace),
      Defines.ConstructorMethodName,
      signature
    )
    val constructorNode = NewMethod()
      .name(io.joern.x2cpg.Defines.ConstructorMethodName)
      .fullName(fullName)
      .signature(signature)
      .filename(filename)
      .isExternal(false)

    val thisAst = Ast(thisNodeForMethod(typeFullName, lineNumber = None))
    val bodyAst = Ast(NewBlock()).withChildren(scope.memberInitializers)

    val returnNode = newMethodReturnNode(TypeConstants.Void, line = None, column = None)

    val modifiers = List(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.PUBLIC))

    methodAstWithAnnotations(constructorNode, Seq(thisAst), bodyAst, returnNode, modifiers)
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val maybeArraySuffix = if (parameter.isVarArgs) "[]" else ""
    val typeFullName =
      typeInfoCalc
        .fullName(parameter.getType)
        .orElse(scope.lookupType(parameter.getTypeAsString))
        .map(_ ++ maybeArraySuffix)
        .getOrElse(s"${Defines.UnresolvedNamespace}.${parameter.getTypeAsString}")
    val evalStrat =
      if (parameter.getType.isPrimitiveType) EvaluationStrategies.BY_VALUE else EvaluationStrategies.BY_SHARING
    typeInfoCalc.registerType(typeFullName)

    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(parameter.toString)
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
      .evaluationStrategy(evalStrat)
      .typeFullName(typeFullName)
      .index(childNum)
      .order(childNum)
    val annotationAsts = parameter.getAnnotations.asScala.map(astForAnnotationExpr)
    val ast            = Ast(parameterNode)

    scope.addParameter(parameterNode)

    ast.withChildren(annotationAsts)
  }

  def calcParameterTypes(
    methodLike: ResolvedMethodLikeDeclaration,
    typeParamValues: ResolvedTypeParametersMap
  ): Option[List[String]] = {
    val parameterTypes =
      Range(0, methodLike.getNumberOfParams)
        .flatMap { index =>
          Try(methodLike.getParam(index)).toOption
        }
        .map { param =>
          Try(param.getType).toOption
            .flatMap(paramType => typeInfoCalc.fullName(paramType, typeParamValues))
        }

    toOptionList(parameterTypes)
  }

  def composeSignature(
    maybeReturnType: Option[String],
    maybeParameterTypes: Option[List[String]],
    parameterCount: Int
  ): String = {
    (maybeReturnType, maybeParameterTypes) match {
      case (Some(returnType), Some(parameterTypes)) =>
        composeMethodLikeSignature(returnType, parameterTypes)

      case _ =>
        composeUnresolvedSignature(parameterCount)
    }
  }

  def methodSignature(method: ResolvedMethodDeclaration, typeParamValues: ResolvedTypeParametersMap): String = {
    val maybeParameterTypes = calcParameterTypes(method, typeParamValues)

    val maybeReturnType =
      Try(method.getReturnType).toOption
        .flatMap(returnType => typeInfoCalc.fullName(returnType, typeParamValues))

    composeSignature(maybeReturnType, maybeParameterTypes, method.getNumberOfParams)
  }
  private def astsForParameterList(parameters: NodeList[Parameter]): Seq[Ast] = {
    parameters.asScala.toList.zipWithIndex.map { case (param, idx) =>
      astForParameter(param, idx + 1)
    }
  }

  def astForConstructor(constructorDeclaration: ConstructorDeclaration): Ast = {
    val constructorNode = createPartialMethod(constructorDeclaration)
      .name(io.joern.x2cpg.Defines.ConstructorMethodName)

    scope.pushMethodScope(constructorNode, ExpectedType.Void)
    val maybeResolved = tryWithSafeStackOverflow(constructorDeclaration.resolve())

    val parameterAsts = astsForParameterList(constructorDeclaration.getParameters).toList
    val paramTypes    = argumentTypesForMethodLike(maybeResolved)
    val signature     = composeSignature(Some(TypeConstants.Void), paramTypes, parameterAsts.size)
    val typeFullName  = scope.enclosingTypeDeclFullName
    val fullName =
      composeMethodFullName(
        typeFullName.getOrElse(Defines.UnresolvedNamespace),
        Defines.ConstructorMethodName,
        signature
      )

    constructorNode
      .fullName(fullName)
      .signature(signature)

    parameterAsts.foreach { ast =>
      ast.root match {
        case Some(parameter: NewMethodParameterIn) => scope.addParameter(parameter)
        case _                                     => // This should never happen
      }
    }

    val thisNode = thisNodeForMethod(typeFullName, line(constructorDeclaration))
    scope.addParameter(thisNode)
    val thisAst = Ast(thisNode)

    val bodyAst      = astForConstructorBody(Some(constructorDeclaration.getBody))
    val methodReturn = constructorReturnNode(constructorDeclaration)

    val annotationAsts = constructorDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toList

    val modifiers =
      NewModifier().modifierType(ModifierTypes.CONSTRUCTOR) :: modifiersForMethod(constructorDeclaration).filterNot(
        _.modifierType == ModifierTypes.VIRTUAL
      )

    scope.popScope()

    methodAstWithAnnotations(
      constructorNode,
      thisAst :: parameterAsts,
      bodyAst,
      methodReturn,
      modifiers,
      annotationAsts
    )
  }

  private def constructorReturnNode(constructorDeclaration: ConstructorDeclaration): NewMethodReturn = {
    val line   = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val column = constructorDeclaration.getEnd.map(x => Integer.valueOf(x.column)).toScala
    newMethodReturnNode(TypeConstants.Void, None, line, column)
  }

  private def astForConstructorBody(body: Option[BlockStmt]): Ast = {
    val containsThisInvocation =
      body
        .flatMap(_.getStatements.asScala.headOption)
        .collect { case e: ExplicitConstructorInvocationStmt => e }
        .exists(_.isThis)

    val memberInitializers =
      if (containsThisInvocation)
        Seq.empty
      else
        scope.memberInitializers

    body match {
      case Some(b) => astForBlockStatement(b, prefixAsts = memberInitializers)

      case None => Ast(NewBlock()).withChildren(memberInitializers)
    }
  }

  /** Constructor and Method declarations share a lot of fields, so this method adds the fields they have in common.
    * `fullName` and `signature` are omitted
    */
  private def createPartialMethod(declaration: CallableDeclaration[_]): NewMethod = {
    val code         = declaration.getDeclarationAsString.trim
    val columnNumber = declaration.getBegin.map(x => Integer.valueOf(x.column)).toScala
    val endLine      = declaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val endColumn    = declaration.getEnd.map(x => Integer.valueOf(x.column)).toScala

    val methodNode = NewMethod()
      .name(declaration.getNameAsString)
      .code(code)
      .isExternal(false)
      .filename(filename)
      .lineNumber(line(declaration))
      .columnNumber(columnNumber)
      .lineNumberEnd(endLine)
      .columnNumberEnd(endColumn)

    methodNode
  }

  def thisNodeForMethod(maybeTypeFullName: Option[String], lineNumber: Option[Integer]): NewMethodParameterIn = {
    val typeFullName = typeInfoCalc.registerType(maybeTypeFullName.getOrElse(TypeConstants.Any))
    NodeBuilders.newThisParameterNode(
      typeFullName = typeFullName,
      dynamicTypeHintFullName = maybeTypeFullName.toSeq,
      line = lineNumber
    )
  }
}
