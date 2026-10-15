package io.joern.javasrc2cpg.astcreation.declarations

import com.github.javaparser.ast.Node
import com.github.javaparser.ast.`type`.{ClassOrInterfaceType, Type}
import com.github.javaparser.ast.body.*
import com.github.javaparser.ast.stmt.ExplicitConstructorInvocationStmt
import com.github.javaparser.resolution.declarations.{
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedParameterDeclaration
}
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserParameterDeclaration
import io.joern.javasrc2cpg.astcreation.declarations.AstForMethodsCreator.PartialConstructorDeclaration
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.JavaScopeElement.fullName
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util.*
import io.joern.javasrc2cpg.util.{NameConstants, Util}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.*

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

private[declarations] trait AstForMethodsCreator { this: AstCreator =>

  def astForAnnotationMemberDeclaration(member: AnnotationMemberDeclaration): Ast = {
    val methodNode = createPartialMethod(member)

    val (expectedType, returnTypeFullName, thisAst) =
      pushMethodScopeAndThis(member, methodNode, member.getType, member.getTypeAsString, Nil, isStatic = false)

    // Must run before the body is built: a default value may reference nested/local types whose binary names
    // are namespaced under this method's fullName.
    setMethodFullNameAndSignature(methodNode, member.getNameAsString, returnTypeFullName, Option(Nil), 0)

    val bodyAst = member.getDefaultValue.toScala
      .map { defaultValue =>
        val returnStmt = returnNode(defaultValue, s"return ${code(defaultValue)}")
        val valueAsts  = astsForExpression(defaultValue, expectedType)
        blockAst(blockNode(member), returnAst(returnStmt, valueAsts) :: Nil)
      }
      .getOrElse(Ast(NewBlock()))

    // Annotation elements are always implicitly public, non-static, and have no concrete dispatch target.
    val modifiers = List(
      modifierNode(member, ModifierTypes.PUBLIC),
      modifierNode(member, ModifierTypes.ABSTRACT),
      modifierNode(member, ModifierTypes.VIRTUAL)
    )
    val annotationAsts = member.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    finishMethodAst(methodNode, member.getType, returnTypeFullName, thisAst, Nil, bodyAst, modifiers, annotationAsts)
  }

  def astForMethod(methodDeclaration: MethodDeclaration): Ast = {
    val methodNode = createPartialMethod(methodDeclaration)

    val typeParameters = getIdentifiersForTypeParameters(methodDeclaration)
    val maybeResolved  = tryWithSafeStackOverflow(methodDeclaration.resolve()).toOption

    val (_, returnTypeFullName, thisAst) = pushMethodScopeAndThis(
      methodDeclaration,
      methodNode,
      methodDeclaration.getType,
      methodDeclaration.getTypeAsString,
      typeParameters,
      methodDeclaration.isStatic()
    )

    // Must run after type parameters are registered in scope (done above by pushMethodScopeAndThis), since
    // the signature may reference them as type variables rather than resolved classes.
    methodNode.genericSignature(binarySignatureCalculator.methodBinarySignature(methodDeclaration))

    val parameterAsts  = astsForParameterList(methodDeclaration.getParameters.asScala.toList)
    val parameterTypes = argumentTypesForMethodLike(maybeResolved)

    // Must run before the body is built: nested/local types declared in the body are namespaced under this
    // method's fullName.
    setMethodFullNameAndSignature(
      methodNode,
      methodDeclaration.getNameAsString,
      returnTypeFullName,
      parameterTypes,
      parameterAsts.size
    )

    val bodyAst = methodDeclaration.getBody.toScala
      .map(astForBlockStatement(_, includeTemporaryLocals = true))
      .getOrElse(Ast(NewBlock()))

    val modifiers      = modifiersForMethod(methodDeclaration)
    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    finishMethodAst(
      methodNode,
      methodDeclaration.getType,
      returnTypeFullName,
      thisAst,
      parameterAsts,
      bodyAst,
      modifiers,
      annotationAsts
    )
  }

  private def setMethodFullNameAndSignature(
    methodNode: NewMethod,
    name: String,
    returnTypeFullName: Option[String],
    parameterTypes: Option[List[String]],
    parameterCount: Int
  ): Unit = {
    val signature      = composeSignature(returnTypeFullName, parameterTypes, parameterCount)
    val namespaceName  = scope.enclosingTypeDecl.fullName.getOrElse(Defines.UnresolvedNamespace)
    val methodFullName = composeMethodFullName(namespaceName, name, signature)

    methodNode
      .fullName(methodFullName)
      .signature(signature)
  }

  /** Resolves the return type, pushes the method scope, registers type parameters, and wires up the implicit `this`
    * parameter (registering it in scope so bodies built afterwards may reference it implicitly). Must be followed by a
    * call to [[finishMethodAst]] once parameters and the body have been built.
    *
    * @return
    *   the resolved expected return type, the resolved return type full name, and the `this` parameter as an AST (empty
    *   for static methods)
    */
  private def pushMethodScopeAndThis(
    originNode: Node,
    methodNode: NewMethod,
    returnType: => Type,
    returnTypeAsString: => String,
    typeParameters: List[NewIdentifier],
    isStatic: Boolean
  ): (ExpectedType, Option[String], List[Ast]) = {
    val expectedReturnType = tryWithSafeStackOverflow(
      symbolSolver.toResolvedType(returnType, classOf[ResolvedType])
    ).toOption
    val simpleReturnType =
      tryWithSafeStackOverflow(returnTypeAsString).map(Util.stripGenericTypes).toOption
    val returnTypeFullName = expectedReturnType
      .flatMap(typeInfoCalc.fullName)
      .orElse(simpleReturnType.flatMap(scope.lookupType(_)))
      .orElse(tryWithSafeStackOverflow(returnType).toOption.collect { case t: ClassOrInterfaceType =>
        scope.lookupType(t.getNameAsString)
      }.flatten)
      .orElse(typeParameters.find(typeParam => simpleReturnType.contains(typeParam.name)).map(_.typeFullName))

    val expectedType = ExpectedType(returnTypeFullName, expectedReturnType)
    scope.pushMethodScope(methodNode, expectedType, isStatic)
    typeParameters.foreach { typeParameter => scope.addTypeParameter(typeParameter.name, typeParameter.typeFullName) }

    val thisNode = Option.when(!isStatic)(thisNodeForMethod(originNode, scope.enclosingTypeDecl.fullName))
    val thisAst  = thisNode.map(Ast(_)).toList

    thisNode.foreach { node =>
      scope.enclosingMethod.get.addParameter(node, scope.enclosingTypeDecl.get.typeDecl.genericSignature)
    }

    (expectedType, returnTypeFullName, thisAst)
  }

  /** Pops the method scope pushed by [[pushMethodScopeAndThis]] and assembles the completed method AST from its
    * already-built parts. The method's fullName/signature must already be set (see [[setMethodFullNameAndSignature]])
    * before this is called.
    */
  private def finishMethodAst(
    methodNode: NewMethod,
    returnType: => Type,
    returnTypeFullName: Option[String],
    thisAst: List[Ast],
    parameterAsts: Seq[Ast],
    bodyAst: Ast,
    modifiers: List[NewModifier],
    annotationAsts: Seq[Ast]
  ): Ast = {
    val methodReturn =
      methodReturnNode(returnType, returnTypeFullName.getOrElse(defaultTypeFallback(returnType)))

    scope.popMethodScope()

    methodAstWithAnnotations(methodNode, thisAst ++ parameterAsts, bodyAst, methodReturn, modifiers, annotationAsts)
  }

  private[declarations] def astForRecordParameterAccessor(
    parameter: Parameter,
    recordTypeFullName: String,
    parameterName: String,
    parameterTypeFullName: String
  ): Ast = {
    val signature =
      if (isResolvedTypeFullName(parameterTypeFullName))
        composeSignature(Option(parameterTypeFullName), Option(Nil), 0)
      else
        composeSignature(None, Option(Nil), 0)

    val methodFullName = composeMethodFullName(recordTypeFullName, parameterName, signature)

    val methodReturn =
      methodReturnNode(parameter, parameterTypeFullName)

    val genericSignature = binarySignatureCalculator.recordParameterAccessorBinarySignature(parameter)
    val methodRoot       = methodNode(
      parameter,
      parameterName,
      s"public ${code(parameter.getType)} ${parameterName}()",
      methodFullName,
      Option(signature),
      filename,
      Option(NodeTypes.TYPE_DECL),
      Option(recordTypeFullName),
      genericSignature = Option(genericSignature)
    )

    val modifier = modifierNode(parameter, ModifierTypes.PUBLIC)

    val thisParameter = thisNodeForMethod(parameter, Option(recordTypeFullName))

    val thisIdentifier    = identifierNode(parameter, thisParameter.name, thisParameter.code, recordTypeFullName)
    val thisIdentifierAst = Ast(thisIdentifier).withRefEdge(thisIdentifier, thisParameter)
    val fieldIdentifier   = fieldIdentifierNode(parameter, parameterName, parameterName)

    val fieldAccessNode = operatorCallNode(
      parameter,
      s"${thisIdentifier.code}.${fieldIdentifier.code}",
      Operators.fieldAccess,
      Option(parameterTypeFullName)
    )
    val fieldAccessCall = callAst(fieldAccessNode, thisIdentifierAst :: Ast(fieldIdentifier) :: Nil)

    val returnStmt = returnNode(parameter, s"return ${fieldAccessNode.code}")
    val returnAst  = Ast(returnStmt).withChild(fieldAccessCall)

    val methodBodyAst = blockAst(blockNode(parameter), returnAst :: Nil)

    methodAst(methodRoot, Ast(thisParameter) :: Nil, methodBodyAst, methodReturn, modifier :: Nil)
  }

  private def abstractModifierForCallable(
    callableDeclaration: CallableDeclaration[?],
    isInterfaceMethod: Boolean
  ): Option[NewModifier] = {
    callableDeclaration match {
      case methodDeclaration: MethodDeclaration =>
        Option.when(methodDeclaration.isAbstract || (isInterfaceMethod && !methodDeclaration.isDefault)) {
          modifierNode(callableDeclaration, ModifierTypes.ABSTRACT)
        }

      case _ => None
    }
  }

  private def modifiersForMethod(
    methodDeclaration: CallableDeclaration[?] | CompactConstructorDeclaration
  ): List[NewModifier] = {
    val isInterfaceMethod = scope.enclosingTypeDecl.isInterface

    val abstractModifier = Option
      .when(methodDeclaration.isCallableDeclaration)(
        abstractModifierForCallable(methodDeclaration.asCallableDeclaration(), isInterfaceMethod)
      )
      .flatten

    // TODO: The opposite of static is not virtual
    val staticVirtualModifierType =
      if (methodDeclaration.isCallableDeclaration && methodDeclaration.asCallableDeclaration().isStatic)
        ModifierTypes.STATIC
      else ModifierTypes.VIRTUAL
    val staticVirtualModifier = Some(modifierNode(methodDeclaration, staticVirtualModifierType))

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
    val accessModifier = accessModifierType.map(modifierNode(methodDeclaration, _))

    List(accessModifier, abstractModifier, staticVirtualModifier).flatten
  }

  private def getIdentifiersForTypeParameters(methodDeclaration: CallableDeclaration[?]): List[NewIdentifier] = {
    methodDeclaration.getTypeParameters.asScala.map { typeParameter =>
      val name         = typeParameter.getNameAsString
      val typeFullName = tryWithSafeStackOverflow(typeParameter.getTypeBound.asScala.headOption).toOption.flatten
        .flatMap(typeInfoCalc.fullName)
        .getOrElse(TypeConstants.Object)
      typeInfoCalc.registerType(typeFullName)

      NewIdentifier().name(name).typeFullName(typeFullName)
    }.toList
  }

  def clinitAstFromStaticInits(node: Node, staticInits: Seq[Ast]): Option[Ast] = {
    Option.when(staticInits.nonEmpty) {
      val signature         = composeMethodLikeSignature(TypeConstants.Void, Nil)
      val enclosingDeclName = scope.enclosingTypeDecl.fullName.getOrElse(Defines.UnresolvedNamespace)
      val fullName          = composeMethodFullName(enclosingDeclName, Defines.StaticInitMethodName, signature)
      staticInitMethodAst(node, staticInits.toList, fullName, Some(signature), TypeConstants.Void)
    }
  }

  private def astsForFieldInitializers(fieldDeclarations: List[FieldDeclaration]): List[Ast] = {
    scope.pushBlockScope()
    val fieldInitializerAsts = fieldDeclarations.flatMap { fieldDeclaration =>
      fieldDeclaration.getVariables.asScala.filter(_.getInitializer.isPresent).toList.flatMap { variableDeclaration =>
        scope.pushFieldDeclScope(fieldDeclaration.isStatic, variableDeclaration.getNameAsString)
        val assignmentAsts = astsForVariableDeclarator(variableDeclaration, fieldDeclaration)
        val patternAsts    = scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_))
        scope.popFieldDeclScope()
        patternAsts ++ assignmentAsts
      }
    }
    scope.popBlockScope()
    fieldInitializerAsts
  }

  def astForDefaultConstructor(originNode: Node, instanceFieldDeclarations: List[FieldDeclaration]): Ast = {
    val parameters       = scope.enclosingTypeDecl.get.recordParameters
    val genericSignature = binarySignatureCalculator.defaultConstructorSignature(parameters)
    val constructorNode  = NewMethod()
      .name(io.joern.x2cpg.Defines.ConstructorMethodName)
      .filename(filename)
      .isExternal(false)
      .genericSignature(genericSignature)
      .lineNumber(line(originNode))
      .columnNumber(column(originNode))
    scope.pushMethodScope(constructorNode, ExpectedType.Void, isStatic = false)

    val parameterAsts = parameters.zipWithIndex.map { case (param, idx) =>
      astForParameter(param, idx + 1)
    }
    val parameterTypes         = parameterAsts.map(_.rootType.getOrElse(defaultTypeFallback()))
    val resolvedParameterTypes = Option.when(parameterTypes.forall(isResolvedTypeFullName))(parameterTypes)

    val typeFullName = scope.enclosingTypeDecl.fullName
    val signature    = composeSignature(Option(TypeConstants.Void), resolvedParameterTypes, parameterAsts.size)
    val fullName     = composeMethodFullName(
      typeFullName.getOrElse(Defines.UnresolvedNamespace),
      Defines.ConstructorMethodName,
      signature
    )

    constructorNode.fullName(fullName)
    constructorNode.signature(signature)

    val thisNode = thisNodeForMethod(originNode, typeFullName)
    scope.enclosingMethod.foreach(_.addParameter(thisNode, scope.enclosingTypeDecl.get.typeDecl.genericSignature))
    val recordParameterAssignments = parameterAsts
      .flatMap(_.nodes)
      .collect { case param: nodes.NewMethodParameterIn => param }
      .zipWithIndex
      .map((paramNode, idx) => astForEponymousFieldAssignment(parameters(idx), thisNode, paramNode))
    val bodyStatementAsts =
      astsForFieldInitializers(instanceFieldDeclarations) ++ recordParameterAssignments
    val patternLocalAsts = scope.enclosingMethod.map(_.getAndClearUnaddedPatternLocals()).getOrElse(Nil).map(Ast(_))

    val returnNode = methodReturnNode(originNode, TypeConstants.Void)

    val modifiers =
      List(modifierNode(originNode, ModifierTypes.CONSTRUCTOR), modifierNode(originNode, ModifierTypes.PUBLIC))
    val partialConstructor =
      PartialConstructorDeclaration(
        originNode,
        constructorNode,
        thisNode,
        explicitParameterAsts = parameterAsts,
        bodyStatementAsts = patternLocalAsts ++ bodyStatementAsts,
        methodReturn = returnNode,
        annotationAsts = Nil,
        modifiers = modifiers,
        startsWithThisCall = false
      )

    val constructorAst = completePartialConstructor(partialConstructor)
    scope.popMethodScope()
    constructorAst
  }

  private def astForEponymousFieldAssignment(
    paramNode: Parameter,
    thisParam: NewMethodParameterIn,
    recordParameter: NewMethodParameterIn
  ): Ast = {
    val thisIdentifier = NewIdentifier()
      .name(thisParam.name)
      .code(thisParam.name)
      .typeFullName(thisParam.typeFullName)
      .lineNumber(recordParameter.lineNumber)
      .columnNumber(recordParameter.columnNumber)
      .dynamicTypeHintFullName(thisParam.dynamicTypeHintFullName)
    val thisIdentifierAst = Ast(thisIdentifier).withRefEdge(thisIdentifier, thisParam)

    val fieldIdentifier = NewFieldIdentifier()
      .canonicalName(recordParameter.name)
      .code(recordParameter.name)

    val fieldAccessNode = operatorCallNode(
      paramNode,
      s"${thisIdentifier.code}.${fieldIdentifier.code}",
      Operators.fieldAccess,
      Option(recordParameter.typeFullName)
    )
    val fieldAccessAst = callAst(fieldAccessNode, thisIdentifierAst :: Ast(fieldIdentifier) :: Nil)

    val recordParamIdentifier = NewIdentifier()
      .name(recordParameter.name)
      .code(recordParameter.name)
      .typeFullName(recordParameter.typeFullName)
      .lineNumber(recordParameter.lineNumber)
      .columnNumber(recordParameter.columnNumber)
      .dynamicTypeHintFullName(recordParameter.dynamicTypeHintFullName)
    val recordParamIdentifierAst = Ast(recordParamIdentifier).withRefEdge(recordParamIdentifier, recordParameter)

    val assignmentNode = operatorCallNode(
      paramNode,
      s"${fieldAccessNode.code} = ${recordParamIdentifier.code}",
      Operators.assignment,
      Option(recordParameter.typeFullName)
    )

    callAst(assignmentNode, fieldAccessAst :: recordParamIdentifierAst :: Nil)
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val maybeArraySuffix     = if (parameter.isVarArgs) "[]" else ""
    val rawParameterTypeName =
      tryWithSafeStackOverflow(parameter.getTypeAsString).map(Util.stripGenericTypes).getOrElse(NameConstants.Unknown)
    val parameterType = tryWithSafeStackOverflow(parameter.getType).toOption
    val typeFullName  =
      parameterType
        .flatMap(typeInfoCalc.fullName)
        .orElse(scope.lookupType(rawParameterTypeName))
        .map(_ ++ maybeArraySuffix)
        .getOrElse(s"${Defines.UnresolvedNamespace}.$rawParameterTypeName")
    val evalStrat =
      if (parameterType.exists(_.isPrimitiveType)) EvaluationStrategies.BY_VALUE else EvaluationStrategies.BY_SHARING
    typeInfoCalc.registerType(typeFullName)

    val parameterNode = NewMethodParameterIn()
      .name(parameter.getName.toString)
      .code(code(parameter))
      .lineNumber(line(parameter))
      .columnNumber(column(parameter))
      .evaluationStrategy(evalStrat)
      .typeFullName(typeFullName)
      .index(childNum)
      .order(childNum)
    val annotationAsts = parameter.getAnnotations.asScala.map(astForAnnotationExpr)
    val ast            = Ast(parameterNode)

    scope.enclosingMethod.get
      .addParameter(parameterNode, binarySignatureCalculator.variableBinarySignature(parameter.getType))

    ast.withChildren(annotationAsts)
  }

  def calcParameterTypes(
    methodLike: ResolvedMethodLikeDeclaration,
    typeParamValues: ResolvedTypeParametersMap
  ): Option[List[String]] = {
    val parameters =
      Range(0, methodLike.getNumberOfParams).flatMap { index =>
        Try(methodLike.getParam(index)).toOption
      }.toList

    calcParameterTypes(parameters, typeParamValues)
  }

  def calcParameterTypes(
    parameters: List[ResolvedParameterDeclaration],
    typeParamValues: ResolvedTypeParametersMap
  ): Option[List[String]] = {
    val parameterTypes = parameters.map { param =>
      tryWithSafeStackOverflow(param.getType).toOption
        .flatMap(paramType => typeInfoCalc.fullName(paramType, typeParamValues))
        // In a scenario where we have an import of an external type e.g. `import foo.bar.Baz` and
        // this parameter's type is e.g. `Baz<String>`, the lookup will fail. However, if we lookup
        // for `Baz` instead (i.e. without type arguments), then the lookup will succeed.
        .orElse(
          Try(param.asInstanceOf[JavaParserParameterDeclaration].getWrappedNode.getType.asClassOrInterfaceType).toOption
            .flatMap(t => scope.lookupType(t.getNameAsString))
        )
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
        .flatMap(typeInfoCalc.fullName(_, typeParamValues))

    composeSignature(maybeReturnType, maybeParameterTypes, method.getNumberOfParams)
  }

  private def astsForParameterList(parameters: List[Parameter]): Seq[Ast] = {
    parameters.zipWithIndex.map { case (param, idx) =>
      astForParameter(param, idx + 1)
    }
  }

  private def partialConstructorAsts(
    constructorDeclarations: List[ConstructorDeclaration | CompactConstructorDeclaration],
    instanceFieldDeclarations: List[FieldDeclaration]
  ): List[PartialConstructorDeclaration] = {
    constructorDeclarations.map { constructorDeclaration =>
      val maybeResolved = Option
        .when(constructorDeclaration.isConstructorDeclaration)(
          tryWithSafeStackOverflow(constructorDeclaration.resolve()).toOption
        )
        .flatten
      val constructorNode = createPartialMethod(constructorDeclaration)
        .name(io.joern.x2cpg.Defines.ConstructorMethodName)

      scope.pushMethodScope(constructorNode, ExpectedType.Void, isStatic = false)
      constructorDeclaration match {
        case regularConstructor: ConstructorDeclaration =>
          val typeParameters = getIdentifiersForTypeParameters(regularConstructor)
          typeParameters.foreach(typeParam => scope.addTypeParameter(typeParam.name, typeParam.typeFullName))
        case _ => // Compact constructor cannot have type parameters
      }

      val parameters = constructorDeclaration match {
        case regularConstructor: ConstructorDeclaration        => regularConstructor.getParameters.asScala.toList
        case compactConstructor: CompactConstructorDeclaration => scope.enclosingTypeDecl.get.recordParameters
      }
      val parameterAsts = astsForParameterList(parameters).toList
      val paramTypes    = constructorDeclaration match {
        case constructor: ConstructorDeclaration        => argumentTypesForMethodLike(maybeResolved)
        case constructor: CompactConstructorDeclaration =>
          val resolvedParams = parameters.flatMap(param => tryWithSafeStackOverflow(param.resolve()).toOption).toList
          calcParameterTypes(resolvedParams, ResolvedTypeParametersMap.empty())
      }
      val signature    = composeSignature(Some(TypeConstants.Void), paramTypes, parameterAsts.size)
      val typeFullName = scope.enclosingTypeDecl.fullName
      val fullName     =
        composeMethodFullName(
          typeFullName.getOrElse(Defines.UnresolvedNamespace),
          Defines.ConstructorMethodName,
          signature
        )

      constructorNode
        .fullName(fullName)
        .signature(signature)

      parameterAsts.zip(parameters).foreach { (ast, parameterNode) =>
        ast.root match {
          case Some(parameter: NewMethodParameterIn) =>
            val genericType = binarySignatureCalculator.variableBinarySignature(parameterNode.getType)
            scope.enclosingMethod.get.addParameter(parameter, genericType)
          case _ => // This should never happen
        }
      }

      val thisNode = thisNodeForMethod(constructorDeclaration, typeFullName)
      scope.enclosingMethod.get.addParameter(thisNode, scope.enclosingTypeDecl.get.typeDecl.genericSignature)

      scope.pushBlockScope()
      val recordParameterAssignments = constructorDeclaration match {
        case constructor: CompactConstructorDeclaration =>
          parameterAsts.zipWithIndex
            .collect { case (ast, idx) =>
              (ast.nodes.collectFirst { case node: NewMethodParameterIn => node }.head, idx)
            }
            .map { case (newMethodParameterIn, idx) =>
              astForEponymousFieldAssignment(parameters(idx), thisNode, newMethodParameterIn)
            }
        case _ => Nil
      }

      val bodyStatements   = constructorDeclaration.getBody.getStatements.asScala.toList
      val statementsAsts   = bodyStatements.flatMap(astsForStatement)
      val bodyContainsThis = bodyStatements.headOption
        .collect { case consInvocation: ExplicitConstructorInvocationStmt => consInvocation.isThis }
        .getOrElse(false)
      val fieldAssignmentsAndTempLocals =
        if (bodyContainsThis)
          Nil
        else
          scope.enclosingMethod.get.getAndClearUnaddedPatternLocals().map(Ast(_)) ++ astsForFieldInitializers(
            instanceFieldDeclarations
          )

      // The this(...) call must always be the first statement in the body, but adding the fieldAssignmentsAndTempLocals
      // before the body asts here is safe, since the list will be empty if the body does start with this()
      val bodyAsts = recordParameterAssignments ++ fieldAssignmentsAndTempLocals ++ statementsAsts
      scope.popBlockScope()
      val methodReturn = constructorReturnNode(constructorDeclaration)

      val annotationAsts = constructorDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toList

      val modifiers =
        NewModifier().modifierType(ModifierTypes.CONSTRUCTOR) :: modifiersForMethod(constructorDeclaration).filterNot(
          _.modifierType == ModifierTypes.VIRTUAL
        )

      scope.popMethodScope()
      PartialConstructorDeclaration(
        constructorDeclaration,
        constructorNode,
        thisNode,
        parameterAsts,
        bodyAsts.toList,
        methodReturn,
        annotationAsts,
        modifiers,
        bodyContainsThis
      )
    }
  }

  private def assignmentForCapture(
    originNode: Node,
    parameter: NewMethodParameterIn,
    thisParam: NewMethodParameterIn
  ): Ast = {
    val assignment =
      operatorCallNode(
        originNode,
        s"this.${parameter.name} = ${parameter.name}",
        Operators.assignment,
        Some(parameter.typeFullName)
      )
    val fieldAccess =
      operatorCallNode(originNode, s"this.${parameter.name}", Operators.fieldAccess, Some(thisParam.typeFullName))

    val fieldAccessTarget =
      identifierNode(originNode, "this", "this", thisParam.typeFullName, List(thisParam.typeFullName))
    val fieldIdentifier = fieldIdentifierNode(originNode, parameter.name, parameter.name)

    val sourceIdentifier =
      identifierNode(originNode, parameter.name, parameter.name, parameter.typeFullName)

    diffGraph.addEdge(fieldAccessTarget, thisParam, EdgeTypes.REF)
    diffGraph.addEdge(sourceIdentifier, parameter, EdgeTypes.REF)

    val fieldAccessAst = callAst(fieldAccess, List(fieldAccessTarget, fieldIdentifier).map(Ast(_)))

    callAst(assignment, List(fieldAccessAst, Ast(sourceIdentifier)))

  }

  private def completePartialConstructor(partialConstructor: PartialConstructorDeclaration): Ast = {
    val paramsForCaptures = scope.enclosingTypeDecl.getUsedCaptures().zipWithIndex.map { case (variable, index) =>
      parameterInNode(
        partialConstructor.originNode,
        variable.name,
        variable.name,
        partialConstructor.explicitParameterAsts.length + 1 + index,
        isVariadic = false,
        EvaluationStrategies.BY_VALUE,
        variable.typeFullName
      )
    }

    val thisNode               = partialConstructor.thisNode
    val assignmentsForCaptures =
      if (partialConstructor.startsWithThisCall) {
        // Register the synthetic capture parameters for this constructor's this() call
        // so that REF edges can be created correctly in addArgsToPartialInits
        partialConstructor.bodyStatementAsts.headOption
          .flatMap(_.root)
          .collect { case initCall: NewCall => initCall }
          .foreach { initCall =>
            scope.enclosingTypeDecl.foreach(_.registerThisCallCaptureParams(initCall, paramsForCaptures))
          }
        Nil
      } else {
        paramsForCaptures.map(assignmentForCapture(partialConstructor.originNode, _, partialConstructor.thisNode))
      }

    val bodyAst =
      astForConstructorBody(partialConstructor.originNode, partialConstructor.bodyStatementAsts, assignmentsForCaptures)

    methodAstWithAnnotations(
      partialConstructor.constructorNode,
      Ast(partialConstructor.thisNode) :: (partialConstructor.explicitParameterAsts ++ paramsForCaptures.map(Ast(_))),
      bodyAst,
      partialConstructor.methodReturn,
      partialConstructor.modifiers,
      partialConstructor.annotationAsts
    )
  }

  def astsForConstructors(
    constructorDeclarations: List[ConstructorDeclaration | CompactConstructorDeclaration],
    instanceFieldDeclarations: List[FieldDeclaration]
  ): Map[Node, Ast] = {
    val partialConstructors = partialConstructorAsts(constructorDeclarations, instanceFieldDeclarations)
    partialConstructors.map { partialConstructor =>
      partialConstructor.originNode -> completePartialConstructor(partialConstructor)
    }.toMap
  }

  private def constructorReturnNode(
    constructorDeclaration: ConstructorDeclaration | CompactConstructorDeclaration
  ): NewMethodReturn = {
    methodReturnNode(constructorDeclaration, TypeConstants.Void)
  }

  private def astForConstructorBody(originNode: Node, bodyStmts: List[Ast], captureInitializers: List[Ast]): Ast = {
    val hasThisCall = bodyStmts.headOption
      .flatMap(_.root)
      .collect { case thisCall: NewCall => thisCall.name == "<init>" }
      .getOrElse(false)

    val statementsInOrder = bodyStmts match {
      case Nil => captureInitializers

      case head :: tail if hasThisCall =>
        head :: (captureInitializers ++ tail)

      case bodyStmts => captureInitializers ++ bodyStmts
    }

    Ast(blockNode(originNode)).withChildren(statementsInOrder)
  }

  /** Constructor and Method declarations share a lot of fields, so this method adds the fields they have in common.
    * `fullName` and `signature` are omitted
    */
  private def createPartialMethod(
    declaration: CallableDeclaration[?] | CompactConstructorDeclaration | AnnotationMemberDeclaration
  ): NewMethod = {
    val methodCode = declaration match {
      case callableDeclaration: CallableDeclaration[?]       => callableDeclaration.getDeclarationAsString.trim
      case compactConstructor: CompactConstructorDeclaration => code(compactConstructor)
      case annotationMember: AnnotationMemberDeclaration     => code(annotationMember)
    }
    val columnNumber = declaration.getBegin.map(x => Integer.valueOf(x.column)).toScala
    val endLine      = declaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val endColumn    = declaration.getEnd.map(x => Integer.valueOf(x.column)).toScala

    val placeholderFullName = ""

    val genericSignature = declaration match {
      case callableDeclaration: CallableDeclaration[_] =>
        binarySignatureCalculator.methodBinarySignature(callableDeclaration)
      case compactConstructor: CompactConstructorDeclaration =>
        binarySignatureCalculator.defaultConstructorSignature(scope.enclosingTypeDecl.get.recordParameters)
      case annotationMember: AnnotationMemberDeclaration =>
        binarySignatureCalculator.annotationMemberBinarySignature(annotationMember)
    }
    methodNode(
      declaration,
      declaration.getNameAsString(),
      methodCode,
      placeholderFullName,
      None,
      filename,
      genericSignature = Option(genericSignature)
    )
  }

  def thisNodeForMethod(originNode: Node, maybeTypeFullName: Option[String]): NewMethodParameterIn = {
    val typeFullName = typeInfoCalc.registerType(maybeTypeFullName.getOrElse(defaultTypeFallback()))
    parameterInNode(
      originNode,
      NameConstants.This,
      NameConstants.This,
      index = 0,
      isVariadic = false,
      typeFullName = Option(typeFullName),
      dynamicTypeHintFullName = maybeTypeFullName.toSeq,
      evaluationStrategy = EvaluationStrategies.BY_SHARING
    )
  }
}

object AstForMethodsCreator {
  private case class PartialConstructorDeclaration(
    originNode: Node,
    constructorNode: NewMethod,
    thisNode: NewMethodParameterIn,
    explicitParameterAsts: List[Ast],
    bodyStatementAsts: List[Ast],
    methodReturn: NewMethodReturn,
    annotationAsts: List[Ast],
    modifiers: List[NewModifier],
    startsWithThisCall: Boolean
  )
}
