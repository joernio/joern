package io.joern.javasrc2cpg.astcreation.declarations

import io.joern.x2cpg.utils.AstPropertiesUtil.*
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.{
  CallableDeclaration,
  CompactConstructorDeclaration,
  ConstructorDeclaration,
  FieldDeclaration,
  MethodDeclaration,
  Parameter,
  VariableDeclarator
}
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt}
import com.github.javaparser.resolution.declarations.{
  ResolvedMethodDeclaration,
  ResolvedMethodLikeDeclaration,
  ResolvedParameterDeclaration
}
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNodeNew,
  NewBlock,
  NewCall,
  NewFieldIdentifier,
  NewIdentifier,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewModifier
}
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  NodeTypes,
  Operators,
  nodes
}
import io.joern.javasrc2cpg.scope.JavaScopeElement.fullName

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserParameterDeclaration
import io.joern.javasrc2cpg.astcreation.declarations.AstForMethodsCreator.PartialConstructorDeclaration
import io.joern.javasrc2cpg.util.{NameConstants, Util}

private[declarations] trait AstForMethodsCreator { this: AstCreator =>
  def astForMethod(methodDeclaration: MethodDeclaration): Ast = {
    val methodNode = createPartialMethod(methodDeclaration)

    val typeParameters = getIdentifiersForTypeParameters(methodDeclaration)
    methodDeclaration.getType

    val maybeResolved = tryWithSafeStackOverflow(methodDeclaration.resolve()).toOption
    val expectedReturnType = tryWithSafeStackOverflow(
      symbolSolver.toResolvedType(methodDeclaration.getType, classOf[ResolvedType])
    ).toOption
    val simpleMethodReturnType =
      tryWithSafeStackOverflow(methodDeclaration.getTypeAsString).map(Util.stripGenericTypes).toOption
    val returnTypeFullName = expectedReturnType
      .flatMap(typeInfoCalc.fullName)
      .orElse(simpleMethodReturnType.flatMap(scope.lookupType(_)))
      .orElse(tryWithSafeStackOverflow(methodDeclaration.getType).toOption.collect { case t: ClassOrInterfaceType =>
        scope.lookupType(t.getNameAsString)
      }.flatten)
      .orElse(typeParameters.find(typeParam => simpleMethodReturnType.contains(typeParam.name)).map(_.typeFullName))

    scope.pushMethodScope(
      methodNode,
      ExpectedType(returnTypeFullName, expectedReturnType),
      methodDeclaration.isStatic()
    )
    typeParameters.foreach { typeParameter => scope.addTypeParameter(typeParameter.name, typeParameter.typeFullName) }

    val genericSignature = binarySignatureCalculator.methodBinarySignature(methodDeclaration)
    methodNode.genericSignature(genericSignature)

    val parameterAsts  = astsForParameterList(methodDeclaration.getParameters.asScala.toList)
    val parameterTypes = argumentTypesForMethodLike(maybeResolved)
    val signature      = composeSignature(returnTypeFullName, parameterTypes, parameterAsts.size)
    val namespaceName  = scope.enclosingTypeDecl.fullName.getOrElse(Defines.UnresolvedNamespace)
    val methodFullName = composeMethodFullName(namespaceName, methodDeclaration.getNameAsString, signature)

    methodNode
      .fullName(methodFullName)
      .signature(signature)

    val thisNode = Option.when(!methodDeclaration.isStatic) {
      val typeFullName = scope.enclosingTypeDecl.fullName
      thisNodeForMethod(typeFullName, line(methodDeclaration), column(methodDeclaration))
    }
    val thisAst = thisNode.map(Ast(_)).toList

    thisNode.foreach { node =>
      scope.enclosingMethod.get.addParameter(node, scope.enclosingTypeDecl.get.typeDecl.genericSignature)
    }

    val bodyAst = methodDeclaration.getBody.toScala
      .map(astForBlockStatement(_, includeTemporaryLocals = true))
      .getOrElse(Ast(NewBlock()))
    val (lineNr, columnNr) = tryWithSafeStackOverflow(methodDeclaration.getType) match {
      case Success(typ) => (line(typ), column(typ))
      case Failure(_)   => (line(methodDeclaration), column(methodDeclaration))
    }
    val methodReturn =
      newMethodReturnNode(
        returnTypeFullName.getOrElse(defaultTypeFallback(methodDeclaration.getType)),
        None,
        lineNr,
        columnNr
      )

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    val modifiers = modifiersForMethod(methodDeclaration)

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
      newMethodReturnNode(parameterTypeFullName, line = line(parameter), column = column(parameter))

    val genericSignature = binarySignatureCalculator.recordParameterAccessorBinarySignature(parameter)
    val methodRoot = methodNode(
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

    val modifier = newModifierNode(ModifierTypes.PUBLIC)

    val thisParameter = thisNodeForMethod(Option(recordTypeFullName), line(parameter), column(parameter))

    val thisIdentifier    = identifierNode(parameter, thisParameter.name, thisParameter.code, recordTypeFullName)
    val thisIdentifierAst = Ast(thisIdentifier).withRefEdge(thisIdentifier, thisParameter)
    val fieldIdentifier   = fieldIdentifierNode(parameter, parameterName, parameterName)

    val fieldAccessNode = newOperatorCallNode(
      Operators.fieldAccess,
      s"${thisIdentifier.code}.${fieldIdentifier.code}",
      Option(parameterTypeFullName),
      line(parameter),
      column(parameter)
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
          newModifierNode(ModifierTypes.ABSTRACT)
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
    val staticVirtualModifier = Some(newModifierNode(staticVirtualModifierType))

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

  private def getIdentifiersForTypeParameters(methodDeclaration: CallableDeclaration[?]): List[NewIdentifier] = {
    methodDeclaration.getTypeParameters.asScala.map { typeParameter =>
      val name = typeParameter.getNameAsString
      val typeFullName = tryWithSafeStackOverflow(typeParameter.getTypeBound.asScala.headOption).toOption.flatten
        .flatMap(typeInfoCalc.fullName)
        .getOrElse(TypeConstants.Object)
      typeInfoCalc.registerType(typeFullName)

      NewIdentifier().name(name).typeFullName(typeFullName)
    }.toList
  }

  def clinitAstFromStaticInits(staticInits: Seq[Ast]): Option[Ast] = {
    Option.when(staticInits.nonEmpty) {
      val signature         = composeMethodLikeSignature(TypeConstants.Void, Nil)
      val enclosingDeclName = scope.enclosingTypeDecl.fullName.getOrElse(Defines.UnresolvedNamespace)
      val fullName          = composeMethodFullName(enclosingDeclName, Defines.StaticInitMethodName, signature)
      staticInitMethodAst(staticInits.toList, fullName, Some(signature), TypeConstants.Void)
    }
  }

  private def astsForFieldInitializers(fieldDeclarations: List[FieldDeclaration]): List[Ast] = {
    fieldDeclarations.flatMap { fieldDeclaration =>
      fieldDeclaration.getVariables.asScala.filter(_.getInitializer.isPresent).toList.flatMap { variableDeclaration =>
        scope.pushFieldDeclScope(fieldDeclaration.isStatic, variableDeclaration.getNameAsString)
        val assignmentAsts = astsForVariableDeclarator(variableDeclaration, fieldDeclaration)
        val patternAsts    = scope.enclosingMethod.get.getUnaddedPatternVariableAstsAndMarkAdded()
        scope.popFieldDeclScope()
        patternAsts ++ assignmentAsts
      }
    }
  }

  def astForDefaultConstructor(originNode: Node, instanceFieldDeclarations: List[FieldDeclaration]): Ast = {
    val parameters       = scope.enclosingTypeDecl.get.recordParameters
    val genericSignature = binarySignatureCalculator.defaultConstructorSignature(parameters)
    val constructorNode = NewMethod()
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
    val fullName = composeMethodFullName(
      typeFullName.getOrElse(Defines.UnresolvedNamespace),
      Defines.ConstructorMethodName,
      signature
    )

    constructorNode.fullName(fullName)
    constructorNode.signature(signature)

    val thisNode = thisNodeForMethod(typeFullName, lineNumber = None, columnNumber = None)
    scope.enclosingMethod.foreach(_.addParameter(thisNode, scope.enclosingTypeDecl.get.typeDecl.genericSignature))
    val recordParameterAssignments = parameterAsts
      .flatMap(_.nodes)
      .collect { case param: nodes.NewMethodParameterIn => param }
      .map(astForEponymousFieldAssignment(thisNode, _))
    val bodyStatementAsts =
      astsForFieldInitializers(instanceFieldDeclarations) ++ recordParameterAssignments
    val temporaryLocalAsts = scope.enclosingMethod.map(_.getTemporaryLocals).getOrElse(Nil).map(Ast(_))

    val returnNode = newMethodReturnNode(TypeConstants.Void, line = None, column = None)

    val modifiers = List(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.PUBLIC))
    val partialConstructor =
      PartialConstructorDeclaration(
        originNode,
        constructorNode,
        thisNode,
        explicitParameterAsts = parameterAsts,
        bodyStatementAsts = temporaryLocalAsts ++ bodyStatementAsts,
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

    val fieldAccessNode = newOperatorCallNode(
      Operators.fieldAccess,
      s"${thisIdentifier.code}.${fieldIdentifier.code}",
      Option(recordParameter.typeFullName),
      recordParameter.lineNumber,
      recordParameter.columnNumber
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

    val assignmentNode = newOperatorCallNode(
      Operators.assignment,
      s"${fieldAccessNode.code} = ${recordParamIdentifier.code}",
      Option(recordParameter.typeFullName),
      recordParameter.lineNumber,
      recordParameter.columnNumber
    )

    callAst(assignmentNode, fieldAccessAst :: recordParamIdentifierAst :: Nil)
  }

  private def astForParameter(parameter: Parameter, childNum: Int): Ast = {
    val maybeArraySuffix = if (parameter.isVarArgs) "[]" else ""
    val rawParameterTypeName =
      tryWithSafeStackOverflow(parameter.getTypeAsString).map(Util.stripGenericTypes).getOrElse(NameConstants.Unknown)
    val parameterType = tryWithSafeStackOverflow(parameter.getType).toOption
    val typeFullName =
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
      val paramTypes = constructorDeclaration match {
        case constructor: ConstructorDeclaration => argumentTypesForMethodLike(maybeResolved)
        case constructor: CompactConstructorDeclaration =>
          val resolvedParams = parameters.flatMap(param => tryWithSafeStackOverflow(param.resolve()).toOption).toList
          calcParameterTypes(resolvedParams, ResolvedTypeParametersMap.empty())
      }
      val signature    = composeSignature(Some(TypeConstants.Void), paramTypes, parameterAsts.size)
      val typeFullName = scope.enclosingTypeDecl.fullName
      val fullName =
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

      val thisNode = thisNodeForMethod(typeFullName, line(constructorDeclaration), column(constructorDeclaration))
      scope.enclosingMethod.get.addParameter(thisNode, scope.enclosingTypeDecl.get.typeDecl.genericSignature)

      scope.pushBlockScope()
      val recordParameterAssignments = constructorDeclaration match {
        case constructor: CompactConstructorDeclaration =>
          parameterAsts
            .flatMap(_.nodes)
            .collect { case param: nodes.NewMethodParameterIn => param }
            .map(astForEponymousFieldAssignment(thisNode, _))
        case _ => Nil
      }

      val bodyStatements = constructorDeclaration.getBody.getStatements.asScala.toList
      val statementsAsts = bodyStatements.flatMap(astsForStatement)
      val bodyContainsThis = bodyStatements.headOption
        .collect { case consInvocation: ExplicitConstructorInvocationStmt => consInvocation.isThis }
        .getOrElse(false)
      val fieldAssignmentsAndTempLocals =
        if (bodyContainsThis)
          Nil
        else
          scope.enclosingMethod.get.getTemporaryLocals.map(Ast(_)) ++ astsForFieldInitializers(
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
      newOperatorCallNode(
        Operators.assignment,
        s"this.${parameter.name} = ${parameter.name}",
        Some(parameter.typeFullName),
        parameter.lineNumber,
        parameter.columnNumber
      )
    val fieldAccess = newOperatorCallNode(
      Operators.fieldAccess,
      s"this.${parameter.name}",
      Some(thisParam.typeFullName),
      parameter.lineNumber,
      parameter.columnNumber
    )

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

    val thisNode = partialConstructor.thisNode
    val assignmentsForCaptures =
      if (partialConstructor.startsWithThisCall)
        Nil
      else
        paramsForCaptures.map(assignmentForCapture(partialConstructor.originNode, _, partialConstructor.thisNode))

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
    val line   = constructorDeclaration.getEnd.map(_.line).toScala
    val column = constructorDeclaration.getEnd.map(_.column).toScala
    newMethodReturnNode(TypeConstants.Void, None, line, column)
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
  private def createPartialMethod(declaration: CallableDeclaration[?] | CompactConstructorDeclaration): NewMethod = {
    val methodCode = declaration match {
      case callableDeclaration: CallableDeclaration[?]       => callableDeclaration.getDeclarationAsString.trim
      case compactConstructor: CompactConstructorDeclaration => code(compactConstructor)
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

  def thisNodeForMethod(
    maybeTypeFullName: Option[String],
    lineNumber: Option[Int],
    columnNumber: Option[Int]
  ): NewMethodParameterIn = {
    val typeFullName = typeInfoCalc.registerType(maybeTypeFullName.getOrElse(defaultTypeFallback()))
    NodeBuilders.newThisParameterNode(
      typeFullName = typeFullName,
      dynamicTypeHintFullName = maybeTypeFullName.toSeq,
      line = lineNumber,
      column = columnNumber
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
