package io.joern.javasrc2cpg.astcreation.declarations

import io.joern.x2cpg.utils.AstPropertiesUtil.*
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.{
  CallableDeclaration,
  ConstructorDeclaration,
  FieldDeclaration,
  MethodDeclaration,
  Parameter,
  VariableDeclarator
}
import com.github.javaparser.ast.stmt.{BlockStmt, ExplicitConstructorInvocationStmt}
import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedMethodLikeDeclaration}
import com.github.javaparser.resolution.types.ResolvedType
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.Util.*
import io.joern.x2cpg.utils.NodeBuilders
import io.joern.x2cpg.utils.NodeBuilders.*
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.{
  NewBlock,
  NewIdentifier,
  NewMethod,
  NewMethodParameterIn,
  NewMethodReturn,
  NewModifier
}
import io.shiftleft.codepropertygraph.generated.{EvaluationStrategies, ModifierTypes}
import io.joern.javasrc2cpg.scope.JavaScopeElement.fullName

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}
import io.shiftleft.codepropertygraph.generated.nodes.AstNodeNew
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import com.github.javaparser.ast.Node
import com.github.javaparser.symbolsolver.javaparsermodel.declarations.JavaParserParameterDeclaration
import io.joern.javasrc2cpg.astcreation.declarations.AstForMethodsCreator.PartialConstructorDeclaration
import io.joern.javasrc2cpg.util.{NameConstants, Util}

private[declarations] trait AstForMethodsCreator { this: AstCreator =>
  def astForMethod(methodDeclaration: MethodDeclaration): Ast = {
    val methodNode = createPartialMethod(methodDeclaration)

    val typeParameters = getIdentifiersForTypeParameters(methodDeclaration)

    val maybeResolved = tryWithSafeStackOverflow(methodDeclaration.resolve())
    val expectedReturnType = tryWithSafeStackOverflow(
      symbolSolver.toResolvedType(methodDeclaration.getType, classOf[ResolvedType])
    ).toOption
    val simpleMethodReturnType =
      tryWithSafeStackOverflow(methodDeclaration.getTypeAsString).map(Util.stripGenericTypes).toOption
    val returnTypeFullName = expectedReturnType
      .flatMap(typeInfoCalc.fullName)
      .orElse(simpleMethodReturnType.flatMap(scope.lookupType(_)))
      .orElse(
        tryWithSafeStackOverflow(methodDeclaration.getType.asClassOrInterfaceType).toOption.flatMap(t =>
          scope.lookupType(t.getNameAsString)
        )
      )
      .orElse(typeParameters.find(typeParam => simpleMethodReturnType.contains(typeParam.name)).map(_.typeFullName))

    scope.pushMethodScope(
      methodNode,
      ExpectedType(returnTypeFullName, expectedReturnType),
      methodDeclaration.isStatic()
    )
    typeParameters.foreach { typeParameter => scope.addTopLevelType(typeParameter.name, typeParameter.typeFullName) }

    val parameterAsts  = astsForParameterList(methodDeclaration.getParameters)
    val parameterTypes = argumentTypesForMethodLike(maybeResolved)
    val signature      = composeSignature(returnTypeFullName, parameterTypes, parameterAsts.size)
    val namespaceName  = scope.enclosingTypeDecl.fullName.getOrElse(Defines.UnresolvedNamespace)
    val methodFullName = composeMethodFullName(namespaceName, methodDeclaration.getNameAsString, signature)

    methodNode
      .fullName(methodFullName)
      .signature(signature)

    val thisNode = Option.when(!methodDeclaration.isStatic) {
      val typeFullName = scope.enclosingTypeDecl.fullName
      thisNodeForMethod(typeFullName, line(methodDeclaration))
    }
    val thisAst = thisNode.map(Ast(_)).toList

    thisNode.foreach { node =>
      scope.enclosingMethod.get.addParameter(node)
    }

    val bodyAst = methodDeclaration.getBody.toScala.map(astForBlockStatement(_)).getOrElse(Ast(NewBlock()))
    val (lineNr, columnNr) = tryWithSafeStackOverflow(methodDeclaration.getType) match {
      case Success(typ) => (line(typ), column(typ))
      case Failure(_)   => (line(methodDeclaration), column(methodDeclaration))
    }
    val methodReturn = newMethodReturnNode(returnTypeFullName.getOrElse(TypeConstants.Any), None, lineNr, columnNr)

    val annotationAsts = methodDeclaration.getAnnotations.asScala.map(astForAnnotationExpr).toSeq

    val modifiers = modifiersForMethod(methodDeclaration)

    scope.popMethodScope()

    methodAstWithAnnotations(methodNode, thisAst ++ parameterAsts, bodyAst, methodReturn, modifiers, annotationAsts)
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

  private def modifiersForMethod(methodDeclaration: CallableDeclaration[?]): List[NewModifier] = {
    val isInterfaceMethod = scope.enclosingTypeDecl.isInterface

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
        scope.popFieldDeclScope()
        assignmentAsts
      }
    }
  }

  def astForDefaultConstructor(originNode: Node, instanceFieldDeclarations: List[FieldDeclaration]): Ast = {
    val typeFullName = scope.enclosingTypeDecl.fullName
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

    scope.pushMethodScope(constructorNode, ExpectedType.Void, isStatic = false)

    val thisNode = thisNodeForMethod(typeFullName, lineNumber = None)
    scope.enclosingMethod.foreach(_.addParameter(thisNode))
    val bodyStatementAsts = astsForFieldInitializers(instanceFieldDeclarations)

    val returnNode = newMethodReturnNode(TypeConstants.Void, line = None, column = None)

    val modifiers = List(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.PUBLIC))
    val partialConstructor =
      PartialConstructorDeclaration(
        originNode,
        constructorNode,
        thisNode,
        explicitParameterAsts = Nil,
        bodyStatementAsts = bodyStatementAsts,
        methodReturn = returnNode,
        annotationAsts = Nil,
        modifiers = modifiers,
        startsWithThisCall = false
      )

    val constructorAst = completePartialConstructor(partialConstructor)
    scope.popMethodScope()
    constructorAst
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

    scope.enclosingMethod.get.addParameter(parameterNode)

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
          tryWithSafeStackOverflow(param.getType).toOption
            .flatMap(paramType => typeInfoCalc.fullName(paramType, typeParamValues))
            // In a scenario where we have an import of an external type e.g. `import foo.bar.Baz` and
            // this parameter's type is e.g. `Baz<String>`, the lookup will fail. However, if we lookup
            // for `Baz` instead (i.e. without type arguments), then the lookup will succeed.
            .orElse(
              Try(
                param.asInstanceOf[JavaParserParameterDeclaration].getWrappedNode.getType.asClassOrInterfaceType
              ).toOption.flatMap(t => scope.lookupType(t.getNameAsString))
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
  private def astsForParameterList(parameters: NodeList[Parameter]): Seq[Ast] = {
    parameters.asScala.toList.zipWithIndex.map { case (param, idx) =>
      astForParameter(param, idx + 1)
    }
  }

  private def partialConstructorAsts(
    constructorDeclarations: List[ConstructorDeclaration],
    instanceFieldDeclarations: List[FieldDeclaration]
  ): List[PartialConstructorDeclaration] = {
    constructorDeclarations.map { constructorDeclaration =>
      val constructorNode = createPartialMethod(constructorDeclaration)
        .name(io.joern.x2cpg.Defines.ConstructorMethodName)

      scope.pushMethodScope(constructorNode, ExpectedType.Void, isStatic = false)
      val maybeResolved = tryWithSafeStackOverflow(constructorDeclaration.resolve())

      val parameterAsts = astsForParameterList(constructorDeclaration.getParameters).toList
      val paramTypes    = argumentTypesForMethodLike(maybeResolved)
      val signature     = composeSignature(Some(TypeConstants.Void), paramTypes, parameterAsts.size)
      val typeFullName  = scope.enclosingTypeDecl.fullName
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
          case Some(parameter: NewMethodParameterIn) => scope.enclosingMethod.get.addParameter(parameter)
          case _                                     => // This should never happen
        }
      }

      val thisNode = thisNodeForMethod(typeFullName, line(constructorDeclaration))
      scope.enclosingMethod.get.addParameter(thisNode)

      scope.pushBlockScope()
      val bodyStatements = constructorDeclaration.getBody.getStatements.asScala.toList
      val bodyContainsThis = bodyStatements.headOption
        .collect { case consInvocation: ExplicitConstructorInvocationStmt => consInvocation.isThis }
        .getOrElse(false)
      val fieldAssignments =
        if (bodyContainsThis)
          Nil
        else
          astsForFieldInitializers(instanceFieldDeclarations)

      // The this(...) call must always be the first statement in the body, but adding the fieldAssignments
      // before the body asts here is safe, since the list will be empty if the body does start with this()
      val bodyAsts = fieldAssignments ++ bodyStatements.flatMap(astsForStatement)
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
    constructorDeclarations: List[ConstructorDeclaration],
    instanceFieldDeclarations: List[FieldDeclaration]
  ): Map[Node, Ast] = {
    val partialConstructors = partialConstructorAsts(constructorDeclarations, instanceFieldDeclarations)
    partialConstructors.map { partialConstructor =>
      partialConstructor.originNode -> completePartialConstructor(partialConstructor)
    }.toMap
  }

  private def constructorReturnNode(constructorDeclaration: ConstructorDeclaration): NewMethodReturn = {
    val line   = constructorDeclaration.getEnd.map(x => x.line).toScala
    val column = constructorDeclaration.getEnd.map(x => x.column).toScala
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
  private def createPartialMethod(declaration: CallableDeclaration[?]): NewMethod = {
    val code         = declaration.getDeclarationAsString.trim
    val columnNumber = declaration.getBegin.map(x => Integer.valueOf(x.column)).toScala
    val endLine      = declaration.getEnd.map(x => Integer.valueOf(x.line)).toScala
    val endColumn    = declaration.getEnd.map(x => Integer.valueOf(x.column)).toScala

    val placeholderFullName = ""
    methodNode(declaration, declaration.getNameAsString(), code, placeholderFullName, None, filename)
  }

  def thisNodeForMethod(maybeTypeFullName: Option[String], lineNumber: Option[Int]): NewMethodParameterIn = {
    val typeFullName = typeInfoCalc.registerType(maybeTypeFullName.getOrElse(TypeConstants.Any))
    NodeBuilders.newThisParameterNode(
      typeFullName = typeFullName,
      dynamicTypeHintFullName = maybeTypeFullName.toSeq,
      line = lineNumber
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
