package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.expr.LambdaExpr
import io.shiftleft.codepropertygraph.generated.nodes.NewLocal
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.joern.x2cpg.Ast
import io.joern.javasrc2cpg.util.BindingTable
import io.joern.javasrc2cpg.util.LambdaBindingInfo
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.stmt.BlockStmt
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.shiftleft.codepropertygraph.generated.nodes.NewReturn
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.joern.x2cpg.utils.NodeBuilders.{newClosureBindingNode, newModifierNode, newMethodReturnNode}
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import com.github.javaparser.resolution.types.ResolvedReferenceType
import com.github.javaparser.resolution.declarations.ResolvedMethodDeclaration
import scala.jdk.CollectionConverters.*
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.shiftleft.codepropertygraph.generated.nodes.MethodParameterIn.PropertyDefaults as ParameterDefaults
import io.joern.javasrc2cpg.util.Util.composeMethodLikeSignature
import io.joern.javasrc2cpg.util.Util.composeUnresolvedSignature
import io.joern.x2cpg.Defines
import io.joern.javasrc2cpg.util.Util.composeMethodFullName
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import org.slf4j.LoggerFactory
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Try, Success, Failure}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.ObjectMethodSignatures
import io.shiftleft.passes.IntervalKeyPool
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.joern.javasrc2cpg.util.BindingTableAdapterForLambdas
import io.joern.javasrc2cpg.util.BindingTable.createBindingTable
import com.github.javaparser.resolution.types.ResolvedTypeVariable
import com.github.javaparser.resolution.types.ResolvedType
import io.joern.javasrc2cpg.astcreation.AstCreator
import io.joern.javasrc2cpg.astcreation.ExpectedType
import io.joern.javasrc2cpg.astcreation.expressions.AstForLambdasCreator.LambdaImplementedInfo
import io.joern.javasrc2cpg.scope.Scope.ScopeVariable
import io.shiftleft.codepropertygraph.generated.nodes.NewClosureBinding
import io.joern.javasrc2cpg.astcreation.expressions.AstForLambdasCreator.ClosureBindingEntry

object AstForLambdasCreator {
  case class LambdaImplementedInfo(
    implementedInterface: Option[ResolvedReferenceType],
    implementedMethod: Option[ResolvedMethodDeclaration]
  )

  case class ClosureBindingEntry(node: ScopeVariable, binding: NewClosureBinding)
}

private[expressions] trait AstForLambdasCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  private val LambdaNamePrefix = "lambda$"
  private val lambdaKeyPool    = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  private def nextLambdaName(): String = {
    s"$LambdaNamePrefix${lambdaKeyPool.next}"
  }

  private def createAndPushLambdaMethod(
    expr: LambdaExpr,
    lambdaMethodName: String,
    implementedInfo: LambdaImplementedInfo,
    localsForCaptured: Seq[NewLocal],
    expectedLambdaType: ExpectedType
  ): NewMethod = {
    val implementedMethod    = implementedInfo.implementedMethod
    val implementedInterface = implementedInfo.implementedInterface

    // We need to get this information from the expected type as the JavaParser
    // symbol solver returns the erased types when resolving the lambda itself.
    val expectedTypeParamTypes = genericParamTypeMapForLambda(expectedLambdaType)
    val parametersWithoutThis  = buildParamListForLambda(expr, implementedMethod, expectedTypeParamTypes)

    val returnType = getLambdaReturnType(implementedInterface, implementedMethod, expectedTypeParamTypes)

    val lambdaMethodBody = astForLambdaBody(expr.getBody, localsForCaptured, returnType)

    val thisParam = lambdaMethodBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .find { identifier => identifier.name == NameConstants.This || identifier.name == NameConstants.Super }
      .map { _ =>
        val typeFullName = scope.enclosingTypeDeclFullName
        Ast(thisNodeForMethod(typeFullName, line(expr)))
      }
      .toList

    val parameters = thisParam ++ parametersWithoutThis

    val lambdaMethodNode = createLambdaMethodNode(lambdaMethodName, parametersWithoutThis, returnType)
    val returnNode       = newMethodReturnNode(returnType.getOrElse(TypeConstants.Any), None, line(expr), column(expr))
    val virtualModifier  = Some(newModifierNode(ModifierTypes.VIRTUAL))
    val staticModifier   = Option.when(thisParam.isEmpty)(newModifierNode(ModifierTypes.STATIC))
    val privateModifier  = Some(newModifierNode(ModifierTypes.PRIVATE))

    val modifiers = List(virtualModifier, staticModifier, privateModifier).flatten.map(Ast(_))

    val lambdaParameterNamesToNodes =
      parameters
        .flatMap(_.root)
        .collect { case param: NewMethodParameterIn => param }
        .map { param => param.name -> param }
        .toMap

    val identifiersMatchingParams = lambdaMethodBody.nodes
      .collect { case identifier: NewIdentifier => identifier }
      .filter { identifier => lambdaParameterNamesToNodes.contains(identifier.name) }

    val lambdaMethodAstWithoutRefs =
      Ast(lambdaMethodNode)
        .withChildren(parameters)
        .withChild(lambdaMethodBody)
        .withChild(Ast(returnNode))
        .withChildren(modifiers)

    val lambdaMethodAst = identifiersMatchingParams.foldLeft(lambdaMethodAstWithoutRefs)((ast, identifier) =>
      ast.withRefEdge(identifier, lambdaParameterNamesToNodes(identifier.name))
    )

    scope.addLambdaMethod(lambdaMethodAst)

    lambdaMethodNode
  }

  private def lambdaMethodSignature(returnType: Option[String], parameters: Seq[Ast]): String = {
    val maybeParameterTypes = toOptionList(parameters.map(_.rootType))
    val containsEmptyType   = maybeParameterTypes.exists(_.contains(ParameterDefaults.TypeFullName))

    (returnType, maybeParameterTypes) match {
      case (Some(returnTpe), Some(parameterTpes)) if !containsEmptyType =>
        composeMethodLikeSignature(returnTpe, parameterTpes)

      case _ => composeUnresolvedSignature(parameters.size)
    }
  }

  private def createLambdaMethodNode(
    lambdaName: String,
    parameters: Seq[Ast],
    returnType: Option[String]
  ): NewMethod = {
    val enclosingTypeName = scope.enclosingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace)
    val signature         = lambdaMethodSignature(returnType, parameters)
    val lambdaFullName    = composeMethodFullName(enclosingTypeName, lambdaName, signature)

    NewMethod()
      .name(lambdaName)
      .fullName(lambdaFullName)
      .signature(signature)
      .filename(filename)
      .code("<lambda>")
  }

  private def createAndPushLambdaTypeDecl(
    lambdaMethodNode: NewMethod,
    implementedInfo: LambdaImplementedInfo
  ): NewTypeDecl = {
    val inheritsFromTypeFullName =
      implementedInfo.implementedInterface
        .flatMap(typeInfoCalc.fullName)
        .orElse(Some(TypeConstants.Object))
        .toList

    typeInfoCalc.registerType(lambdaMethodNode.fullName)
    val lambdaTypeDeclNode =
      NewTypeDecl()
        .fullName(lambdaMethodNode.fullName)
        .name(lambdaMethodNode.name)
        .inheritsFromTypeFullName(inheritsFromTypeFullName)
    scope.addLocalDecl(Ast(lambdaTypeDeclNode))

    lambdaTypeDeclNode
  }

  private def getLambdaImplementedInfo(expr: LambdaExpr, expectedType: ExpectedType): LambdaImplementedInfo = {
    val maybeImplementedType = {
      val maybeResolved = tryWithSafeStackOverflow(expr.calculateResolvedType())
      maybeResolved.toOption
        .orElse(expectedType.resolvedType)
        .collect { case refType: ResolvedReferenceType => refType }
    }

    val maybeImplementedInterface = maybeImplementedType.flatMap(_.getTypeDeclaration.toScala)

    if (maybeImplementedInterface.isEmpty) {
      val location = s"$filename:${line(expr)}:${column(expr)}"
      logger.debug(
        s"Could not resolve the interface implemented by a lambda. Type info may be missing: $location. Type info may be missing."
      )
    }

    val maybeBoundMethod = maybeImplementedInterface.flatMap { interface =>
      interface.getDeclaredMethods.asScala
        .filter(_.isAbstract)
        .filterNot { method =>
          // Filter out java.lang.Object methods re-declared by the interface as these are also considered abstract.
          // See https://docs.oracle.com/javase/8/docs/api/java/lang/FunctionalInterface.html for details.
          Try(method.getSignature) match {
            case Success(signature) => ObjectMethodSignatures.contains(signature)
            case Failure(_) =>
              false // If the signature could not be calculated, it's probably not a standard object method.
          }
        }
        .headOption
    }

    LambdaImplementedInfo(maybeImplementedType, maybeBoundMethod)
  }

  private def addClosureBindingsToDiffGraph(
    bindingEntries: Iterable[ClosureBindingEntry],
    methodRef: NewMethodRef
  ): Unit = {
    bindingEntries.foreach { case ClosureBindingEntry(nodeTypeInfo, closureBinding) =>
      diffGraph.addNode(closureBinding)
      diffGraph.addEdge(closureBinding, nodeTypeInfo.node, EdgeTypes.REF)
      diffGraph.addEdge(methodRef, closureBinding, EdgeTypes.CAPTURE)
    }
  }

  // TODO: All of this will be thrown out, probably
  def astForLambdaExpr(expr: LambdaExpr, expectedType: ExpectedType): Ast = {
    scope.pushMethodScope(NewMethod(), expectedType)

    val lambdaMethodName = nextLambdaName()

    val closureBindingsForCapturedVars = closureBindingsForCapturedNodes(lambdaMethodName)
    val localsForCaptured              = localsForCapturedNodes(closureBindingsForCapturedVars)
    val implementedInfo                = getLambdaImplementedInfo(expr, expectedType)
    val lambdaMethodNode =
      createAndPushLambdaMethod(expr, lambdaMethodName, implementedInfo, localsForCaptured, expectedType)

    val methodRef =
      NewMethodRef()
        .methodFullName(lambdaMethodNode.fullName)
        .typeFullName(lambdaMethodNode.fullName)
        .code(lambdaMethodNode.fullName)

    addClosureBindingsToDiffGraph(closureBindingsForCapturedVars, methodRef)

    val interfaceBinding = implementedInfo.implementedMethod.map { implementedMethod =>
      newBindingNode(implementedMethod.getName, lambdaMethodNode.signature, lambdaMethodNode.fullName)
    }

    val bindingTable = getLambdaBindingTable(
      LambdaBindingInfo(lambdaMethodNode.fullName, implementedInfo.implementedInterface, interfaceBinding)
    )

    val lambdaTypeDeclNode = createAndPushLambdaTypeDecl(lambdaMethodNode, implementedInfo)
    BindingTable.createBindingNodes(diffGraph, lambdaTypeDeclNode, bindingTable)

    scope.popScope()
    Ast(methodRef)
  }

  private def getLambdaBindingTable(lambdaBindingInfo: LambdaBindingInfo): BindingTable = {
    val fullName = lambdaBindingInfo.fullName

    bindingTableCache.getOrElseUpdate(
      fullName,
      createBindingTable(
        fullName,
        lambdaBindingInfo,
        getBindingTable,
        new BindingTableAdapterForLambdas(methodSignature)
      )
    )
  }

  private def closureBindingsForCapturedNodes(lambdaMethodName: String): List[ClosureBindingEntry] = {
    scope.capturedVariables.map { capturedNode =>
      val closureBindingId = s"$filename:$lambdaMethodName:${capturedNode.name}"
      val closureBindingNode =
        newClosureBindingNode(closureBindingId, capturedNode.name, EvaluationStrategies.BY_SHARING)
      ClosureBindingEntry(capturedNode, closureBindingNode)
    }
  }

  private def localsForCapturedNodes(closureBindingEntries: List[ClosureBindingEntry]): List[NewLocal] = {
    val localsForCaptured =
      closureBindingEntries.map { case ClosureBindingEntry(node, binding) =>
        val local = NewLocal()
          .name(node.name)
          .code(node.name)
          .closureBindingId(binding.closureBindingId)
          .typeFullName(node.typeFullName)
        local
      }
    localsForCaptured.foreach { local => scope.addLocal(local) }
    localsForCaptured
  }

  private def astForLambdaBody(
    body: Statement,
    localsForCapturedVars: Seq[NewLocal],
    returnType: Option[String]
  ): Ast = {
    body match {
      case block: BlockStmt => astForBlockStatement(block, prefixAsts = localsForCapturedVars.map(Ast(_)))

      case stmt =>
        val blockAst = Ast(NewBlock().lineNumber(line(body)))
        val bodyAst = if (returnType.contains(TypeConstants.Void)) {
          astsForStatement(stmt)
        } else {
          val returnNode =
            NewReturn()
              .code(s"return ${body.toString}")
              .lineNumber(line(body))
          val returnArgs = astsForStatement(stmt)
          Seq(returnAst(returnNode, returnArgs))
        }

        blockAst
          .withChildren(localsForCapturedVars.map(Ast(_)))
          .withChildren(bodyAst)
    }
  }

  private def genericParamTypeMapForLambda(expectedType: ExpectedType): ResolvedTypeParametersMap = {
    expectedType.resolvedType
      // This should always be true for correct code
      .collect { case r: ResolvedReferenceType => r }
      .map(_.typeParametersMap())
      .getOrElse(new ResolvedTypeParametersMap.Builder().build())
  }

  private def buildParamListForLambda(
    expr: LambdaExpr,
    maybeBoundMethod: Option[ResolvedMethodDeclaration],
    expectedTypeParamTypes: ResolvedTypeParametersMap
  ): Seq[Ast] = {
    val lambdaParameters = expr.getParameters.asScala.toList
    val paramTypesList = maybeBoundMethod match {
      case Some(resolvedMethod) =>
        val resolvedParameters = (0 until resolvedMethod.getNumberOfParams).map(resolvedMethod.getParam)

        // Substitute generic typeParam with the expected type if it can be found; leave unchanged otherwise.
        resolvedParameters.map(param => Try(param.getType)).map {
          case Success(resolvedType: ResolvedTypeVariable) =>
            val typ = expectedTypeParamTypes.getValue(resolvedType.asTypeParameter)
            typeInfoCalc.fullName(typ)

          case Success(resolvedType) => typeInfoCalc.fullName(resolvedType)

          case Failure(_) => None
        }

      case None =>
        // Unless types are explicitly specified in the lambda definition,
        // this will yield the erased types which is why the actual lambda
        // expression parameters are only used as a fallback.
        lambdaParameters
          .map(_.getType)
          .map(typeInfoCalc.fullName)
    }

    if (paramTypesList.sizeIs != lambdaParameters.size) {
      logger.error(s"Found different number lambda params and param types for $expr. Some parameters will be missing.")
    }

    val parameterNodes = lambdaParameters
      .zip(paramTypesList)
      .zipWithIndex
      .map { case ((param, maybeType), idx) =>
        val name         = param.getNameAsString
        val typeFullName = maybeType.getOrElse(TypeConstants.Any)
        val code         = s"$typeFullName $name"
        val evalStrat =
          if (param.getType.isPrimitiveType) EvaluationStrategies.BY_VALUE else EvaluationStrategies.BY_SHARING
        val paramNode = NewMethodParameterIn()
          .name(name)
          .index(idx + 1)
          .order(idx + 1)
          .code(code)
          .evaluationStrategy(evalStrat)
          .typeFullName(typeFullName)
          .lineNumber(line(expr))
          .columnNumber(column(expr))
        typeInfoCalc.registerType(typeFullName)
        paramNode
      }

    parameterNodes.foreach { paramNode =>
      scope.addParameter(paramNode)
    }

    parameterNodes.map(Ast(_))
  }

  private def getLambdaReturnType(
    maybeResolvedLambdaType: Option[ResolvedType],
    maybeBoundMethod: Option[ResolvedMethodDeclaration],
    expectedTypeParamTypes: ResolvedTypeParametersMap
  ): Option[String] = {
    val maybeBoundMethodReturnType = maybeBoundMethod.flatMap { boundMethod =>
      Try(boundMethod.getReturnType).collect {
        case returnType: ResolvedTypeVariable => expectedTypeParamTypes.getValue(returnType.asTypeParameter)
        case other                            => other
      }.toOption
    }

    val returnType = maybeBoundMethodReturnType.orElse(maybeResolvedLambdaType)
    returnType.flatMap(typeInfoCalc.fullName)
  }
}
