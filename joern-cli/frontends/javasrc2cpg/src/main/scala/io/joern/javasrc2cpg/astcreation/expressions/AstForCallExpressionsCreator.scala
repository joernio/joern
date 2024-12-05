package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.body.VariableDeclarator
import com.github.javaparser.ast.expr.AssignExpr.Operator
import com.github.javaparser.ast.expr.{
  AssignExpr,
  Expression,
  FieldAccessExpr,
  LambdaExpr,
  MethodCallExpr,
  NameExpr,
  ObjectCreationExpr,
  SuperExpr,
  ThisExpr
}
import com.github.javaparser.ast.{Node, NodeList}
import com.github.javaparser.resolution.declarations.{ResolvedMethodDeclaration, ResolvedMethodLikeDeclaration}
import com.github.javaparser.resolution.types.parametrization.ResolvedTypeParametersMap
import io.joern.javasrc2cpg.astcreation.expressions.AstForCallExpressionsCreator.AllocAndInitCallAsts
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.scope.Scope.typeFullName
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.{NameConstants, Util}
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, composeMethodLikeSignature, composeUnresolvedSignature}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{
  AstNodeNew,
  ExpressionNew,
  NewBlock,
  NewCall,
  NewIdentifier,
  NewMember
}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Success, Try}
import javassist.compiler.ast.CallExpr
import io.joern.javasrc2cpg.scope.Scope.{NewVariableNode, ScopeInnerType, ScopeParameter, SimpleVariable}
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit
import org.slf4j.LoggerFactory

object AstForCallExpressionsCreator {
  private[expressions] final case class AllocAndInitCallAsts(allocAst: Ast, initAst: Ast)
}

trait AstForCallExpressionsCreator { this: AstCreator =>

  private val logger = LoggerFactory.getLogger(this.getClass)

  private[expressions] def astForMethodCall(call: MethodCallExpr, expectedReturnType: ExpectedType): Ast = {
    val maybeResolvedCall = tryWithSafeStackOverflow(call.resolve())
    val argumentAsts      = argAstsForCall(call, maybeResolvedCall, call.getArguments)

    val expressionTypeFullName =
      expressionReturnTypeFullName(call).orElse(getTypeFullName(expectedReturnType)).map(typeInfoCalc.registerType)

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedCall.toOption)
    val returnType = maybeResolvedCall
      .map { resolvedCall =>
        typeInfoCalc.fullName(resolvedCall.getReturnType, ResolvedTypeParametersMap.empty())
      }
      .toOption
      .flatten
      .orElse(expressionTypeFullName)

    val dispatchType = dispatchTypeForCall(maybeResolvedCall, call.getScope.toScala)

    val receiverTypeOption = targetTypeForCall(call)
    val scopeAsts = call.getScope.toScala match {
      case Some(scope) => astsForExpression(scope, ExpectedType(receiverTypeOption))

      case None =>
        Option
          .when(dispatchType == DispatchTypes.DYNAMIC_DISPATCH) {
            astForImplicitCallReceiver(receiverTypeOption, call)
          }
          .toList
    }

    val receiverType = scopeAsts.rootType.filter(_ != TypeConstants.Any).orElse(receiverTypeOption)

    val argumentsCode = getArgumentCodeString(call.getArguments)
    val codePrefix = scopeAsts.headOption
      .flatMap(_.root)
      .collect { case call: NewCall => s"${call.code}." }
      .getOrElse(codePrefixForMethodCall(call))
    val callCode = s"$codePrefix${call.getNameAsString}($argumentsCode)"

    val callName       = call.getNameAsString
    val namespace      = receiverType.filter(_ != TypeConstants.Any).getOrElse(Defines.UnresolvedNamespace)
    val signature      = composeSignature(returnType, argumentTypes, argumentAsts.size)
    val methodFullName = composeMethodFullName(namespace, callName, signature)
    val callRoot = NewCall()
      .name(callName)
      .methodFullName(methodFullName)
      .signature(signature)
      .code(callCode)
      .dispatchType(dispatchType)
      .lineNumber(line(call))
      .columnNumber(column(call))
      .typeFullName(expressionTypeFullName.getOrElse(defaultTypeFallback()))

    callAst(callRoot, argumentAsts, scopeAsts.headOption)
  }

  private def astForImplicitCallReceiver(declaringType: Option[String], call: MethodCallExpr): Ast = {
    val typeFullName = scope.lookupVariable(NameConstants.This).typeFullName.getOrElse(defaultTypeFallback())
    val thisIdentifier =
      identifierNode(call, NameConstants.This, NameConstants.This, typeFullName)
    scope.lookupVariable(NameConstants.This) match {
      case SimpleVariable(ScopeParameter(thisParam)) => diffGraph.addEdge(thisIdentifier, thisParam, EdgeTypes.REF)
      case _ => // Do nothing. This shouldn't happen for valid code, but could occur in cases where methods could not be resolved
    }
    val thisAst = Ast(thisIdentifier)

    scope.lookupMethodName(call.getNameAsString).drop(1) match {
      case Nil =>
        thisAst

      case typeDeclChain =>
        val lineNumber   = line(call)
        val columnNumber = column(call)
        typeDeclChain.foldLeft(thisAst) { case (accAst, typeDecl) =>
          val rootNode = newOperatorCallNode(
            Operators.fieldAccess,
            s"${accAst.rootCodeOrEmpty}.${NameConstants.OuterClass}",
            Some(typeDecl.fullName),
            lineNumber,
            columnNumber
          )

          val outerClassIdentifier = fieldIdentifierNode(call, NameConstants.OuterClass, NameConstants.OuterClass)
          callAst(rootNode, List(accAst, Ast(outerClassIdentifier)))
        }
    }
  }

  private[expressions] def blockAstForObjectCreationExpr(expr: ObjectCreationExpr, expectedType: ExpectedType): Ast = {
    val tmpName = tempNameProvider.next

    // Use an untyped identifier for receiver here, create the alloc and init ASTs,
    // then use the types of those to fix the local type.
    val assignTarget = identifierNode(expr, tmpName, tmpName, defaultTypeFallback())
    val allocAndInitAst =
      inlinedAstsForObjectCreationExpr(expr, Ast(assignTarget.copy), expectedType, resetAssignmentTargetType = true)

    assignTarget.typeFullName(allocAndInitAst.allocAst.rootType.getOrElse(defaultTypeFallback()))
    val tmpLocal = localNode(expr, tmpName, tmpName, assignTarget.typeFullName)

    val allocAssignCode = s"$tmpName = ${allocAndInitAst.allocAst.rootCodeOrEmpty}"
    val allocAssignCall =
      callNode(
        expr,
        allocAssignCode,
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        signature = None,
        typeFullName = Option(tmpLocal.typeFullName)
      )

    val allocAssignTargetNode = assignTarget
    val allocAssignTargetAst  = Ast(allocAssignTargetNode).withRefEdge(allocAssignTargetNode, tmpLocal)
    val allocAssignAst        = callAst(allocAssignCall, allocAssignTargetAst :: allocAndInitAst.allocAst :: Nil)

    val returnedIdentifier    = assignTarget.copy
    val returnedIdentifierAst = Ast(returnedIdentifier).withRefEdge(returnedIdentifier, tmpLocal)

    Ast(blockNode(expr).typeFullName(returnedIdentifier.typeFullName))
      .withChild(Ast(tmpLocal).withRefEdge(assignTarget, tmpLocal))
      .withChild(allocAssignAst)
      .withChild(allocAndInitAst.initAst)
      .withChild(returnedIdentifierAst)
  }

  private[expressions] def inlinedAstsForObjectCreationExpr(
    expr: ObjectCreationExpr,
    initReceiverAst: Ast,
    expectedType: ExpectedType,
    resetAssignmentTargetType: Boolean
  ): AllocAndInitCallAsts = {
    val maybeResolvedExpr = tryWithSafeStackOverflow(expr.resolve())
    val argumentAsts      = argAstsForCall(expr, maybeResolvedExpr, expr.getArguments)

    val anonymousClassBody = expr.getAnonymousClassBody.toScala.map(_.asScala.toList)
    val nameSuffix         = if (anonymousClassBody.isEmpty) "" else s"$$${scope.getNextAnonymousClassIndex()}"
    val rawType =
      tryWithSafeStackOverflow(expr.getTypeAsString)
        .map(Util.stripGenericTypes)
        .toOption
        .getOrElse(NameConstants.Unknown)
    val typeName = s"$rawType$nameSuffix"

    val baseTypeFromScope = scope.lookupScopeType(rawType)
    // These will be the same for non-anonymous type decls, but in that case only the typeFullName will be used.
    val baseTypeFullName =
      baseTypeFromScope
        .map(_.typeFullName)
        .orElse(tryWithSafeStackOverflow(expr.getType).toOption.map { typ =>
          typeInfoCalc
            .fullName(typ)
            .orElse(getTypeFullName(expectedType))
            .getOrElse(defaultTypeFallback(typ))
        })
    val typeFullName =
      if (anonymousClassBody.isEmpty)
        baseTypeFullName.map(typeFullName => s"$typeFullName$nameSuffix")
      else {
        scope.scopeFullName().map(enclosingScopeName => s"$enclosingScopeName.$typeName")
      }

    if (resetAssignmentTargetType) {
      typeFullName.foreach { typeFullName =>
        initReceiverAst.root.collect { case identifier: NewIdentifier => identifier.typeFullName(typeFullName) }
      }
    }

    val initReceiverType = initReceiverAst.rootType match {
      case Some(TypeConstants.Any)             => typeFullName
      case Some(PropertyDefaults.TypeFullName) => typeFullName
      case Some(typ)                           => Option(typ)
      case None                                => defaultTypeFallback()
    }

    anonymousClassBody.foreach { bodyStmts =>
      val anonymousClassDecl = astForAnonymousClassDecl(expr, bodyStmts, typeName, typeFullName, baseTypeFullName)
      scope.addLocalDecl(anonymousClassDecl)
    }

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedExpr.toOption)

    val allocNode = newOperatorCallNode(
      Operators.alloc,
      expr.toString,
      typeFullName.orElse(Some(defaultTypeFallback())),
      line(expr),
      column(expr)
    )

    val initCall = initNode(
      typeFullName.orElse(Some(defaultTypeFallback())),
      argumentTypes,
      argumentAsts.size,
      expr.toString,
      line(expr)
    )

    val isInnerType = anonymousClassBody.isDefined || baseTypeFromScope.exists(
      _.isInstanceOf[ScopeInnerType]
    ) || expr.getScope.isPresent

    val capturedOuterClassAst =
      expr.getScope.toScala.flatMap(astsForExpression(_, ExpectedType.empty).headOption).orElse {
        scope.lookupVariable(NameConstants.This) match {
          case SimpleVariable(param: ScopeParameter) if !scope.isEnclosingScopeStatic =>
            val outerClassIdentifier = identifierNode(expr, param.name, param.name, param.typeFullName)
            Some(Ast(outerClassIdentifier).withRefEdge(outerClassIdentifier, param.node))
          case _ => None
        }
      }

    val initAst = if (isInnerType) {
      val initCallAst = Ast(initCall)
      scope.enclosingTypeDecl.foreach(
        _.registerInitToComplete(
          PartialInit(allocNode.typeFullName, initCallAst, initReceiverAst, argumentAsts.toList, capturedOuterClassAst)
        )
      )
      initCallAst
    } else {
      callAst(initCall, argumentAsts, Option(initReceiverAst))
    }

    AllocAndInitCallAsts(callAst(allocNode, Nil), initAst)
  }

  def argAstsForCall(
    call: Node,
    tryResolvedDecl: Try[ResolvedMethodLikeDeclaration],
    args: NodeList[Expression]
  ): Seq[Ast] = {
    val hasVariadicParameter = tryResolvedDecl.map(_.hasVariadicParameter).getOrElse(false)
    val paramCount           = tryResolvedDecl.map(_.getNumberOfParams).getOrElse(-1)

    val argsAsts = args.asScala.zipWithIndex.flatMap { case (arg, idx) =>
      val expectedType = getExpectedParamType(tryResolvedDecl, idx)
      astsForExpression(arg, expectedType)
    }.toList

    tryResolvedDecl match {
      case Success(_) if hasVariadicParameter =>
        val expectedVariadicTypeFullName = getTypeFullName(getExpectedParamType(tryResolvedDecl, paramCount - 1))
        val (regularArgs, varargs)       = argsAsts.splitAt(paramCount - 1)
        val arrayInitializer = newOperatorCallNode(
          Operators.arrayInitializer,
          Operators.arrayInitializer,
          expectedVariadicTypeFullName,
          line(call),
          column(call)
        )

        val arrayInitializerAst = callAst(arrayInitializer, varargs)

        regularArgs ++ Seq(arrayInitializerAst)

      case _ => argsAsts
    }
  }

  private def getExpectedParamType(maybeResolvedCall: Try[ResolvedMethodLikeDeclaration], idx: Int): ExpectedType = {
    maybeResolvedCall.toOption
      .map { methodDecl =>
        val paramCount = methodDecl.getNumberOfParams

        val resolvedType = if (idx < paramCount) {
          tryWithSafeStackOverflow(methodDecl.getParam(idx).getType).toOption
        } else if (paramCount > 0 && methodDecl.getParam(paramCount - 1).isVariadic) {
          tryWithSafeStackOverflow(methodDecl.getParam(paramCount - 1).getType).toOption
        } else {
          None
        }

        val typeName = resolvedType.flatMap(typeInfoCalc.fullName)
        ExpectedType(typeName, resolvedType)
      }
      .getOrElse(ExpectedType.empty)
  }

  def initNode(
    namespaceName: Option[String],
    argumentTypes: Option[List[String]],
    argsSize: Int,
    code: String,
    lineNumber: Option[Int] = None,
    columnNumber: Option[Int] = None
  ): NewCall = {
    val initSignature = argumentTypes match {
      case Some(tpe)          => composeMethodLikeSignature(TypeConstants.Void, tpe)
      case _ if argsSize == 0 => composeMethodLikeSignature(TypeConstants.Void, Nil)
      case _                  => composeUnresolvedSignature(argsSize)
    }
    val namespace          = namespaceName.getOrElse(Defines.UnresolvedNamespace)
    val initMethodFullName = composeMethodFullName(namespace, Defines.ConstructorMethodName, initSignature)
    NewCall()
      .name(Defines.ConstructorMethodName)
      .methodFullName(initMethodFullName)
      .signature(initSignature)
      .typeFullName(TypeConstants.Void)
      .code(code)
      .dispatchType(DispatchTypes.STATIC_DISPATCH)
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
  }

  private def getArgumentCodeString(args: NodeList[Expression]): String = {
    args.asScala
      .map {
        case _: LambdaExpr => "<lambda>"
        case other         => code(other)
      }
      .mkString(", ")
  }

  private def dispatchTypeForCall(maybeDecl: Try[ResolvedMethodDeclaration], maybeScope: Option[Expression]): String = {
    maybeScope match {
      case Some(_: SuperExpr) =>
        DispatchTypes.STATIC_DISPATCH
      case _ =>
        maybeDecl match {
          case Success(decl) =>
            if (decl.isStatic) DispatchTypes.STATIC_DISPATCH else DispatchTypes.DYNAMIC_DISPATCH

          case _ =>
            DispatchTypes.DYNAMIC_DISPATCH
        }
    }
  }

  private def targetTypeForCall(callExpr: MethodCallExpr): Option[String] = {
    val maybeType = callExpr.getScope.toScala match {
      case Some(callScope: ThisExpr) =>
        // TODO Can't we just use the enclosing decl? Would need to pay attention to `this` in lambda
        expressionReturnTypeFullName(callScope)
          .orElse(scope.enclosingTypeDecl.fullName)

      case Some(callScope: SuperExpr) =>
        expressionReturnTypeFullName(callScope)
          .orElse(scope.enclosingTypeDecl.flatMap(_.typeDecl.inheritsFromTypeFullName.headOption))

      case Some(scope) => expressionReturnTypeFullName(scope)

      case None =>
        tryWithSafeStackOverflow(callExpr.resolve()).toOption
          .flatMap { methodDeclOption =>
            typeInfoCalc.fullNameWithoutRegistering(methodDeclOption.declaringType())
          }
          // TODO: Check for the method name in scope
          .orElse(scope.enclosingTypeDecl.fullName)
    }

    maybeType.map(typeInfoCalc.registerType)
  }

  private def createObjectNode(
    typeFullName: Option[String],
    call: MethodCallExpr,
    dispatchType: String
  ): Option[NewIdentifier] = {
    val maybeScope = call.getScope.toScala

    Option.when(maybeScope.isDefined || dispatchType == DispatchTypes.DYNAMIC_DISPATCH) {
      val name = maybeScope.map(_.toString).getOrElse(NameConstants.This)
      identifierNode(call, name, name, typeFullName.getOrElse("ANY"))
    }
  }

  private def codePrefixForMethodCall(call: MethodCallExpr): String = {

    tryWithSafeStackOverflow(call.resolve()) match {
      case Success(resolvedCall) =>
        call.getScope.toScala
          .flatMap(codeForScopeExpr(_, resolvedCall.isStatic))
          .getOrElse(if (resolvedCall.isStatic) "" else s"${NameConstants.This}.")

      case _ =>
        // If the call is unresolvable, we cannot make a good guess about what the prefix should be
        ""
    }
  }

  private def codeForScopeExpr(scopeExpr: Expression, isScopeForStaticCall: Boolean): Option[String] = {
    scopeExpr match {
      case scope: NameExpr => someWithDotSuffix(scope.getNameAsString)

      case fieldAccess: FieldAccessExpr =>
        val maybeScopeString = codeForScopeExpr(fieldAccess.getScope, isScopeForStaticCall = false)
        val name             = fieldAccess.getNameAsString
        maybeScopeString
          .map { scopeString =>
            s"$scopeString$name"
          }
          .orElse(Some(name))
          .flatMap(someWithDotSuffix)

      case _: SuperExpr => someWithDotSuffix(NameConstants.Super)

      case _: ThisExpr => someWithDotSuffix(NameConstants.This)

      case scopeMethodCall: MethodCallExpr =>
        codePrefixForMethodCall(scopeMethodCall) match {
          case "" => Some("")
          case prefix =>
            val argumentsCode = getArgumentCodeString(scopeMethodCall.getArguments)
            someWithDotSuffix(s"$prefix${scopeMethodCall.getNameAsString}($argumentsCode)")
        }

      case objectCreationExpr: ObjectCreationExpr =>
        // Use type name with generics for code
        val typeName = tryWithSafeStackOverflow(objectCreationExpr.getTypeAsString).getOrElse(NameConstants.Unknown)
        val argumentsString = getArgumentCodeString(objectCreationExpr.getArguments)
        someWithDotSuffix(s"new $typeName($argumentsString)")

      case _ => None
    }
  }

  private def someWithDotSuffix(prefix: String): Option[String] = Some(s"$prefix.")
}
