package io.joern.javasrc2cpg.astcreation.expressions

import com.github.javaparser.ast.body.VariableDeclarator
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
import io.joern.javasrc2cpg.astcreation.expressions.AstForCallExpressionsCreator.PartialConstructor
import io.joern.javasrc2cpg.astcreation.{AstCreator, ExpectedType}
import io.joern.javasrc2cpg.typesolvers.TypeInfoCalculator.TypeConstants
import io.joern.javasrc2cpg.util.NameConstants
import io.joern.javasrc2cpg.util.Util.{composeMethodFullName, composeMethodLikeSignature, composeUnresolvedSignature}
import io.joern.x2cpg.utils.AstPropertiesUtil.*
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines}
import io.shiftleft.codepropertygraph.generated.nodes.Call.PropertyDefaults
import io.shiftleft.codepropertygraph.generated.nodes.{ExpressionNew, NewBlock, NewCall, NewIdentifier}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}

import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Success, Try}
import javassist.compiler.ast.CallExpr
import io.joern.javasrc2cpg.scope.Scope.ScopeInnerType
import io.joern.javasrc2cpg.scope.Scope.SimpleVariable
import io.joern.javasrc2cpg.scope.Scope.ScopeParameter
import io.joern.javasrc2cpg.scope.JavaScopeElement.PartialInit

object AstForCallExpressionsCreator {
  case class PartialConstructor(typeFullName: Option[String], initNode: NewCall, initArgs: Seq[Ast], blockAst: Ast)
}

trait AstForCallExpressionsCreator { this: AstCreator =>

  private var tempConstCount = 0

  private[expressions] def astForMethodCall(call: MethodCallExpr, expectedReturnType: ExpectedType): Ast = {
    val maybeResolvedCall = tryWithSafeStackOverflow(call.resolve())
    val argumentAsts      = argAstsForCall(call, maybeResolvedCall, call.getArguments)

    val expressionTypeFullName =
      expressionReturnTypeFullName(call).orElse(expectedReturnType.fullName).map(typeInfoCalc.registerType)

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedCall)
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
      .typeFullName(expressionTypeFullName.getOrElse(TypeConstants.Any))

    callAst(callRoot, argumentAsts, scopeAsts.headOption)
  }

  private def astForImplicitCallReceiver(declaringType: Option[String], call: MethodCallExpr): Ast = {
    val typeFullName = scope.lookupVariable(NameConstants.This).typeFullName.getOrElse(TypeConstants.Any)
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

  /** The below representation for constructor invocations and object creations was chosen for the sake of consistency
    * with the Java frontend. It follows the bytecode approach of splitting a constructor call into separate `alloc` and
    * `init` calls.
    *
    * There are two cases to consider. The first is a constructor invocation in an assignment, for example:
    *
    * Foo f = new Foo(42);
    *
    * is represented as
    *
    * Foo f = <operator>.alloc() f.init(42);
    *
    * The second case is a constructor invocation not in an assignment, for example as an argument to a method call. In
    * this case, the representation does not stay as close to Java as in case
    *   1. In particular, a new BLOCK is introduced to contain the constructor invocation. For example:
    *
    * foo(new Foo(42));
    *
    * is represented as
    *
    * foo({ Foo temp = alloc(); temp.init(42); temp })
    *
    * This is not valid Java code, but this representation is a decent compromise between staying faithful to Java and
    * being consistent with the Java bytecode frontend.
    */
  private[expressions] def astForObjectCreationExpr(expr: ObjectCreationExpr, expectedType: ExpectedType): Ast = {
    val maybeResolvedExpr = tryWithSafeStackOverflow(expr.resolve())
    val argumentAsts      = argAstsForCall(expr, maybeResolvedExpr, expr.getArguments)

    val anonymousClassBody = expr.getAnonymousClassBody.toScala.map(_.asScala.toList)
    val nameSuffix         = if (anonymousClassBody.isEmpty) "" else s"$$${scope.getNextAnonymousClassIndex()}"
    val typeName           = s"${expr.getTypeAsString}$nameSuffix"

    val baseTypeFromScope = scope.lookupScopeType(expr.getTypeAsString)
    // These will be the same for non-anonymous type decls, but in that case only the typeFullName will be used.
    val baseTypeFullName =
      baseTypeFromScope
        .map(_.typeFullName)
        .orElse(
          tryWithSafeStackOverflow(typeInfoCalc.fullName(expr.getType)).toOption.flatten.orElse(expectedType.fullName)
        )
    val typeFullName =
      if (anonymousClassBody.isEmpty)
        baseTypeFullName.map(typeFullName => s"$typeFullName$nameSuffix")
      else {
        scope.scopeFullName().map(enclosingScopeName => s"$enclosingScopeName.$typeName")
      }

    anonymousClassBody.foreach { bodyStmts =>
      val anonymousClassDecl = astForAnonymousClassDecl(expr, bodyStmts, typeName, typeFullName, baseTypeFullName)
      scope.addLocalDecl(anonymousClassDecl)
    }

    val argumentTypes = argumentTypesForMethodLike(maybeResolvedExpr)

    val allocNode = newOperatorCallNode(
      Operators.alloc,
      expr.toString,
      typeFullName.orElse(Some(TypeConstants.Any)),
      line(expr),
      column(expr)
    )

    val initCall = initNode(
      typeFullName.orElse(Some(TypeConstants.Any)),
      argumentTypes,
      argumentAsts.size,
      expr.toString,
      line(expr)
    )

    val isInnerType = anonymousClassBody.isDefined || baseTypeFromScope.exists(_.isInstanceOf[ScopeInnerType])

    // Assume that a block ast is required, since there isn't enough information to decide otherwise.
    // This simplifies logic elsewhere, and unnecessary blocks will be garbage collected soon.
    val blockAst =
      blockAstForConstructorInvocation(line(expr), column(expr), allocNode, initCall, argumentAsts, isInnerType)

    expr.getParentNode.toScala match {
      case Some(parent) if parent.isInstanceOf[VariableDeclarator] || parent.isInstanceOf[AssignExpr] =>
        val partialConstructor = PartialConstructor(typeFullName, initCall, argumentAsts, blockAst)
        partialConstructorQueue.append(partialConstructor)
        Ast(allocNode)

      case _ =>
        blockAst
    }
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
        val expectedVariadicTypeFullName = getExpectedParamType(tryResolvedDecl, paramCount - 1).fullName
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
          Some(methodDecl.getParam(idx).getType)
        } else if (paramCount > 0 && methodDecl.getParam(paramCount - 1).isVariadic) {
          Some(methodDecl.getParam(paramCount - 1).getType)
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
    lineNumber: Option[Integer] = None,
    columnNumber: Option[Integer] = None
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

  private def blockAstForConstructorInvocation(
    lineNumber: Option[Integer],
    columnNumber: Option[Integer],
    allocNode: NewCall,
    initNode: NewCall,
    args: Seq[Ast],
    isInnerType: Boolean
  ): Ast = {
    val blockNode = NewBlock()
      .lineNumber(lineNumber)
      .columnNumber(columnNumber)
      .typeFullName(allocNode.typeFullName)

    val tempName = "$obj" ++ tempConstCount.toString
    tempConstCount += 1
    val identifier    = newIdentifierNode(tempName, allocNode.typeFullName)
    val identifierAst = Ast(identifier)

    val allocAst = Ast(allocNode)

    val assignmentNode = newOperatorCallNode(Operators.assignment, PropertyDefaults.Code, Some(allocNode.typeFullName))

    val assignmentAst = callAst(assignmentNode, List(identifierAst, allocAst))

    val identifierWithDefaultOrder = identifier.copy.order(PropertyDefaults.Order)
    val identifierForInit          = identifierWithDefaultOrder.copy
    val initCopyWithDefaultOrder   = initNode.copy.order(PropertyDefaults.Order)

    val returnAst = Ast(identifierWithDefaultOrder.copy)

    val capturedThis = scope.lookupVariable(NameConstants.This) match {
      case SimpleVariable(param: ScopeParameter) => Some(param.node)
      case _                                     => None
    }

    val initAst = if (isInnerType) {
      val initCallAst = Ast(initCopyWithDefaultOrder)
      scope.enclosingTypeDecl.foreach(
        _.registerInitToComplete(
          PartialInit(allocNode.typeFullName, initCallAst, Ast(identifierForInit), args.toList, capturedThis)
        )
      )
      initCallAst
    } else {
      callAst(initCopyWithDefaultOrder, args, Some(Ast(identifierForInit)))
    }

    Ast(blockNode)
      .withChild(assignmentAst)
      .withChild(initAst)
      .withChild(returnAst)
  }

  private def getArgumentCodeString(args: NodeList[Expression]): String = {
    args.asScala
      .map {
        case _: LambdaExpr => "<lambda>"
        case other         => other.toString
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
        val typeName        = objectCreationExpr.getTypeAsString
        val argumentsString = getArgumentCodeString(objectCreationExpr.getArguments)
        someWithDotSuffix(s"new $typeName($argumentsString)")

      case _ => None
    }
  }

  private def someWithDotSuffix(prefix: String): Option[String] = Some(s"$prefix.")
}
