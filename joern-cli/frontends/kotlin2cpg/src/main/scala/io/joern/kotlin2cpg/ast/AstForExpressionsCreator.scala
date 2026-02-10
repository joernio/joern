package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.CallKind
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.Properties
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.descriptors.ClassDescriptor
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.*

import scala.jdk.CollectionConverters.*
import io.joern.x2cpg.AstNodeBuilder.bindingNode
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import io.joern.kotlin2cpg.ast.AstCreator.BindingInfo
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import org.jetbrains.kotlin.resolve.scopes.DescriptorKindFilter
import org.jetbrains.kotlin.types.KotlinType

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) {
  this: AstCreator =>

  def astsForBinaryExpr(
    expr: KtBinaryExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Seq[Ast] = {
    val opRef = expr.getOperationReference

    // TODO: add the rest of the operators
    val operatorOption = opRef.getOperationSignTokenType match {
      case KtTokens.ANDAND     => Some(Operators.logicalAnd)
      case KtTokens.DIVEQ      => Some(Operators.assignmentDivision)
      case KtTokens.DIV        => Some(Operators.division)
      case KtTokens.ELVIS      => Some(Operators.elvis)
      case KtTokens.EQEQEQ     => Some(Operators.equals)
      case KtTokens.EQEQ       => Some(Operators.equals)
      case KtTokens.EQ         => Some(Operators.assignment)
      case KtTokens.EXCLEQEQEQ => Some(Operators.notEquals)
      case KtTokens.EXCLEQ     => Some(Operators.notEquals)
      case KtTokens.GTEQ       => Some(Operators.greaterEqualsThan)
      case KtTokens.GT         => Some(Operators.greaterThan)
      case KtTokens.IN_KEYWORD => Some(Operators.in)
      case KtTokens.LTEQ       => Some(Operators.lessEqualsThan)
      case KtTokens.LT         => Some(Operators.lessThan)
      case KtTokens.MINUSEQ    => Some(Operators.assignmentMinus)
      case KtTokens.MINUS      => Some(Operators.subtraction)
      case KtTokens.MUL        => Some(Operators.multiplication)
      case KtTokens.MULTEQ     => Some(Operators.assignmentMultiplication)
      case KtTokens.NOT_IN     => Some(Operators.notIn)
      case KtTokens.OROR       => Some(Operators.logicalOr)
      case KtTokens.PERCEQ     => Some(Operators.assignmentModulo)
      case KtTokens.PERC       => Some(Operators.modulo)
      case KtTokens.PLUSEQ     => Some(Operators.assignmentPlus)
      case KtTokens.PLUS       => Some(Operators.addition)
      case KtTokens.RANGE      => Some(Operators.range)
      case null =>
        val opElement = expr.getOperationReference.getReferencedNameElement
        opElement.getText match {
          case "and"  => Some(Operators.and)
          case "or"   => Some(Operators.or)
          case "shl"  => Some(Operators.shiftLeft)
          case "shr"  => Some(Operators.arithmeticShiftRight)
          case "ushl" => Some(Operators.shiftLeft)
          case "ushr" => Some(Operators.logicalShiftRight)
          case "xor"  => Some(Operators.xor)
          case _      => None
        }
      case _ =>
        logger.warn(
          s"Unhandled operator token type `${opRef.getOperationSignTokenType}` for expression `${expr.getText}` in this file `$relativizedPath`."
        )
        Some(Constants.UnknownOperator)
    }
    val (fullName, signature) =
      if (operatorOption.isDefined) (operatorOption.get, TypeConstants.Any)
      // TODO: fix the fallback METHOD_FULL_NAME and SIGNATURE here (should be a correct number of ANYs)
      else {
        val funcDesc = bindingUtils.getCalledFunctionDesc(expr.getOperationReference)
        val descFullName = funcDesc
          .orElse(getAmbiguousFuncDescIfFullNamesEqual(expr.getOperationReference))
          .flatMap(nameRenderer.descFullName)
          .getOrElse(TypeConstants.Any)
        val signature = funcDesc
          .orElse(getAmbiguousFuncDescIfSignaturesEqual(expr.getOperationReference))
          .flatMap(nameRenderer.funcDescSignature)
          .getOrElse(TypeConstants.Any)
        val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)
        (fullName, signature)
      }

    val finalSignature =
      // TODO: add test case for this situation
      if (fullName.startsWith(Constants.OperatorSuffix)) Constants.Empty
      else signature

    val typeFullName = registerType(
      bindingUtils
        .getCalledFunctionDesc(expr.getOperationReference)
        .orElse(getAmbiguousFuncDescIfSignaturesEqual(expr.getOperationReference))
        .flatMap(funcDesc => nameRenderer.typeFullName(funcDesc.getOriginal.getReturnType))
        .getOrElse(TypeConstants.Any)
    )
    val name =
      if (operatorOption.isDefined) operatorOption.get
      else if (expr.getChildren.toList.sizeIs >= 2) expr.getChildren.toList(1).getText
      else expr.getName
    val node = callNode(
      expr,
      expr.getText,
      name,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(finalSignature),
      Some(typeFullName)
    )
    val lhsArgs = astsForExpression(expr.getLeft, None)
    val rhsArgs = astsForExpression(expr.getRight, None)
    lhsArgs.dropRight(1) ++ rhsArgs.dropRight(1) ++ Seq(
      callAst(
        withArgumentIndex(node, argIdx).argumentName(argNameMaybe),
        List(
          lhsArgs.lastOption.getOrElse(Ast(unknownNode(expr.getLeft, Constants.Empty))),
          rhsArgs.lastOption.getOrElse(Ast(unknownNode(expr.getRight, Constants.Empty)))
        )
      )
        .withChildren(annotations.map(astForAnnotationEntry))
    )
  }

  private def astForQualifiedExpressionFieldAccess(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    val exprNode = astsForExpression(expr.getReceiverExpression, Some(1)).headOption
      .getOrElse(Ast(unknownNode(expr.getReceiverExpression, Constants.Empty)))

    val nameReferenceExpr = expr.getSelectorExpression.asInstanceOf[KtNameReferenceExpression]
    val fieldIdentifier = Ast(
      fieldIdentifierNode(nameReferenceExpr, nameReferenceExpr.getText, nameReferenceExpr.getText).argumentIndex(2)
    )

    val retType = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val node = withArgumentIndex(operatorCallNode(expr, expr.getText, Operators.fieldAccess, Option(retType)), argIdx)
      .argumentName(argNameMaybe)
    callAst(node, List(exprNode, fieldIdentifier))
  }

  private def astForQualifiedExpressionExtensionCall(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    val argAsts = selectorExpressionArgAsts(expr, 2)

    // TODO fix the cast to KtCallExpression
    val (fullName, signature) = calleeFullnameAndSignature(
      getCalleeExpr(expr),
      astDerivedFullNameWithSignature(expr, argAsts)._1,
      astDerivedFullNameWithSignature(expr, argAsts)._2
    )

    val retType    = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val methodName = expr.getSelectorExpression.getFirstChild.getText
    val node =
      withArgumentIndex(
        callNode(
          expr,
          expr.getText,
          methodName,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(retType)
        ),
        argIdx
      ).argumentName(argNameMaybe)

    val instanceArg = astsForExpression(expr.getReceiverExpression, Some(1)).headOption
      .getOrElse(Ast(unknownNode(expr.getReceiverExpression, Constants.Empty)))
    callAst(node, instanceArg +: argAsts)
  }

  private def astForQualifiedExpressionCallToSuper(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    val receiverAst = astsForExpression(expr.getReceiverExpression, Some(0)).headOption
      .getOrElse(Ast(unknownNode(expr.getReceiverExpression, Constants.Empty)))
    val argAsts = selectorExpressionArgAsts(expr)

    val (fullName, signature) = calleeFullnameAndSignature(
      getCalleeExpr(expr),
      astDerivedFullNameWithSignature(expr, argAsts)._1,
      astDerivedFullNameWithSignature(expr, argAsts)._2
    )

    val retType    = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val methodName = expr.getSelectorExpression.getFirstChild.getText
    val node =
      withArgumentIndex(
        callNode(
          expr,
          expr.getText,
          methodName,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(retType)
        ),
        argIdx
      ).argumentName(argNameMaybe)
    callAst(node, argAsts, Option(receiverAst))
  }

  private def astForQualifiedExpressionCtor(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    expr.getSelectorExpression match {
      case callExpr: KtCallExpression =>
        val localName         = "tmp"
        val localTypeFullName = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
        val local             = localNode(expr, localName, localName, localTypeFullName)
        scope.addToScope(localName, local)
        val localAst = Ast(local)

        val typeFullName = registerType(exprTypeFullName(expr).getOrElse(Defines.UnresolvedNamespace))
        val rhsAst       = Ast(operatorCallNode(expr, Operators.alloc, Operators.alloc, Option(typeFullName)))

        val identifier    = identifierNode(expr, localName, localName, local.typeFullName)
        val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

        val assignmentNode =
          operatorCallNode(expr, s"${identifier.name} = ${Operators.alloc}", Operators.assignment, None)
        val assignmentCallAst = callAst(assignmentNode, List(identifierAst) ++ List(rhsAst))

        val (fullName, signature) =
          calleeFullnameAndSignature(
            getCalleeExpr(expr),
            Defines.UnresolvedNamespace,
            s"${Defines.UnresolvedSignature}(${callExpr.getValueArguments.size()})"
          )
        val initCallNode = callNode(
          callExpr,
          callExpr.getText,
          Defines.ConstructorMethodName,
          fullName,
          DispatchTypes.STATIC_DISPATCH,
          Some(signature),
          Some(TypeConstants.Void)
        )
        val initReceiverNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
        val initReceiverAst  = Ast(initReceiverNode).withRefEdge(initReceiverNode, local)

        val argAsts = astsForKtCallExpressionArguments(callExpr)
        val initAst = callAst(initCallNode, argAsts, Option(initReceiverAst))

        val returningIdentifierNode = identifierNode(expr, identifier.name, identifier.name, identifier.typeFullName)
        val returningIdentifierAst  = Ast(returningIdentifierNode).withRefEdge(returningIdentifierNode, local)

        val node = blockNode(expr, expr.getText, localTypeFullName).argumentName(argNameMaybe)
        argIdx match {
          case Some(idx) => node.argumentIndex(idx)
          case _         =>
        }
        blockAst(node, List(localAst, assignmentCallAst, initAst, returningIdentifierAst))
      case _ =>
        val node = blockNode(expr, "", TypeConstants.Any).argumentName(argNameMaybe)
        argIdx match {
          case Some(idx) => node.argumentIndex(idx)
          case _         =>
        }
        blockAst(node, List())
    }
  }

  private def astForQualifiedExpressionWithNoAstForReceiver(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    val receiverAst = astsForExpression(expr.getReceiverExpression, Some(1)).headOption
      .getOrElse(Ast(unknownNode(expr.getReceiverExpression, Constants.Empty)))
    val argAsts = selectorExpressionArgAsts(expr)

    val (fullName, signature) = calleeFullnameAndSignature(
      getCalleeExpr(expr),
      astDerivedFullNameWithSignature(expr, argAsts)._1,
      astDerivedFullNameWithSignature(expr, argAsts)._2
    )
    val retType      = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val methodName   = expr.getSelectorExpression.getFirstChild.getText
    val dispatchType = DispatchTypes.STATIC_DISPATCH

    val node = withArgumentIndex(
      callNode(expr, expr.getText, methodName, fullName, dispatchType, Some(signature), Some(retType)),
      argIdx
    ).argumentName(argNameMaybe)
    Ast(node)
      .withChild(receiverAst)
      .withChildren(argAsts)
      .withArgEdges(node, argAsts.map(_.root.get))
  }

  private def astForQualifiedExpressionWithReceiverEdge(
    expr: KtQualifiedExpression,
    callKind: CallKind,
    argIdx: Option[Int],
    argNameMaybe: Option[String]
  ): Ast = {
    val isDynamicCall = callKind == CallKind.DynamicCall
    val isStaticCall  = callKind == CallKind.StaticCall
    val argIdxForReceiver =
      if (isDynamicCall) 0
      else if (isStaticCall) 1
      else 1
    val dispatchType =
      if (callKind == CallKind.DynamicCall) DispatchTypes.DYNAMIC_DISPATCH
      else DispatchTypes.STATIC_DISPATCH

    val receiverAst = astsForExpression(expr.getReceiverExpression, Some(argIdxForReceiver)).headOption
      .getOrElse(Ast(unknownNode(expr.getReceiverExpression, Constants.Empty)))
    val argAsts = selectorExpressionArgAsts(expr)

    val (fullName, signature) = calleeFullnameAndSignature(
      getCalleeExpr(expr),
      astDerivedFullNameWithSignature(expr, argAsts)._1,
      astDerivedFullNameWithSignature(expr, argAsts)._2
    )
    val retType    = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val methodName = expr.getSelectorExpression.getFirstChild.getText

    val node =
      withArgumentIndex(
        if (fullName.startsWith("<operator>.")) {
          operatorCallNode(expr, expr.getText, fullName, Option(retType))
        } else {
          callNode(expr, expr.getText, methodName, fullName, dispatchType, Some(signature), Some(retType))
        },
        argIdx
      ).argumentName(argNameMaybe)
    val receiverNode =
      if (argAsts.sizeIs == 1 && argAsts.head.root.get.isInstanceOf[NewMethodRef]) argAsts.head.root.get
      else receiverAst.root.get

    Ast(node)
      .withChild(receiverAst)
      .withArgEdge(node, receiverNode)
      .withChildren(argAsts)
      .withArgEdges(node, argAsts.map(_.root.get))
      .withReceiverEdge(node, receiverNode)
  }

  // TODO: clean up this whole fn
  def astForQualifiedExpression(
    expr: KtQualifiedExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val callKind        = typeInfoProvider.bindingKind(expr)
    val isExtensionCall = callKind == CallKind.ExtensionCall

    val hasNameRefSelector = expr.getSelectorExpression.isInstanceOf[KtNameReferenceExpression]
    val isCallToSuper = expr.getReceiverExpression match {
      case _: KtSuperExpression => true
      case _                    => false
    }
    val isStaticMethodCall = typeInfoProvider.isStaticMethodCall(expr)
    val hasRefToClassReceiver = expr.getReceiverExpression match {
      case r: KtNameReferenceExpression =>
        typeInfoProvider.isReferenceToClass(r)
      case _ =>
        false
    }
    val isCtorCtorCall   = typeInfoProvider.isConstructorCall(expr)
    val noAstForReceiver = isStaticMethodCall && hasRefToClassReceiver

    val outAst =
      if (isCtorCtorCall.getOrElse(false)) {
        astForQualifiedExpressionCtor(expr, argIdx, argNameMaybe)
      } else if (isExtensionCall) {
        astForQualifiedExpressionExtensionCall(expr, argIdx, argNameMaybe)
      } else if (hasNameRefSelector) {
        astForQualifiedExpressionFieldAccess(expr, argIdx, argNameMaybe)
      } else if (isCallToSuper) {
        astForQualifiedExpressionCallToSuper(expr, argIdx, argNameMaybe)
      } else if (noAstForReceiver) {
        astForQualifiedExpressionWithNoAstForReceiver(expr, argIdx, argNameMaybe)
      } else {
        astForQualifiedExpressionWithReceiverEdge(expr, callKind, argIdx, argNameMaybe)
      }
    outAst.withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForIsExpression(
    expr: KtIsExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val args = astsForExpression(expr.getLeftHandSide, None) ++
      Seq(astForTypeReference(expr.getTypeReference, None, argName))
    val node = operatorCallNode(expr, expr.getText, Operators.is, None)
    callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args.toList)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForBinaryExprWithTypeRHS(
    expr: KtBinaryExpressionWithTypeRHS,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val args = astsForExpression(expr.getLeft, None) ++ Seq(astForTypeReference(expr.getRight, None, None))
    val node = operatorCallNode(expr, expr.getText, Operators.cast, None)
    callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args.toList)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astsForCall(
    expr: KtCallExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Seq[Ast] = {
    val isCtorCall = typeInfoProvider.isConstructorCall(expr)
    if (isCtorCall.getOrElse(false)) astsForCtorCall(expr, argIdx, argNameMaybe, annotations)
    else astsForNonCtorCall(expr, argIdx, argNameMaybe, annotations)
  }

  private def astsForNonCtorCall(
    expr: KtCallExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Seq[Ast] = {
    val argAsts = astsForKtCallExpressionArguments(expr)

    // TODO: add tests for the empty `referencedName` here
    val referencedName = Option(expr.getFirstChild)
      .collect { case expr: KtNameReferenceExpression => expr }
      .map(_.getText)
      .getOrElse("")
    val nameToClass =
      expr.getContainingKtFile.getDeclarations.asScala.collect { case c: KtClass => c.getName -> c }.toMap

    val imports = expr.getContainingKtFile.getImportList.getImports.asScala.toList
    val importedNames = imports.map { imp =>
      val importedName = Option(imp.getImportedName).map(_.toString).getOrElse(Constants.WildcardImportName)
      importedName -> imp
    }.toMap

    val methodFqName = if (importedNames.isDefinedAt(referencedName)) {
      importedNames(referencedName).getImportedFqName.toString
    } else if (Option(expr.getCalleeExpression).map(_.getText).exists(nameToClass.contains)) {
      val klass = nameToClass(expr.getCalleeExpression.getText)
      s"${klass.getContainingKtFile.getPackageFqName.toString}.$referencedName"
    } else {
      s"${expr.getContainingKtFile.getPackageFqName.toString}.$referencedName"
    }
    val explicitSignature = s"${Defines.UnresolvedSignature}(${argAsts.size})"
    val explicitFullName  = methodFqName

    val funcDesc = bindingUtils.getCalledFunctionDesc(expr.getCalleeExpression)
    val descFullName = funcDesc
      .orElse(getAmbiguousFuncDescIfFullNamesEqual(expr.getCalleeExpression))
      .flatMap(nameRenderer.descFullName)
      .getOrElse(explicitFullName)
    val signature = funcDesc
      .orElse(getAmbiguousFuncDescIfSignaturesEqual(expr.getCalleeExpression))
      .flatMap(nameRenderer.funcDescSignature)
      .getOrElse(explicitSignature)
    val fullName = nameRenderer.combineFunctionFullName(descFullName, signature)

    val resolvedCall = bindingUtils.getResolvedCallDesc(expr.getCalleeExpression)

    val (dispatchType, instanceAsArgument) =
      if (resolvedCall.isEmpty) {
        (DispatchTypes.STATIC_DISPATCH, false)
      } else {
        if (resolvedCall.get.getDispatchReceiver == null) {
          (DispatchTypes.STATIC_DISPATCH, false)
        } else {
          resolvedCall.get.getResultingDescriptor match {
            case functionDescriptor: FunctionDescriptor
                if functionDescriptor.getVisibility == DescriptorVisibilities.PRIVATE =>
              (DispatchTypes.STATIC_DISPATCH, true)
            case _ =>
              (DispatchTypes.DYNAMIC_DISPATCH, true)
          }
        }
      }

    // TODO: add test case to confirm whether the ANY fallback makes sense (could be void)
    val returnType = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val node = callNode(expr, expr.getText, referencedName, fullName, dispatchType, Some(signature), Some(returnType))

    val annotationsAsts = annotations.map(astForAnnotationEntry)
    val astWithAnnotations =
      if (dispatchType == DispatchTypes.STATIC_DISPATCH) {
        val compoundArgAsts =
          if (instanceAsArgument) {
            val instanceArgument = identifierNode(
              expr,
              Constants.ThisName,
              Constants.ThisName,
              nameRenderer.typeFullName(resolvedCall.get.getDispatchReceiver.getType).getOrElse(TypeConstants.Any)
            )
            val args = argAsts.prepended(Ast(instanceArgument))
            setArgumentIndices(args, 0)
            args
          } else {
            setArgumentIndices(argAsts)
            argAsts
          }

        Ast(withArgumentIndex(node, argIdx).argumentName(argNameMaybe))
          .withChildren(compoundArgAsts)
          .withArgEdges(node, compoundArgAsts.flatMap(_.root))
          .withChildren(annotationsAsts)
      } else {
        val receiverNode = identifierNode(
          expr,
          Constants.ThisName,
          Constants.ThisName,
          nameRenderer.typeFullName(resolvedCall.get.getDispatchReceiver.getType).getOrElse(TypeConstants.Any)
        )

        callAst(withArgumentIndex(node, argIdx).argumentName(argNameMaybe), argAsts, base = Some(Ast(receiverNode)))
          .withChildren(annotationsAsts)
      }

    List(astWithAnnotations)
  }

  private def astsForCtorCall(
    expr: KtCallExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Seq[Ast] = {
    val typeFullName = registerType(exprTypeFullName(expr).getOrElse(Defines.UnresolvedNamespace))
    val tmpBlockNode = blockNode(expr, "", typeFullName)
    val tmpName      = s"${Constants.TmpLocalPrefix}${tmpKeyPool.next}"
    val tmpLocalNode = localNode(expr, tmpName, tmpName, typeFullName)
    scope.addToScope(tmpName, tmpLocalNode)
    val tmpLocalAst = Ast(tmpLocalNode)

    val assignmentRhsNode =
      operatorCallNode(expr, Constants.Alloc, Operators.alloc, Option(typeFullName))
    val assignmentLhsNode = identifierNode(expr, tmpName, tmpName, typeFullName)
    val assignmentLhsAst  = astWithRefEdgeMaybe(tmpName, assignmentLhsNode)

    val assignmentNode = operatorCallNode(expr, Operators.assignment, Operators.assignment, None)
    val assignmentAst  = callAst(assignmentNode, List(assignmentLhsAst, Ast(assignmentRhsNode)))
    val initReceiverNode = identifierNode(expr, tmpName, tmpName, typeFullName)
      .argumentIndex(0)
    val initReceiverAst = astWithRefEdgeMaybe(tmpName, initReceiverNode)

    val argAstsWithTrail =
      withIndex(expr.getValueArguments.asScala.toSeq) { case (arg, idx) =>
        val argNameOpt = if (arg.isNamed) Option(arg.getArgumentName.getAsName.toString) else None
        val asts       = astsForExpression(arg.getArgumentExpression, Option(idx), argNameOpt)
        (asts.dropRight(1), asts.lastOption.getOrElse(Ast(unknownNode(arg.getArgumentExpression, Constants.Empty))))
      }
    val astsForTrails    = argAstsWithTrail.map(_._2)
    val astsForNonTrails = argAstsWithTrail.flatMap(_._1)

    val (fullName, signature) =
      calleeFullnameAndSignature(
        getCalleeExpr(expr),
        Defines.UnresolvedNamespace,
        s"${Defines.UnresolvedSignature}(${expr.getValueArguments.size()})"
      )
    registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))

    val initCallNode = callNode(
      expr,
      expr.getText,
      Defines.ConstructorMethodName,
      fullName,
      DispatchTypes.STATIC_DISPATCH,
      Some(signature),
      Some(TypeConstants.Void)
    )
    val initCallAst       = callAst(initCallNode, astsForTrails, Option(initReceiverAst))
    val lastIdentifier    = identifierNode(expr, tmpName, tmpName, typeFullName)
    val lastIdentifierAst = astWithRefEdgeMaybe(tmpName, lastIdentifier)

    val annotationsAsts = annotations.map(astForAnnotationEntry)
    astsForNonTrails ++ Seq(
      blockAst(
        withArgumentIndex(tmpBlockNode, argIdx).argumentName(argNameMaybe),
        List(tmpLocalAst, assignmentAst, initCallAst, lastIdentifierAst)
      ).withChildren(annotationsAsts)
    )
  }

  def astForPostfixExpression(
    expr: KtPostfixExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val operatorType = ktTokenToOperator(forPostfixExpr = true).applyOrElse(
      KtPsiUtil.getOperationToken(expr),
      { (token: KtToken) =>
        logger.warn(s"Unsupported token type encountered: $token")
        Constants.UnknownOperator
      }
    )
    val typeFullName = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val args = List(astsForExpression(expr.getBaseExpression, None).headOption.getOrElse(Ast()))
      .filterNot(_.root == null)
    val node =
      operatorCallNode(expr, expr.getText, operatorType, Option(typeFullName))
    callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForPrefixExpression(
    expr: KtPrefixExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val operatorType = ktTokenToOperator(forPostfixExpr = false).applyOrElse(
      KtPsiUtil.getOperationToken(expr),
      { (token: KtToken) =>
        logger.warn(s"Unsupported token type encountered: $token")
        Constants.UnknownOperator
      }
    )
    val typeFullName = registerType(exprTypeFullName(expr).getOrElse(TypeConstants.Any))
    val args = List(astsForExpression(expr.getBaseExpression, None).headOption.getOrElse(Ast()))
      .filterNot(_.root == null)
    val node =
      operatorCallNode(expr, expr.getText, operatorType, Option(typeFullName))
    callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForArrayAccess(
    expression: KtArrayAccessExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val typeFullName      = registerType(exprTypeFullName(expression).getOrElse(TypeConstants.Any))
    val baseExpressionAst = astsForExpression(expression.getArrayExpression, None)
    val astsForIndexExpr = expression.getIndexExpressions.asScala.zipWithIndex.flatMap { case (expr, idx) =>
      astsForExpression(expr, Option(idx + 1))
    }
    val callNode =
      operatorCallNode(expression, expression.getText, Operators.indexAccess, Option(typeFullName))
    callAst(withArgumentName(withArgumentIndex(callNode, argIdx), argName), baseExpressionAst ++ astsForIndexExpr)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  private def extractReceiverInfo(
    expr: KtCallableReferenceExpression,
    receiverExpr: Option[KtExpression],
    callableNameExpr: KtSimpleNameExpression,
    funcDesc: Option[FunctionDescriptor]
  ): AstCreator.ReceiverInfo = {
    val (ctorParamAst, receiverName, hasReceiver, isStaticReference) = receiverExpr match {
      case Some(r: KtNameReferenceExpression) if typeInfoProvider.isReferenceToClass(r) =>
        val receiverText = r.getText
        val companion    = s"$receiverText.companion"

        val companionObjectDescOpt = funcDesc.flatMap { desc =>
          desc.getContainingDeclaration match {
            case classDesc: ClassDescriptor if classDesc.isCompanionObject => Some(classDesc)
            case _                                                         => None
          }
        }

        val referencesCompanionObject = typeInfoProvider.isRefToCompanionObject(r) || companionObjectDescOpt.isDefined

        val (baseTypeFullName, companionTypeFullName) = if (referencesCompanionObject) {
          val companionType = companionObjectDescOpt
            .flatMap(nameRenderer.descFullName)
            .orElse(exprTypeFullName(r).map(t => s"$t$$Companion"))
            .getOrElse(s"$receiverText$$Companion")

          val baseType = companionObjectDescOpt
            .flatMap(c => Option(c.getContainingDeclaration))
            .collect { case classDesc: ClassDescriptor => classDesc }
            .flatMap(nameRenderer.descFullName)
            .orElse(exprTypeFullName(r))
            .getOrElse(receiverText)

          (baseType, companionType)
        } else {
          val baseType = exprTypeFullName(r).getOrElse(receiverText)
          (baseType, baseType)
        }

        if (referencesCompanionObject) {
          val argAsts = List(
            identifierNode(expr, receiverText, receiverText, registerType(baseTypeFullName)),
            fieldIdentifierNode(expr, Constants.CompanionObjectMemberName, Constants.CompanionObjectMemberName)
          ).map(Ast(_))
          val fieldAccessNode =
            operatorCallNode(expr, receiverText, Operators.fieldAccess, Option(companionTypeFullName))
          (callAst(fieldAccessNode, argAsts), companion, true, true)
        } else {
          val identNode = identifierNode(expr, receiverText, receiverText, registerType(companionTypeFullName))
          (astWithRefEdgeMaybe(receiverText, identNode), companion, true, true)
        }
      case Some(r: KtThisExpression) =>
        val thisTypeFullName = exprTypeFullName(r).getOrElse(TypeConstants.Any)
        val thisNode         = identifierNode(expr, "this", "this", registerType(thisTypeFullName))
        (Ast(thisNode), "this", true, false)
      case Some(r: KtSuperExpression) =>
        val superTypeFullName = exprTypeFullName(r).getOrElse(TypeConstants.Any)
        val superNode         = identifierNode(expr, "super", "super", registerType(superTypeFullName))
        (Ast(superNode), "super", true, false)
      case Some(r) =>
        val receiverText = r.getText
        val paramAst     = astsForExpression(r, Some(1)).headOption.getOrElse(Ast())
        (paramAst, receiverText, true, false)
      case None =>
        val resolvedCall = bindingUtils.getResolvedCallDesc(callableNameExpr)
        resolvedCall match {
          case Some(call) if call.getDispatchReceiver != null =>
            val thisTypeFullName =
              nameRenderer.typeFullName(call.getDispatchReceiver.getType).getOrElse(TypeConstants.Any)
            val thisNode = identifierNode(expr, "this", "this", registerType(thisTypeFullName))
            (Ast(thisNode), "this", true, false)
          case _ =>
            (Ast(), "", false, true)
        }
    }

    AstCreator.ReceiverInfo(ctorParamAst, receiverName, hasReceiver, isStaticReference)
  }

  private case class SamMethodInfo(
    methodName: String,
    params: List[(String, String)],
    signature: String,
    genericSignature: Option[String]
  )

  private def resolveSamMethodInfo(
    exprType: Option[KotlinType],
    funcDesc: Option[FunctionDescriptor]
  ): SamMethodInfo = {
    val samMethod = exprType.flatMap { ktType =>
      val classDescriptor = ktType.getConstructor.getDeclarationDescriptor.asInstanceOf[ClassDescriptor]
      val abstractMethods = classDescriptor.getUnsubstitutedMemberScope
        .getContributedDescriptors(DescriptorKindFilter.FUNCTIONS, _ => true)
        .asScala
        .collect { case f: FunctionDescriptor if f.getModality.toString == "ABSTRACT" => f }
        .toList
      abstractMethods.headOption
    }

    val samMethodName = samMethod.flatMap(f => Some(f.getName().toString)).getOrElse("invoke")

    val (samMethodParams, samMethodSig, samGenericMethodSig) = (samMethod, exprType) match {
      case (Some(desc), Some(ktType)) =>
        val typeSubstitutor = org.jetbrains.kotlin.types.TypeSubstitutor.create(ktType)

        val params = desc.getValueParameters.asScala.toList.zipWithIndex.map { case (param, idx) =>
          val paramName = param.getName.toString match {
            case "<anonymous>" => s"p$idx"
            case name          => name
          }
          val substitutedType = typeSubstitutor.substitute(param.getType, org.jetbrains.kotlin.types.Variance.INVARIANT)
          val paramTypeFullName = Option(substitutedType)
            .flatMap(t => nameRenderer.typeFullName(t))
            .getOrElse(TypeConstants.Any)
          (paramName, paramTypeFullName)
        }

        val substitutedReturnType =
          typeSubstitutor.substitute(desc.getReturnType, org.jetbrains.kotlin.types.Variance.INVARIANT)
        val returnTypeFullName = Option(substitutedReturnType)
          .flatMap(t => nameRenderer.typeFullName(t))
          .getOrElse(TypeConstants.Any)

        val paramTypes = params.map(_._2).mkString(",")
        val sig        = s"$returnTypeFullName($paramTypes)"

        val genericSig = createErasedSignature(desc)

        (params, sig, genericSig)
      case _ =>
        (List.empty, "java.lang.Object(java.lang.Object[])", None)
    }

    SamMethodInfo(samMethodName, samMethodParams, samMethodSig, samGenericMethodSig)
  }

  def astForCallableReferenceExpression(
    expr: KtCallableReferenceExpression,
    argIdx: Option[Int],
    argNameMaybe: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq(),
    argTypeFallback: Option[KotlinType] = None
  ): Ast = {

    val callableNameExpr = expr.getCallableReference
    val methodName       = callableNameExpr.getText

    val funcDesc = bindingUtils
      .getCalledFunctionDesc(callableNameExpr)
      .orElse(getAmbiguousFuncDescIfSignaturesEqual(callableNameExpr))
    val fullName = funcDesc.flatMap(nameRenderer.descFullName).getOrElse(methodName)
    val signature = funcDesc
      .flatMap(nameRenderer.funcDescSignature)
      .getOrElse(Defines.UnresolvedSignature)

    val exprFallbackType = bindingUtils.getExpectedExprType(expr)
    val exprType         = argTypeFallback.orElse(exprFallbackType)
    val inheritsFromTypeFullName =
      exprType.flatMap(nameRenderer.typeFullName).getOrElse("kotlin.jvm.functions.FunctionN")

    val receiverExpr = Option(expr.getReceiverExpression)

    val receiverInfo = extractReceiverInfo(expr, receiverExpr, callableNameExpr, funcDesc)

    val isUnboundReference = !receiverInfo.hasReceiver && receiverExpr.isEmpty

    val inheritsList =
      if (receiverInfo.hasReceiver) List(inheritsFromTypeFullName, "kotlin.jvm.internal.CallableReference")
      else List(inheritsFromTypeFullName)

    val samImplClass = s"$fullName$$${inheritsFromTypeFullName}Impl"
    registerType(samImplClass)

    val samMethodInfo = resolveSamMethodInfo(exprType, funcDesc)

    if (isUnboundReference) {
      val methodRefNode = NewMethodRef()
        .methodFullName(s"$fullName:$signature")
        .code(code(expr))
        .typeFullName(samImplClass)
        .lineNumber(line(expr))
        .columnNumber(column(expr))

      val samInfo = AstCreator.UnboundSamInfo(
        samImplClass,
        fullName,
        signature,
        inheritsList,
        samMethodInfo.methodName,
        samMethodInfo.signature,
        samMethodInfo.genericSignature,
        samMethodInfo.params,
        expr
      )

      val samMethodFullName = s"$samImplClass.${samMethodInfo.methodName}:${samMethodInfo.signature}"
      samImplInfoQueue.getOrElseUpdate(samMethodFullName, samInfo)

      Ast(methodRefNode)
    } else {
      val receiverTypeFullName = receiverInfo.ctorParamAst.root
        .map(_.properties.get("TYPE_FULL_NAME").getOrElse(TypeConstants.JavaLangObject).toString)
        .getOrElse(TypeConstants.JavaLangObject)

      val samInfo = AstCreator.BoundSamInfo(
        samImplClass,
        fullName,
        signature,
        inheritsList,
        samMethodInfo.methodName,
        samMethodInfo.signature,
        samMethodInfo.genericSignature,
        (receiverInfo.ctorParamAst, receiverInfo.receiverName),
        receiverTypeFullName,
        samMethodInfo.params,
        receiverInfo.isStaticReference,
        expr
      )

      val samMethodFullName = s"$samImplClass.${samMethodInfo.methodName}:${samMethodInfo.signature}"
      samImplInfoQueue.getOrElseUpdate(samMethodFullName, samInfo)

      val tmpBlockNode = blockNode(expr, expr.getText, samImplClass)
      val tmpName      = s"${Constants.TmpLocalPrefix}${tmpKeyPool.next}"
      val tmpLocalNode = localNode(expr, tmpName, tmpName, samImplClass)
      scope.addToScope(tmpName, tmpLocalNode)
      val tmpLocalAst = Ast(tmpLocalNode)

      val assignmentRhsNode = operatorCallNode(expr, Operators.alloc, Operators.alloc, Option(samImplClass))
      val assignmentLhsNode = identifierNode(expr, tmpName, tmpName, samImplClass)
      val assignmentLhsAst  = astWithRefEdgeMaybe(tmpName, assignmentLhsNode)

      val assignmentNode = operatorCallNode(expr, Operators.assignment, Operators.assignment, None)
      val assignmentAst  = callAst(assignmentNode, List(assignmentLhsAst, Ast(assignmentRhsNode)))

      val initReceiverNode = identifierNode(expr, tmpName, tmpName, samImplClass).argumentIndex(0)
      val initReceiverAst  = astWithRefEdgeMaybe(tmpName, initReceiverNode)

      val ctorSignature = s"void($receiverTypeFullName)"
      val ctorCallNode = callNode(
        expr,
        s"$samImplClass(${receiverInfo.receiverName})",
        Defines.ConstructorMethodName,
        s"$samImplClass.${Defines.ConstructorMethodName}:$ctorSignature",
        DispatchTypes.STATIC_DISPATCH,
        Some(ctorSignature),
        Some(TypeConstants.Void)
      )
      val ctorArgs    = List(receiverInfo.ctorParamAst)
      val ctorCallAst = callAst(ctorCallNode, ctorArgs, Option(initReceiverAst))

      val lastIdentifier    = identifierNode(expr, tmpName, tmpName, samImplClass)
      val lastIdentifierAst = astWithRefEdgeMaybe(tmpName, lastIdentifier)

      blockAst(tmpBlockNode, List(tmpLocalAst, assignmentAst, ctorCallAst, lastIdentifierAst))
    }
  }
}
