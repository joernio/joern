package io.joern.kotlin2cpg.ast

import io.joern.kotlin2cpg.Constants
import io.joern.kotlin2cpg.types.CallKind
import io.joern.kotlin2cpg.types.TypeConstants
import io.joern.x2cpg.Ast
import io.joern.x2cpg.Defines
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.utils.NodeBuilders
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodRef
import org.jetbrains.kotlin.descriptors.DescriptorVisibilities
import org.jetbrains.kotlin.descriptors.FunctionDescriptor
import org.jetbrains.kotlin.lexer.KtToken
import org.jetbrains.kotlin.lexer.KtTokens
import org.jetbrains.kotlin.psi.*

import scala.jdk.CollectionConverters.*

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
    val node = withArgumentIndex(
      NodeBuilders.newOperatorCallNode(Operators.fieldAccess, expr.getText, Option(retType), line(expr), column(expr)),
      argIdx
    ).argumentName(argNameMaybe)
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
        val rhsAst       = Ast(NodeBuilders.newOperatorCallNode(Operators.alloc, Operators.alloc, Option(typeFullName)))

        val identifier    = identifierNode(expr, localName, localName, local.typeFullName)
        val identifierAst = astWithRefEdgeMaybe(identifier.name, identifier)

        val assignmentNode = NodeBuilders.newOperatorCallNode(
          Operators.assignment,
          s"${identifier.name} = ${Operators.alloc}",
          None,
          line(expr),
          column(expr)
        )
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
          NodeBuilders.newOperatorCallNode(fullName, expr.getText, Option(retType), line(expr), column(expr))
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
    val node = NodeBuilders.newOperatorCallNode(Operators.is, expr.getText, None, line(expr), column(expr))
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
    val node = NodeBuilders.newOperatorCallNode(Operators.cast, expr.getText, None, line(expr), column(expr))
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
      NodeBuilders.newOperatorCallNode(Operators.alloc, Constants.Alloc, Option(typeFullName), line(expr), column(expr))
    val assignmentLhsNode = identifierNode(expr, tmpName, tmpName, typeFullName)
    val assignmentLhsAst  = astWithRefEdgeMaybe(tmpName, assignmentLhsNode)

    val assignmentNode = NodeBuilders.newOperatorCallNode(Operators.assignment, Operators.assignment)
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
      NodeBuilders.newOperatorCallNode(operatorType, expr.getText, Option(typeFullName), line(expr), column(expr))
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
      NodeBuilders.newOperatorCallNode(operatorType, expr.getText, Option(typeFullName), line(expr), column(expr))
    callAst(withArgumentName(withArgumentIndex(node, argIdx), argName), args)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

  def astForArrayAccess(
    expression: KtArrayAccessExpression,
    argIdx: Option[Int],
    argName: Option[String],
    annotations: Seq[KtAnnotationEntry] = Seq()
  ): Ast = {
    val arrayExpr     = expression.getArrayExpression
    val typeFullName  = registerType(exprTypeFullName(expression).getOrElse(TypeConstants.Any))
    val identifier    = identifierNode(arrayExpr, arrayExpr.getText, arrayExpr.getText, typeFullName)
    val identifierAst = astWithRefEdgeMaybe(arrayExpr.getText, identifier)
    val astsForIndexExpr = expression.getIndexExpressions.asScala.zipWithIndex.flatMap { case (expr, idx) =>
      astsForExpression(expr, Option(idx + 1))
    }
    val callNode =
      NodeBuilders.newOperatorCallNode(
        Operators.indexAccess,
        expression.getText,
        Option(typeFullName),
        line(expression),
        column(expression)
      )
    callAst(withArgumentName(withArgumentIndex(callNode, argIdx), argName), List(identifierAst) ++ astsForIndexExpr)
      .withChildren(annotations.map(astForAnnotationEntry))
  }

}
