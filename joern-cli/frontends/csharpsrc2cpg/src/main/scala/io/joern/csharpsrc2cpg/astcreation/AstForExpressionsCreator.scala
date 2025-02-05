package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.astcreation.AstParseLevel.FULL_AST
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.csharpsrc2cpg.datastructures.{CSharpMethod, FieldDecl}
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.csharpsrc2cpg.utils.Utils.{composeMethodFullName, composeMethodLikeSignature}
import io.joern.csharpsrc2cpg.{CSharpOperators, Constants}
import io.joern.x2cpg.utils.NodeBuilders.{newCallNode, newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewLiteral, NewTypeRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import ujson.Value

import scala.collection.mutable.ArrayBuffer
import scala.util.Try
trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpressionStatement(expr: DotNetNodeInfo): Seq[Ast] = {
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    astForExpression(expressionNode)
  }

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    expr.node match {
      case _: UnaryExpr                      => astForUnaryExpression(expr)
      case _: BinaryExpr                     => astForBinaryExpression(expr)
      case _: LiteralExpr                    => astForLiteralExpression(expr)
      case InvocationExpression              => astForInvocationExpression(expr)
      case AwaitExpression                   => astForAwaitExpression(expr)
      case ObjectCreationExpression          => astForObjectCreationExpression(expr)
      case SimpleMemberAccessExpression      => astForSimpleMemberAccessExpression(expr)
      case ElementAccessExpression           => astForElementAccessExpression(expr)
      case ImplicitArrayCreationExpression   => astForImplicitArrayCreationExpression(expr)
      case ConditionalExpression             => astForConditionalExpression(expr)
      case _: IdentifierNode                 => astForIdentifier(expr) :: Nil
      case ThisExpression                    => astForThisReceiver(expr) :: Nil
      case CastExpression                    => astForCastExpression(expr)
      case InterpolatedStringExpression      => astForInterpolatedStringExpression(expr)
      case ConditionalAccessExpression       => astForConditionalAccessExpression(expr)
      case SuppressNullableWarningExpression => astForSuppressNullableWarningExpression(expr)
      case _: BaseLambdaExpression           => astForSimpleLambdaExpression(expr)
      case ParenthesizedExpression           => astForParenthesizedExpression(expr)
      case _                                 => notHandledYet(expr)
    }
  }

  /** Attempts to decide if [[expr]] denotes a setter property reference, in which case returns its corresponding
    * [[CSharpMethod]] meta-data and class full name it belongs to.
    */
  private def tryResolveSetterInvocation(expr: DotNetNodeInfo): Option[(CSharpMethod, String)] = {
    val baseType = expr.node match {
      case SimpleMemberAccessExpression =>
        val base = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
        Some(nodeTypeFullName(base))
      case _ =>
        None
    }

    val fieldName = nameFromNode(expr)
    baseType.flatMap(x => scope.tryResolveSetterInvocation(fieldName, Some(x)).map((_, x)))
  }

  private def stripAssignmentFromOperator(operatorName: String): Option[String] = operatorName match {
    case Operators.assignmentPlus                 => Some(Operators.plus)
    case Operators.assignmentMinus                => Some(Operators.minus)
    case Operators.assignmentMultiplication       => Some(Operators.multiplication)
    case Operators.assignmentDivision             => Some(Operators.division)
    case Operators.assignmentExponentiation       => Some(Operators.exponentiation)
    case Operators.assignmentModulo               => Some(Operators.modulo)
    case Operators.assignmentShiftLeft            => Some(Operators.shiftLeft)
    case Operators.assignmentLogicalShiftRight    => Some(Operators.logicalShiftRight)
    case Operators.assignmentArithmeticShiftRight => Some(Operators.arithmeticShiftRight)
    case Operators.assignmentAnd                  => Some(Operators.and)
    case Operators.assignmentOr                   => Some(Operators.or)
    case Operators.assignmentXor                  => Some(Operators.xor)
    case _                                        => None
  }

  private def astForSetterAssignmentExpression(
    assignExpr: DotNetNodeInfo,
    setterInfo: (CSharpMethod, String),
    lhs: DotNetNodeInfo,
    opName: String,
    rhsNode: DotNetNodeInfo
  ): Seq[Ast] = {
    val (setterMethod, setterBaseType) = setterInfo

    lhs.node match {
      case SimpleMemberAccessExpression =>
        val baseNode     = astForNode(createDotNetNodeInfo(lhs.json(ParserKeys.Expression)))
        val receiver     = if setterMethod.isStatic then None else baseNode.headOption
        val propertyName = setterMethod.name.stripPrefix("set_")
        val originalRhs  = astForOperand(rhsNode)

        val rhsAst = opName match {
          case Operators.assignment => originalRhs
          case _ =>
            scope.tryResolveGetterInvocation(propertyName, Some(setterBaseType)) match {
              // Shouldn't happen, provided it is valid code. At any rate, log and emit the RHS verbatim.
              case None =>
                logger.warn(s"Couldn't find matching getter for $propertyName in ${code(assignExpr)}")
                originalRhs
              case Some(getterMethod) =>
                stripAssignmentFromOperator(opName) match {
                  case None =>
                    logger.warn(s"Unrecognized assignment in ${code(assignExpr)}")
                    originalRhs
                  case Some(opName) =>
                    val getterInvocation = createInvocationAst(
                      assignExpr,
                      getterMethod.name,
                      Nil,
                      receiver,
                      Some(getterMethod),
                      Some(setterBaseType)
                    )
                    val operatorCall = newOperatorCallNode(
                      opName,
                      code(assignExpr),
                      Some(setterMethod.returnType),
                      line(assignExpr),
                      column(assignExpr)
                    )
                    callAst(operatorCall, getterInvocation +: originalRhs, None, None) :: Nil
                }
            }
        }

        createInvocationAst(
          assignExpr,
          setterMethod.name,
          rhsAst,
          receiver,
          Some(setterMethod),
          Some(setterBaseType)
        ) :: Nil
      case _ =>
        logger.warn(s"Unsupported setter assignment: ${code(assignExpr)}")
        Nil
    }
  }

  private def astForAssignmentExpression(
    assignExpr: DotNetNodeInfo,
    lhsNode: DotNetNodeInfo,
    opName: String,
    rhsNode: DotNetNodeInfo
  ): Seq[Ast] = {
    tryResolveSetterInvocation(lhsNode) match {
      case Some(setterInfo) => astForSetterAssignmentExpression(assignExpr, setterInfo, lhsNode, opName, rhsNode)
      case None             => astForRegularAssignmentExpression(assignExpr, lhsNode, opName, rhsNode)
    }
  }

  private def astForRegularAssignmentExpression(
    assignExpr: DotNetNodeInfo,
    lhs: DotNetNodeInfo,
    opName: String,
    rhs: DotNetNodeInfo
  ): Seq[Ast] = {
    astForRegularBinaryExpression(assignExpr, lhs, opName, rhs)
  }

  private def astForParenthesizedExpression(parenExpr: DotNetNodeInfo): Seq[Ast] = {
    astForNode(parenExpr.json(ParserKeys.Expression))
  }

  private def astForAwaitExpression(awaitExpr: DotNetNodeInfo): Seq[Ast] = {
    /* fullName is the name in case of STATIC_DISPATCH */
    val node =
      callNode(awaitExpr, awaitExpr.code, CSharpOperators.await, CSharpOperators.await, DispatchTypes.STATIC_DISPATCH)
    val argAsts = astForNode(awaitExpr.json(ParserKeys.Expression))
    Seq(callAst(node, argAsts))
  }

  protected def astForExpressionElement(expressionElement: DotNetNodeInfo): Seq[Ast] = {
    astForNode(expressionElement.json(ParserKeys.Expression))
  }

  protected def astForLiteralExpression(_literalNode: DotNetNodeInfo): Seq[Ast] = {
    Seq(Ast(literalNode(_literalNode, code(_literalNode), nodeTypeFullName(_literalNode))))
  }

  protected def astForOperand(operandNode: DotNetNodeInfo): Seq[Ast] = {
    operandNode.node match {
      case IdentifierName =>
        (scope.findFieldInScope(nameFromNode(operandNode)), scope.lookupVariable(nameFromNode(operandNode))) match {
          case (Some(field), None) => createImplicitBaseFieldAccess(operandNode, field)
          case _                   => astForNode(operandNode)
        }
      case _ => astForNode(operandNode)
    }
  }

  private def createImplicitBaseFieldAccess(fieldNode: DotNetNodeInfo, field: FieldDecl): Seq[Ast] = {
    // TODO: Maybe this should be a TypeRef, like we recently started doing for javasrc?
    val baseNode = if (field.isStatic) {
      newIdentifierNode(scope.surroundingTypeDeclFullName.getOrElse(Defines.Any), field.typeFullName)
    } else {
      newIdentifierNode(Constants.This, field.typeFullName)
    }

    fieldAccessAst(
      base = Ast(baseNode),
      code = s"${baseNode.code}.${field.name}",
      lineNo = fieldNode.lineNumber,
      columnNo = fieldNode.columnNumber,
      fieldName = field.name,
      fieldTypeFullName = field.typeFullName,
      fieldLineNo = fieldNode.lineNumber,
      fieldColumnNo = fieldNode.columnNumber
    ) :: Nil
  }

  protected def astForUnaryExpression(unaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = unaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+" => Operators.plus
      case "-" => Operators.minus
      case "++" =>
        if (unaryExpr.node.getClass == PostIncrementExpression.getClass) Operators.postIncrement
        else Operators.preIncrement
      case "--" =>
        if (unaryExpr.node.getClass == PostDecrementExpression.getClass) Operators.postDecrement
        else Operators.preDecrement
      case "~" => Operators.not
      case "!" => Operators.logicalNot
      case "&" => Operators.addressOf

    val args     = createDotNetNodeInfo(unaryExpr.json(ParserKeys.Operand))
    val argsAst  = astForOperand(args)
    val callNode = operatorCallNode(unaryExpr, operatorName, Some(nodeTypeFullName(args)))

    callAst(callNode, argsAst) :: Nil
  }

  private def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val lhsNode       = createDotNetNodeInfo(binaryExpr.json(ParserKeys.Left))
    val rhsNode       = createDotNetNodeInfo(binaryExpr.json(ParserKeys.Right))
    val operatorToken = binaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = binaryOperatorsMap.getOrElse(
      operatorToken, {
        logger.warn(s"Unhandled operator '$operatorToken' for ${code(binaryExpr)}")
        CSharpOperators.unknown
      }
    )
    binaryExpr.node match {
      case _: AssignmentExpr => astForAssignmentExpression(binaryExpr, lhsNode, operatorName, rhsNode)
      case _                 => astForRegularBinaryExpression(binaryExpr, lhsNode, operatorName, rhsNode)
    }
  }

  private def astForRegularBinaryExpression(
    binaryExpr: DotNetNodeInfo,
    lhsNode: DotNetNodeInfo,
    operatorName: String,
    rhsNode: DotNetNodeInfo
  ): Seq[Ast] = {
    val args         = astForOperand(lhsNode) ++: astForOperand(rhsNode)
    val typeFullName = fixedTypeOperators.get(operatorName).orElse(Some(getTypeFullNameFromAstNode(args)))
    val callNode     = operatorCallNode(binaryExpr, operatorName, typeFullName)
    callAst(callNode, args) :: Nil
  }

  /** Handles the `= ...` part of the equals value clause, thus this only contains an RHS.
    */
  protected def astForEqualsValueClause(clause: DotNetNodeInfo): Seq[Ast] = {
    val rhsNode = createDotNetNodeInfo(clause.json(ParserKeys.Value))
    astForNode(rhsNode)
  }

  protected def astForArrayInitializerExpression(arrayInitializerExpression: DotNetNodeInfo): Seq[Ast] = {
    astForCollectionStaticInitializer(arrayInitializerExpression, ParserKeys.Expressions)
  }

  protected def astForCollectionExpression(collectionExpression: DotNetNodeInfo): Seq[Ast] = {
    astForCollectionStaticInitializer(collectionExpression, ParserKeys.Elements)
  }

  private def astForCollectionStaticInitializer(
    arrayInitializerExpression: DotNetNodeInfo,
    elementParserKey: String
  ): Seq[Ast] = {
    val MAX_INITIALIZERS = 1000

    val elements = arrayInitializerExpression.json(elementParserKey).arr

    val nestedExpressionsExists =
      Try(elements.map(createDotNetNodeInfo).map(_.json(elementParserKey))).getOrElse(ArrayBuffer.empty).nonEmpty

    // We have more expressions in our expressions, which means we have a 2+D array, parse these
    val args: Seq[Ast] = if (nestedExpressionsExists) {
      elements.map(createDotNetNodeInfo).flatMap(astForCollectionStaticInitializer(_, elementParserKey)).toSeq
    } else {
      elements.slice(0, MAX_INITIALIZERS).map(createDotNetNodeInfo).flatMap(astForNode).toSeq
    }

    val typeFullName = elementParserKey match {
      case ParserKeys.Expressions => s"${getTypeFullNameFromAstNode(args)}[]"
      case ParserKeys.Elements    => "System.List"
    }

    val callNode = newOperatorCallNode(
      Operators.arrayInitializer,
      code = arrayInitializerExpression.json(ParserKeys.MetaData)(ParserKeys.Code).str,
      typeFullName = Some(typeFullName),
      line = arrayInitializerExpression.lineNumber,
      column = arrayInitializerExpression.columnNumber
    )

    val ast = callAst(callNode, args)

    // TODO: This will work as expected for 1D collections, but is going to require some thinking for 2+D arrays since we
    //  will have to keep track of the number of elements in each sub-array
    if (elements.size > MAX_INITIALIZERS) {
      val placeholder = NewLiteral()
        .typeFullName(Defines.Any)
        .code("<too-many-initializers>")
        .lineNumber(arrayInitializerExpression.lineNumber)
        .columnNumber(arrayInitializerExpression.columnNumber)

      Seq(ast.withChild(Ast(placeholder)).withArgEdge(callNode, placeholder))
    } else {
      Seq(ast)
    }
  }

  private def createInvocationAst(
    invocationExpr: DotNetNodeInfo,
    callName: String,
    arguments: Seq[Ast],
    baseAst: Option[Ast],
    methodMetaData: Option[CSharpMethod],
    baseTypeFullName: Option[String]
  ): Ast = {
    val methodSignature = methodMetaData match {
      case Some(m) =>
        val returnType = DotNetTypeMap.getOrElse(m.returnType, m.returnType)
        composeMethodLikeSignature(returnType, m.parameterTypes.filterNot(_._1 == Constants.This).map(_._2))
      case None => Defines.UnresolvedSignature
    }

    val methodFullName = baseTypeFullName match {
      case Some(typeFullName) => composeMethodFullName(typeFullName, callName, methodSignature)
      case _                  => composeMethodFullName(Defines.UnresolvedNamespace, callName, methodSignature)
    }
    val dispatchType = methodMetaData
      .map(_.isStatic)
      .map {
        case true  => DispatchTypes.STATIC_DISPATCH
        case false => DispatchTypes.DYNAMIC_DISPATCH
      }
      .getOrElse(DispatchTypes.STATIC_DISPATCH)

    val _callAst = callAst(
      callNode(
        invocationExpr,
        code(invocationExpr),
        callName,
        methodFullName,
        dispatchType,
        Option(methodSignature),
        methodMetaData.map(_.returnType)
      ),
      arguments,
      baseAst
    )

    _callAst
  }

  /** Handles expressions like `foo.Bar()`. If `Bar` can't be found inside `foo`'s class, attempts to find a compatible
    * extension method. If all fails, an AST is still produced.
    */
  private def astForMemberAccessInvocation(
    invocationExpr: DotNetNodeInfo,
    baseAst: Option[Ast],
    argumentList: DotNetNodeInfo,
    callName: String
  ): Seq[Ast] = {

    val baseTypeFullName = Some(getTypeFullNameFromAstNode(baseAst.toList)).filterNot(_ == Defines.Any)
    val arguments        = astForArgumentList(argumentList, baseTypeFullName)
    val argTypes         = arguments.map(getTypeFullNameFromAstNode).toList

    val byMethod         = scope.tryResolveMethodInvocation(callName, argTypes, baseTypeFullName)
    lazy val byExtMethod = scope.tryResolveExtensionMethodInvocation(baseTypeFullName, callName, argTypes)

    val (method, baseType) = byMethod
      .map(x => (Some(x), baseTypeFullName))
      .orElse(byExtMethod.map(x => (Some(x._1), Some(x._2))))
      .getOrElse((None, baseTypeFullName))

    createInvocationAst(invocationExpr, callName, arguments, baseAst, method, baseType) :: Nil
  }

  private def astForIdentifierInvocation(
    invocationExpr: DotNetNodeInfo,
    argumentList: DotNetNodeInfo,
    callName: String
  ): Seq[Ast] = {
    // This is when a call is made directly, which could also be made from a static import
    val argTypes = astForArgumentList(argumentList).map(getTypeFullNameFromAstNode).toList
    val (receiver, baseType, method, args) = scope
      .tryResolveMethodInvocation(callName, argTypes)
      .orElse(scope.tryResolveMethodInvocation(callName, argTypes, scope.surroundingTypeDeclFullName)) match {
      case Some(methodMetaData) if methodMetaData.isStatic =>
        // If static, create implicit type identifier explicitly
        val typeMetaData = scope.typeForMethod(methodMetaData)
        val typeName     = typeMetaData.flatMap(_.name.split("[.]").lastOption).getOrElse(Defines.Any)
        val typeFullName = typeMetaData.map(_.name)
        val receiverNode = Ast(identifierNode(invocationExpr, typeName, typeName, typeFullName.getOrElse(Defines.Any)))
        val arguments    = astForArgumentList(argumentList, typeFullName)
        (Option(receiverNode), typeFullName, Option(methodMetaData), arguments)
      case Some(methodMetaData) =>
        // If dynamic, create implicit `this` identifier explicitly
        val typeMetaData = scope.typeForMethod(methodMetaData)
        val typeFullName = typeMetaData.map(_.name)
        val thisAst      = astForThisReceiver(invocationExpr, typeFullName)
        val arguments    = astForArgumentList(argumentList, typeFullName)
        (Option(thisAst), typeMetaData.map(_.name), Option(methodMetaData), arguments)
      case None =>
        (None, None, None, Seq.empty[Ast])
    }

    createInvocationAst(invocationExpr, callName, args, receiver, method, baseType) :: Nil
  }

  private def astForInvocationExpression(invocationExpr: DotNetNodeInfo): Seq[Ast] = {
    val expression   = createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression))
    val callName     = nameFromNode(expression)
    val argumentList = createDotNetNodeInfo(invocationExpr.json(ParserKeys.ArgumentList))

    expression.node match {
      case SimpleMemberAccessExpression | SuppressNullableWarningExpression =>
        val baseAst = astForNode(createDotNetNodeInfo(expression.json(ParserKeys.Expression)))
        astForMemberAccessInvocation(invocationExpr, baseAst.headOption, argumentList, callName)
      case IdentifierName | MemberBindingExpression =>
        astForIdentifierInvocation(invocationExpr, argumentList, callName)
      case x =>
        logger.warn(s"Unhandled LHS $x for InvocationExpression")
        Nil
    }
  }

  /** Handles expressions like `foo.MyField`, where `MyField` is known to be a getter property. Getters are lowered into
    * calls, e.g. (a) System.Console.Out becomes System.Console.get_Out(), because it's a static method; (b) x.KeyChar
    * becomes System.ConsoleKeyInfo.get_KeyChar(x), because it's a dynamic method.
    */
  private def astForMemberAccessGetterExpression(
    getter: CSharpMethod,
    baseAst: Ast,
    baseTypeFullName: String,
    accessExpr: DotNetNodeInfo
  ): Seq[Ast] = {
    if (getter.isStatic) {
      callAst(
        newCallNode(
          getter.name,
          Some(baseTypeFullName),
          getter.returnType,
          DispatchTypes.STATIC_DISPATCH,
          Nil,
          code(accessExpr),
          line(accessExpr),
          column(accessExpr)
        )
      ) :: Nil
    } else {
      callAst(
        newCallNode(
          getter.name,
          Some(baseTypeFullName),
          getter.returnType,
          DispatchTypes.DYNAMIC_DISPATCH,
          baseTypeFullName :: Nil,
          code(accessExpr),
          line(accessExpr),
          column(accessExpr)
        ),
        base = Some(baseAst)
      ) :: Nil
    }
  }

  private def astForSimpleMemberAccessExpression(accessExpr: DotNetNodeInfo): Seq[Ast] = {
    val fieldIdentifierName = nameFromNode(accessExpr)
    val baseAst             = astForNode(createDotNetNodeInfo(accessExpr.json(ParserKeys.Expression))).head
    val baseTypeFullName    = getTypeFullNameFromAstNode(baseAst)

    // The typical field access resolving mechanism
    lazy val byFieldAccess = scope.tryResolveFieldAccess(fieldIdentifierName, Some(baseTypeFullName))

    // Getters look like fields, but are underneath `get_`-prefixed methods
    lazy val byPropertyName = scope.tryResolveGetterInvocation(fieldIdentifierName, Some(baseTypeFullName))

    // accessExpr might be a qualified name e.g. `System.Console`, in which case `System` (baseAst) is not a type
    // but a namespace. In this scenario, we look up the entire expression
    lazy val byQualifiedName = scope.tryResolveTypeReference(accessExpr.code)

    val (typeFullName, isGetter) = byFieldAccess
      .map(x => (x.typeName, false))
      .orElse(byPropertyName.map(x => (x.returnType, true)))
      .orElse(byQualifiedName.map(x => (x.name, false)))
      .getOrElse((Defines.Any, false))

    if (isGetter) {
      astForMemberAccessGetterExpression(byPropertyName.get, baseAst, baseTypeFullName, accessExpr)
    } else {
      fieldAccessAst(
        baseAst,
        code(accessExpr),
        line(accessExpr),
        column(accessExpr),
        fieldIdentifierName,
        typeFullName,
        line(accessExpr),
        column(accessExpr)
      ) :: Nil
    }
  }

  protected def astForElementAccessExpression(elementAccessExpression: DotNetNodeInfo): Seq[Ast] = {
    val exprAst = astForExpression(createDotNetNodeInfo(elementAccessExpression.json(ParserKeys.Expression)))

    createDotNetNodeInfo(elementAccessExpression.json(ParserKeys.ArgumentList))
      .json(ParserKeys.Arguments)
      .arr
      .map { x =>
        val argDotNetInfo = createDotNetNodeInfo(x)
        val argAst        = astForExpression(createDotNetNodeInfo(argDotNetInfo.json(ParserKeys.Expression)))
        val callNode = newOperatorCallNode(
          Operators.indexAccess,
          code = elementAccessExpression.code,
          line = line(elementAccessExpression),
          column = column(elementAccessExpression)
        )

        callAst(callNode, exprAst ++ argAst)
      }
      .toSeq
  }

  def astForObjectCreationExpression(objectCreation: DotNetNodeInfo): Seq[Ast] = {
    val dispatchType = DispatchTypes.STATIC_DISPATCH
    val typeFullName = Try(createDotNetNodeInfo(objectCreation.json(ParserKeys.Type))).toOption
      .map(nodeTypeFullName)
      .getOrElse(Defines.Any)

    val arguments = Try(astForArgumentList(createDotNetNodeInfo(objectCreation.json(ParserKeys.ArgumentList))))
      .getOrElse(Seq.empty)
    // TODO: Handle signature
    val signature      = None
    val name           = Defines.ConstructorMethodName
    val methodFullName = s"$typeFullName.$name"
    val _callNode = callNode(
      objectCreation,
      code(objectCreation),
      name,
      methodFullName,
      dispatchType,
      signature,
      Option(typeFullName)
    )

    Seq(callAst(_callNode, arguments, Option(Ast(thisNode))))
  }

  private def astForArgumentList(argumentList: DotNetNodeInfo, baseTypeHint: Option[String] = None): Seq[Ast] = {
    argumentList
      .json(ParserKeys.Arguments)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(x =>
        val argExpression = createDotNetNodeInfo(x.json(ParserKeys.Expression))
        argExpression.node match {
          case _: BaseLambdaExpression =>
            astForSimpleLambdaExpression(argExpression, baseTypeHint)
          case _ => astForExpressionStatement(x)
        }
      )
      .toSeq
  }

  private def astForConditionalExpression(condExpr: DotNetNodeInfo): Seq[Ast] = {
    val conditionAst = astForNode(condExpr.json(ParserKeys.Condition))
    val whenTrue     = astForNode(condExpr.json(ParserKeys.WhenTrue))
    val whenFalse    = astForNode(condExpr.json(ParserKeys.WhenFalse))

    val typeFullName =
      Option(getTypeFullNameFromAstNode(whenTrue))
        .orElse(Option(getTypeFullNameFromAstNode(whenFalse)))
        .orElse(Option(Defines.Any))
    val callNode =
      newOperatorCallNode(Operators.conditional, code(condExpr), typeFullName, line(condExpr), column(condExpr))

    Seq(callAst(callNode, conditionAst ++ whenTrue ++ whenFalse))
  }

  private def astForCastExpression(castExpr: DotNetNodeInfo): Seq[Ast] = {
    val typeFullName = nodeTypeFullName(createDotNetNodeInfo(castExpr.json(ParserKeys.Type)))

    val callNode = newOperatorCallNode(
      Operators.cast,
      code = castExpr.code,
      typeFullName = Some(typeFullName),
      line = line(castExpr),
      column = column(castExpr)
    )

    val typeNode = NewTypeRef()
      .code(nameFromNode(castExpr))
      .lineNumber(line(castExpr))
      .columnNumber(column(castExpr))
      .typeFullName(typeFullName)
    val typeAst = Ast(typeNode)

    // We can guarantee that there is an expression on the RHS
    val exprAst = astForExpression(createDotNetNodeInfo(castExpr.json(ParserKeys.Expression)))
    Seq(callAst(callNode, Seq(typeAst) ++ exprAst))
  }

  private def astForImplicitArrayCreationExpression(implArrExpr: DotNetNodeInfo): Seq[Ast] = {
    astForArrayInitializerExpression(createDotNetNodeInfo(implArrExpr.json(ParserKeys.Initializer)))
  }

  private def astForInterpolatedStringExpression(strExpr: DotNetNodeInfo): Seq[Ast] = {
    val contentAsts = strExpr
      .json(ParserKeys.Contents)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap { expr =>
        expr.node match {
          case InterpolatedStringText => astForInterpolatedStringText(expr)
          case Interpolation          => astForInterpolation(expr)
          case _                      => Nil
        }
      }
      .toSeq

    // Collect all the parts of the interpolated string, and concatenate them to form the full code
    val code = strExpr
      .json(ParserKeys.Contents)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap { node =>
        node.node match {
          case InterpolatedStringText =>
            Try(
              node
                .json(ParserKeys.TextToken)(ParserKeys.Value)
                .str
            ).toOption // Accessing node.json directly because DotNetNodeInfo contains stripped code, and does not contain braces
          case Interpolation => Try(node.json(ParserKeys.MetaData)(ParserKeys.Code).str).toOption
          case _             => None
        }
      }
      .mkString("")

    val _callNode = newOperatorCallNode(
      Operators.formatString,
      s"$$\"$code\"",
      Option(BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)),
      line(strExpr),
      column(strExpr)
    )

    Seq(callAst(_callNode, contentAsts))
  }

  private def astForInterpolation(interpolationExpr: DotNetNodeInfo): Seq[Ast] = {
    astForNode(interpolationExpr.json(ParserKeys.Expression))
  }

  private def astForInterpolatedStringText(interpolatedTextExpr: DotNetNodeInfo): Seq[Ast] = {
    Seq(
      Ast(
        literalNode(interpolatedTextExpr, code(interpolatedTextExpr), BuiltinTypes.DotNetTypeMap(BuiltinTypes.String))
      )
    )
  }

  private def astForConditionalAccessExpression(
    condAccExpr: DotNetNodeInfo,
    baseType: Option[String] = None
  ): Seq[Ast] = {
    val baseNode = createDotNetNodeInfo(condAccExpr.json(ParserKeys.Expression))
    val baseTypeFullName =
      baseType.orElse(Some(getTypeFullNameFromAstNode(astForNode(baseNode)))).filterNot(_.equals(Defines.Any))

    Try(createDotNetNodeInfo(condAccExpr.json(ParserKeys.WhenNotNull))).toOption match {
      case Some(node) =>
        node.node match {
          case ConditionalAccessExpression => astForConditionalAccessExpression(node, baseTypeFullName)
          case MemberBindingExpression     => astForMemberBindingExpression(node, baseTypeFullName)
          case _                           => astForNode(node)
        }
      case None => Seq.empty[Ast]
    }
  }

  private def astForSuppressNullableWarningExpression(suppressNullableExpr: DotNetNodeInfo): Seq[Ast] = {
    val _identifierNode = createDotNetNodeInfo(suppressNullableExpr.json(ParserKeys.Operand))
    Seq(astForIdentifier(_identifierNode))
  }

  private def astForMemberBindingExpression(
    memberBindingExpr: DotNetNodeInfo,
    baseTypeFullName: Option[String] = None
  ): Seq[Ast] = {
    val typ = scope
      .tryResolveFieldAccess(nameFromNode(memberBindingExpr), baseTypeFullName)
      .map(_.typeName)
      .map(f => scope.tryResolveTypeReference(f).map(_.name).orElse(Option(f)))
      .getOrElse(Option(Defines.Any))

    val fieldIdentifier = fieldIdentifierNode(memberBindingExpr, memberBindingExpr.code, memberBindingExpr.code)

    val identifier = newIdentifierNode(memberBindingExpr.code, baseTypeFullName.getOrElse(Defines.Any))
    val fieldAccess =
      newOperatorCallNode(
        Operators.fieldAccess,
        memberBindingExpr.code,
        typ,
        memberBindingExpr.lineNumber,
        memberBindingExpr.columnNumber
      )
    val fieldIdentifierAst = Ast(fieldIdentifier)

    Seq(callAst(fieldAccess, Seq(Ast(identifier)) ++ Seq(fieldIdentifierAst)))
  }

  protected def astForAttributeLists(attributeList: DotNetNodeInfo): Seq[Ast] = {
    attributeList.json(ParserKeys.Attributes).arr.map(createDotNetNodeInfo).map(astForAttribute).toSeq
  }

  private def astForAttribute(attribute: DotNetNodeInfo): Ast = {
    val attributeName = nameFromNode(attribute)
    val fullName      = nodeTypeFullName(attribute)
    val argumentAsts =
      Try(astForArgumentList(createDotNetNodeInfo(attribute.json(ParserKeys.ArgumentList)))).getOrElse(Seq.empty[Ast])

    val _annotationNode = annotationNode(attribute, attribute.code, attributeName, fullName)
    annotationAst(_annotationNode, argumentAsts)
  }

  /** Lowers a pattern expression into a condition and then a declaration if one occurs.
    * @param isPatternExpression
    *   a pattern expression which may include a declaration.
    * @return
    *   a condition and then (potentially) declaration.
    */
  protected def astsForIsPatternExpression(isPatternExpression: DotNetNodeInfo): List[Ast] = {
    val pattern = createDotNetNodeInfo(isPatternExpression.json(ParserKeys.Pattern))

    val expressionNode = createDotNetNodeInfo(isPatternExpression.json(ParserKeys.Expression))
    val expression     = astForExpression(expressionNode)

    pattern.node match {
      case DeclarationPattern =>
        val designation = createDotNetNodeInfo(pattern.json(ParserKeys.Designation))
        val typeInfo    = createDotNetNodeInfo(pattern.json(ParserKeys.Type))

        val instanceOfCallNode = newOperatorCallNode(
          Operators.instanceOf,
          code(pattern),
          Option(BuiltinTypes.Bool),
          line(expressionNode),
          column(expressionNode)
        )

        val assignmentAst = newOperatorCallNode(
          Operators.assignment,
          s"${typeInfo.code} ${designation.code} = ${expressionNode.code}",
          Option(nodeTypeFullName(typeInfo)),
          line(expressionNode),
          column(expressionNode)
        )

        val designationAst = astForIdentifier(designation, nodeTypeFullName(typeInfo))

        val typeNode = NewTypeRef()
          .code(nodeTypeFullName(typeInfo))
          .lineNumber(line(expressionNode))
          .columnNumber(column(expressionNode))
          .typeFullName(nodeTypeFullName(typeInfo))

        val conditionAst      = callAst(instanceOfCallNode, expression :+ Ast(typeNode))
        val assignmentCallAst = callAst(assignmentAst, designationAst +: expression)

        List(conditionAst, assignmentCallAst)
      case ConstantPattern =>
        val expr    = createDotNetNodeInfo(pattern.json(ParserKeys.Expression))
        val exprAst = astForExpression(expr)

        val typeFullName = nodeTypeFullName(expr)

        val equalCallNode =
          newOperatorCallNode(Operators.equals, code(pattern), Option(BuiltinTypes.Bool), line(expr), column(expr))
        val equalCallAst = callAst(equalCallNode, expression ++ exprAst)

        List(equalCallAst)
      case x =>
        logger.warn(s"Unsupported pattern in pattern expression, $x")
        astForExpression(pattern).toList
    }
  }

}
