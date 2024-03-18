package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.datastructures.CSharpMethod
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.utils.NodeBuilders.{newIdentifierNode, newOperatorCallNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFieldIdentifier, NewLiteral, NewTypeRef}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, Operators}
import ujson.Value

import scala.collection.mutable.ArrayBuffer
import scala.util.{Failure, Success, Try}
trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def astForExpressionStatement(expr: DotNetNodeInfo): Seq[Ast] = {
    val expressionNode = createDotNetNodeInfo(expr.json(ParserKeys.Expression))
    astForExpression(expressionNode)
  }

  def astForExpression(expr: DotNetNodeInfo): Seq[Ast] = {
    expr.node match
      case _: UnaryExpr                    => astForUnaryExpression(expr)
      case _: BinaryExpr                   => astForBinaryExpression(expr)
      case _: LiteralExpr                  => astForLiteralExpression(expr)
      case InvocationExpression            => astForInvocationExpression(expr)
      case AwaitExpression                 => astForAwaitExpression(expr)
      case ObjectCreationExpression        => astForObjectCreationExpression(expr)
      case SimpleMemberAccessExpression    => astForSimpleMemberAccessExpression(expr)
      case ImplicitArrayCreationExpression => astForImplicitArrayCreationExpression(expr)
      case ConditionalExpression           => astForConditionalExpression(expr)
      case _: IdentifierNode               => astForIdentifier(expr) :: Nil
      case ThisExpression                  => astForThisReceiver(expr) :: Nil
      case CastExpression                  => astForCastExpression(expr)
      case InterpolatedStringExpression    => astForInterpolatedStringExpression(expr)
      case ConditionalAccessExpression     => astForConditionalAccessExpression(expr)
      case _: BaseLambdaExpression         => astForSimpleLambdaExpression(expr)
      case _                               => notHandledYet(expr)
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
        List(scope.findFieldInScope(nameFromNode(operandNode)), scope.lookupVariable(nameFromNode(operandNode))) match {
          case List(Some(_), None) => astForSimpleMemberAccessExpression(operandNode)
          case _                   => astForNode(operandNode)
        }
      case _ => astForNode(operandNode)
    }
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

    val args    = createDotNetNodeInfo(unaryExpr.json(ParserKeys.Operand))
    val argsAst = astForOperand(args)

    Seq(
      callAst(createCallNodeForOperator(unaryExpr, operatorName, typeFullName = Some(nodeTypeFullName(args))), argsAst)
    )
  }

  protected def astForBinaryExpression(binaryExpr: DotNetNodeInfo): Seq[Ast] = {
    val operatorToken = binaryExpr.json(ParserKeys.OperatorToken)(ParserKeys.Value).str
    val operatorName = operatorToken match
      case "+"   => Operators.addition
      case "-"   => Operators.subtraction
      case "*"   => Operators.multiplication
      case "/"   => Operators.division
      case "%"   => Operators.modulo
      case "=="  => Operators.equals
      case "!="  => Operators.notEquals
      case "&&"  => Operators.logicalAnd
      case "||"  => Operators.logicalOr
      case "="   => Operators.assignment
      case "+="  => Operators.assignmentPlus
      case "-="  => Operators.assignmentMinus
      case "*="  => Operators.assignmentMultiplication
      case "/="  => Operators.assignmentDivision
      case "%="  => Operators.assignmentModulo
      case "&="  => Operators.assignmentAnd
      case "|="  => Operators.assignmentOr
      case "^="  => Operators.assignmentXor
      case ">>=" => Operators.assignmentLogicalShiftRight
      case "<<=" => Operators.assignmentShiftLeft
      case ">"   => Operators.greaterThan
      case "<"   => Operators.lessThan
      case ">="  => Operators.greaterEqualsThan
      case "<="  => Operators.lessEqualsThan
      case "|"   => Operators.or
      case "&"   => Operators.and
      case "^"   => Operators.xor
      case x =>
        logger.warn(s"Unhandled operator '$x' for ${code(binaryExpr)}")
        CSharpOperators.unknown

    val args = astForOperand(createDotNetNodeInfo(binaryExpr.json(ParserKeys.Left))) ++: astForOperand(
      createDotNetNodeInfo(binaryExpr.json(ParserKeys.Right))
    )

    val cNode =
      createCallNodeForOperator(
        binaryExpr,
        operatorName,
        typeFullName = Some(fixedTypeOperators.getOrElse(operatorName, getTypeFullNameFromAstNode(args)))
      )
    Seq(callAst(cNode, args))
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

  private def astForInvocationExpression(invocationExpr: DotNetNodeInfo): Seq[Ast] = {
    val expression   = createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression))
    val callName     = nameFromNode(expression)
    val argumentList = createDotNetNodeInfo(invocationExpr.json(ParserKeys.ArgumentList))

    val (
      receiver: Option[Ast],
      baseTypeFullName: Option[String],
      methodMetaData: Option[CSharpMethod],
      arguments: Seq[Ast]
    ) = expression.node match {
      case SimpleMemberAccessExpression =>
        val baseNode = createDotNetNodeInfo(
          createDotNetNodeInfo(invocationExpr.json(ParserKeys.Expression)).json(ParserKeys.Expression)
        )
        val receiverAst = astForNode(baseNode).toList
        val baseTypeFullName = receiverAst match {
          case head :: _ => Option(getTypeFullNameFromAstNode(head)).filterNot(_ == Defines.Any)
          case _         => None
        }
        val arguments      = astForArgumentList(argumentList, baseTypeFullName)
        val argTypes       = arguments.map(getTypeFullNameFromAstNode).toList
        val methodMetaData = scope.tryResolveMethodInvocation(callName, argTypes, baseTypeFullName)
        (receiverAst.headOption, baseTypeFullName, methodMetaData, arguments)
      case IdentifierName | MemberBindingExpression =>
        // This is when a call is made directly, which could also be made from a static import
        val argTypes = astForArgumentList(argumentList).map(getTypeFullNameFromAstNode).toList
        scope
          .tryResolveMethodInvocation(callName, argTypes)
          .orElse(scope.tryResolveMethodInvocation(callName, argTypes, scope.surroundingTypeDeclFullName)) match {
          case Some(methodMetaData) if methodMetaData.isStatic =>
            // If static, create implicit type identifier explicitly
            val typeMetaData = scope.typeForMethod(methodMetaData)
            val typeName     = typeMetaData.flatMap(_.name.split("[.]").lastOption).getOrElse(Defines.Any)
            val typeFullName = typeMetaData.map(_.name)
            val receiverNode = Ast(
              identifierNode(invocationExpr, typeName, typeName, typeFullName.getOrElse(Defines.Any))
            )
            val arguments = astForArgumentList(argumentList, typeFullName)
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
      case x =>
        logger.warn(s"Unhandled LHS $x for InvocationExpression")
        (None, None, None, Seq.empty[Ast])
    }
    val methodSignature = methodMetaData match {
      case Some(m) => s"${m.returnType}(${m.parameterTypes.filterNot(_._1 == "this").map(_._2).mkString(",")})"
      case None    => Defines.UnresolvedSignature
    }
    val methodFullName = baseTypeFullName match {
      case Some(typeFullName) =>
        s"$typeFullName.$callName:$methodSignature"
      case _ =>
        s"${Defines.UnresolvedNamespace}.$callName:$methodSignature"
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
      receiver
    )
    Seq(_callAst)
  }

  protected def astForSimpleMemberAccessExpression(accessExpr: DotNetNodeInfo): Seq[Ast] = {
    val fieldIdentifierName = nameFromNode(accessExpr)

    val fieldInScope = scope.findFieldInScope(fieldIdentifierName)

    val typeFullName = fieldInScope.map(_.typeFullName).getOrElse(Defines.Any)
    val identifierName =
      if (fieldInScope.nonEmpty && fieldInScope.get.isStatic) scope.surroundingTypeDeclFullName.getOrElse(Defines.Any)
      else "this"

    val identifier = newIdentifierNode(identifierName, typeFullName)

    val fieldIdentifier = NewFieldIdentifier()
      .code(fieldIdentifierName)
      .canonicalName(fieldIdentifierName)
      .lineNumber(accessExpr.lineNumber)
      .columnNumber(accessExpr.columnNumber)

    val fieldAccessCode = s"$identifierName.$fieldIdentifierName"

    val fieldAccess =
      newOperatorCallNode(
        Operators.fieldAccess,
        fieldAccessCode,
        Some(typeFullName).orElse(Some(Defines.Any)),
        accessExpr.lineNumber,
        accessExpr.columnNumber
      )

    val identifierAst = Ast(identifier)
    val fieldIdentAst = Ast(fieldIdentifier)

    Seq(callAst(fieldAccess, Seq(identifierAst, fieldIdentAst)))
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
        expr.node match
          case InterpolatedStringText => astForInterpolatedStringText(expr)
          case Interpolation          => astForInterpolation(expr)
      }
      .toSeq

    // Collect all the parts of the interpolated string, and concatenate them to form the full code
    val code = strExpr
      .json(ParserKeys.Contents)
      .arr
      .map(createDotNetNodeInfo)
      .map { node =>
        node.node match
          case InterpolatedStringText =>
            node
              .json(ParserKeys.TextToken)(ParserKeys.Value)
              .str // Accessing node.json directly because DotNetNodeInfo contains stripped code, and does not contain braces
          case Interpolation => node.json(ParserKeys.MetaData)(ParserKeys.Code).str
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

  private def astForConditionalAccessExpression(condAccExpr: DotNetNodeInfo): Seq[Ast] = {
    val baseNode = createDotNetNodeInfo(condAccExpr.json(ParserKeys.Expression))
    val baseAst  = astForNode(baseNode)

    val fieldIdentifier = fieldIdentifierNode(baseNode, baseNode.code, baseNode.code)

    val baseTypeFullName = getTypeFullNameFromAstNode(baseAst)

    Try(createDotNetNodeInfo(condAccExpr.json(ParserKeys.WhenNotNull)(ParserKeys.Name))).toOption match {
      case Some(node) =>
        // Got a member access
        val typ = scope
          .tryResolveFieldAccess(node.code, Option(baseTypeFullName))
          .map(_.typeName)
          .orElse(Option(Defines.Any))

        val identifier      = newIdentifierNode(baseNode.code, baseTypeFullName)
        val fieldAccessCode = s"${baseNode.code}?.${node.code}"
        val fieldAccess = newOperatorCallNode(
          Operators.fieldAccess,
          fieldAccessCode,
          typ,
          condAccExpr.lineNumber,
          condAccExpr.columnNumber
        )
        val fieldIdentifierAst = Ast(fieldIdentifier)

        Seq(callAst(fieldAccess, baseAst ++ Seq(fieldIdentifierAst)))
      case _ => astForInvocationExpression(createDotNetNodeInfo(condAccExpr.json(ParserKeys.WhenNotNull)))
    }
  }

}
