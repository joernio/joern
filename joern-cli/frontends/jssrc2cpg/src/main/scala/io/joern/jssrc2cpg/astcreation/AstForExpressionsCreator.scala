package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.{Defines, GlobalBuiltins}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.{NewFieldIdentifier, NewIdentifier, NewMethodRef, NewNode}

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForExpressionStatement(exprStmt: ExpressionStatement): Ast =
    astForNodeWithFunctionReference(exprStmt.expression)

  private val ImportCallName = "import"

  private type CallLike = CallExpression | OptionalCallExpression | ImportExpression | NewExpression

  private def argumentsOf(callLike: CallLike): Seq[BabelNode] = callLike match {
    case c: CallExpression         => c.arguments
    case c: OptionalCallExpression => c.arguments
    case c: ImportExpression       => c.source +: c.options.toSeq
    case c: NewExpression          => c.arguments
  }

  private def calleeOf(callLike: CallExpression | OptionalCallExpression | NewExpression): BabelNode = callLike match {
    case c: CallExpression         => c.callee
    case c: OptionalCallExpression => c.callee
    case c: NewExpression          => c.callee
  }

  /** `import(...)` is an `ImportExpression`, which has no callee node that could be resolved. */
  private def localCalleeOf(callLike: CallLike): Option[(String, String)] = callLike match {
    case _: ImportExpression                                          => None
    case c: (CallExpression | OptionalCallExpression | NewExpression) => resolveLocalCallee(calleeOf(c))
  }

  private def createBuiltinStaticCall(
    callExpr: CallExpression | OptionalCallExpression,
    callee: BabelNode,
    fullName: String
  ): Ast = {
    val callName = callee match {
      case member: MemberExpression => member.property.code
      case _                        => callee.code
    }
    val callNode = staticCallNode(callExpr.code, callName, fullName, callee.lineNumber, callee.columnNumber)
    val argAsts  = astForNodes(argumentsOf(callExpr))
    callAst(callNode, argAsts)
  }

  private case class CallExpressionInfo(receiverAst: Ast, baseNode: NewIdentifier, callName: String)

  /** If the callee is a function itself, e.g. closure, then resolve this locally, if possible. */
  private def resolveLocalCallee(callee: BabelNode): Option[(String, String)] = callee match {
    case function: FunctionLike => functionNodeToNameAndFullName.get(functionNodeKey(function))
    case _                      => None
  }

  private def handleCallNodeArgs(callExpr: CallLike, callExpressionInfo: CallExpressionInfo): Ast = {
    val args      = astForNodes(argumentsOf(callExpr))
    val callNode_ = callNode(callExpr, callExpr.code, callExpressionInfo.callName, DispatchTypes.DYNAMIC_DISPATCH)
    localCalleeOf(callExpr).foreach { case (name, fullName) =>
      callNode_.name(name).methodFullName(fullName)
    }
    callAst(
      callNode_,
      args,
      receiver = Option(callExpressionInfo.receiverAst),
      base = Option(Ast(callExpressionInfo.baseNode))
    )
  }

  private def callExpressionInfoForCallLikeExpr(callLike: BabelNode): CallExpressionInfo = {
    callLike match {
      case memberExpr: (MemberExpression | OptionalMemberExpression) =>
        val (base, member) = memberExpr match {
          case m: MemberExpression         => (m.`object`: BabelNode, m.property: BabelNode)
          case m: OptionalMemberExpression => (m.`object`: BabelNode, m.property: BabelNode)
        }
        base match {
          case _: ThisExpression =>
            val receiverAst = astForNodeWithFunctionReference(callLike)
            val baseNode    = identifierNode(base, base.code).dynamicTypeHintFullName(typeHintForThisExpression())
            scope.addVariableReference(base.code, baseNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            CallExpressionInfo(receiverAst, baseNode, member.code)
          case _: Identifier =>
            val receiverAst = astForNodeWithFunctionReference(callLike)
            val baseNode    = identifierNode(base, base.code)
            scope.addVariableReference(base.code, baseNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            CallExpressionInfo(receiverAst, baseNode, member.code)
          case _ =>
            val tmpVarName  = generateUnusedVariableName(usedVariableNames, "_tmp")
            val baseTmpNode = identifierNode(base, tmpVarName)
            scope.addVariableReference(tmpVarName, baseTmpNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            val baseAst          = astForNodeWithFunctionReference(base)
            val code             = s"(${codeOf(baseTmpNode)} = ${base.code})"
            val tmpAssignmentAst =
              createAssignmentCallAst(Ast(baseTmpNode), baseAst, code, base.lineNumber, base.columnNumber)
            val fieldName      = stripQuotes(member.code)
            val memberNode     = fieldIdentifierNode(member, fieldName, fieldName)
            val fieldAccessAst =
              createFieldAccessCallAst(tmpAssignmentAst, memberNode, callLike.lineNumber, callLike.columnNumber)
            val thisTmpNode = identifierNode(callLike, tmpVarName)
            scope.addVariableReference(tmpVarName, thisTmpNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
            CallExpressionInfo(fieldAccessAst, thisTmpNode, member.code)
        }
      case _ =>
        val callName = ejsOutputCallName(callLike).getOrElse(callLike.code)
        callExpressionInfoForReceiver(callLike, astForNodeWithFunctionReference(callLike), callName)
    }
  }

  /** A call on an arbitrary receiver, which is called on `this`; `at` provides the location of that `this`. */
  private def callExpressionInfoForReceiver(at: BabelNode, receiverAst: Ast, callName: String): CallExpressionInfo = {
    val thisNode = identifierNode(at, "this").dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    CallExpressionInfo(receiverAst, thisNode, callName)
  }

  protected def astForCallExpression(callExpr: CallExpression | OptionalCallExpression | ImportExpression): Ast =
    callExpr match {
      case importExpr: ImportExpression =>
        val receiverAst = Ast(identifierNode(importExpr, ImportCallName))
        handleCallNodeArgs(importExpr, callExpressionInfoForReceiver(importExpr, receiverAst, ImportCallName))
      case call: (CallExpression | OptionalCallExpression) =>
        val callee     = calleeOf(call)
        val calleeCode = callee.code
        if (GlobalBuiltins.builtins.contains(calleeCode)) {
          createBuiltinStaticCall(call, callee, calleeCode)
        } else {
          handleCallNodeArgs(call, callExpressionInfoForCallLikeExpr(callee))
        }
    }

  protected def astForThisExpression(thisExpr: ThisExpression): Ast = {
    val dynamicTypeOption = typeHintForThisExpression(Option(thisExpr)).headOption
    val thisNode = identifierNode(thisExpr, thisExpr.code, thisExpr.code, Defines.Any, dynamicTypeOption.toList)
    scope.addVariableReference(thisExpr.code, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    Ast(thisNode)
  }

  protected def astForNewExpression(newExpr: NewExpression): Ast = {
    val callee     = newExpr.callee
    val blockNode_ = blockNode(newExpr, newExpr.code, Defines.Any)

    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpAllocName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpAllocNode = localNode(newExpr, tmpAllocName, tmpAllocName, Defines.Any).order(0)
    val tmpAllocNode1     = identifierNode(newExpr, tmpAllocName)
    diffGraph.addEdge(localAstParentStack.head, localTmpAllocNode, EdgeTypes.AST)
    scope.addVariableReference(tmpAllocName, tmpAllocNode1, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val allocCallNode              = callNode(newExpr, ".alloc", Operators.alloc, DispatchTypes.STATIC_DISPATCH)
    val assignmentTmpAllocCallNode =
      createAssignmentCallAst(
        tmpAllocNode1,
        allocCallNode,
        s"$tmpAllocName = ${allocCallNode.code}",
        newExpr.lineNumber,
        newExpr.columnNumber
      )

    val tmpAllocNode2 = identifierNode(newExpr, tmpAllocName)
    val receiverNode  = astForNodeWithFunctionReference(callee)
    val callAst = handleCallNodeArgs(newExpr, CallExpressionInfo(receiverNode, tmpAllocNode2, Defines.OperatorsNew))
    val tmpAllocReturnNode = Ast(identifierNode(newExpr, tmpAllocName))

    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = List(assignmentTmpAllocCallNode, callAst, tmpAllocReturnNode)
    blockAst(blockNode_, blockChildren)
  }

  protected def astForMetaProperty(metaProperty: MetaProperty): Ast = {
    val metaAst   = astForIdentifier(metaProperty.meta)
    val member    = metaProperty.property
    val fieldName = stripQuotes(member.code)
    val memberAst = Ast(fieldIdentifierNode(member, fieldName, fieldName))
    createFieldAccessCallAst(metaAst, memberAst.nodes.head, metaProperty.lineNumber, metaProperty.columnNumber)
  }

  protected def astForMemberExpression(memberExpr: MemberExpression | OptionalMemberExpression): Ast = {
    val (baseNode, member, memberIsComputed, isOptional) = memberExpr match {
      case m: MemberExpression         => (m.`object`, m.property, m.computed, false)
      case m: OptionalMemberExpression => (m.`object`, m.property, m.computed, m.optional)
    }
    val baseAst = astForNodeWithFunctionReference(baseNode)
    if (memberIsComputed) {
      val memberAst = astForNode(member)
      createIndexAccessCallAst(baseAst, memberAst, memberExpr.lineNumber, memberExpr.columnNumber, isOptional)
    } else {
      val fieldName = stripQuotes(member.code)
      val memberAst = Ast(fieldIdentifierNode(member, fieldName, fieldName))
      createFieldAccessCallAst(
        baseAst,
        memberAst.nodes.head,
        memberExpr.lineNumber,
        memberExpr.columnNumber,
        isOptional
      )
    }
  }

  protected def astForAssignmentExpression(assignment: AssignmentExpression | AssignmentPattern): Ast = {
    val (op, left, right) = assignment match {
      case pattern: AssignmentPattern       => (Operators.assignment, pattern.left, pattern.right)
      case expression: AssignmentExpression =>
        val operator = expression.operator match {
          case "="    => Operators.assignment
          case "+="   => Operators.assignmentPlus
          case "-="   => Operators.assignmentMinus
          case "*="   => Operators.assignmentMultiplication
          case "/="   => Operators.assignmentDivision
          case "%="   => Operators.assignmentModulo
          case "**="  => Operators.assignmentExponentiation
          case "&="   => Operators.assignmentAnd
          case "&&="  => Operators.assignmentAnd
          case "|="   => Operators.assignmentOr
          case "||="  => Operators.assignmentOr
          case "^="   => Operators.assignmentXor
          case "<<="  => Operators.assignmentShiftLeft
          case ">>="  => Operators.assignmentArithmeticShiftRight
          case ">>>=" => Operators.assignmentLogicalShiftRight
          case "??="  => Operators.notNullAssert
          case other  =>
            logger.warn(s"Unknown assignment operator: '$other'")
            Operators.assignment
        }
        (operator, expression.left, expression.right)
    }

    left match {
      case pattern: (ObjectPattern | ArrayPattern) =>
        val rhsAst = astForNodeWithFunctionReference(right)
        astForDeconstruction(pattern, rhsAst, assignment.code)
      case _ =>
        val lhsAst    = astForNode(left)
        val rhsAst    = astForNodeWithFunctionReference(right)
        val callNode_ = callNode(assignment, assignment.code, op, DispatchTypes.STATIC_DISPATCH)
        val argAsts   = List(lhsAst, rhsAst)
        callAst(callNode_, argAsts)
    }
  }

  protected def astForConditionalExpression(ternary: ConditionalExpression): Ast = {
    val testAst       = astForNodeWithFunctionReference(ternary.test)
    val consequentAst = astForNodeWithFunctionReference(ternary.consequent)
    val alternateAst  = astForNodeWithFunctionReference(ternary.alternate)
    createTernaryCallAst(testAst, consequentAst, alternateAst, ternary.lineNumber, ternary.columnNumber)
  }

  protected def astForLogicalExpression(logicalExpr: LogicalExpression): Ast =
    astForBinaryExpression(logicalExpr)

  protected def astForTSNonNullExpression(nonNullExpr: TSNonNullExpression): Ast = {
    val op        = Operators.notNullAssert
    val callNode_ = callNode(nonNullExpr, nonNullExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts   = List(astForNodeWithFunctionReference(nonNullExpr.expression))
    callAst(callNode_, argAsts)
  }

  protected def astForCastExpression(
    castExpr: TypeCastExpression | TSTypeAssertion | TSAsExpression | TSTypeCastExpression
  ): Ast = {
    val op                                = Operators.cast
    val castParts: (BabelNode, BabelNode) = castExpr match {
      case c: TypeCastExpression   => (c.typeAnnotation, c.expression)
      case c: TSTypeAssertion      => (c.typeAnnotation, c.expression)
      case c: TSAsExpression       => (c.typeAnnotation, c.expression)
      case c: TSTypeCastExpression => (c.typeAnnotation, c.expression)
    }
    val (lhsNode, rhsNode) = castParts
    val rhsAst             = astForNodeWithFunctionReference(rhsNode)
    val possibleTypes      = Seq(typeFor(castExpr))
    val lhsAst             = Ast(literalNode(castExpr, lhsNode.code, None).possibleTypes(possibleTypes))
    val node    = callNode(castExpr, castExpr.code, op, DispatchTypes.STATIC_DISPATCH).possibleTypes(possibleTypes)
    val argAsts = List(lhsAst, rhsAst)
    callAst(node, argAsts)
  }

  protected def astForBinaryExpression(binExpr: BinaryExpression | LogicalExpression): Ast = {
    val binParts: (String, BabelNode, BabelNode) = binExpr match {
      case b: BinaryExpression  => (b.operator, b.left, b.right)
      case b: LogicalExpression => (b.operator, b.left, b.right)
    }
    val (operator, left, right) = binParts
    val op                      = operator match {
      case "+"          => Operators.addition
      case "-"          => Operators.subtraction
      case "/"          => Operators.division
      case "%"          => Operators.modulo
      case "*"          => Operators.multiplication
      case "**"         => Operators.exponentiation
      case "&"          => Operators.and
      case ">>"         => Operators.arithmeticShiftRight
      case ">>>"        => Operators.logicalShiftRight
      case "<<"         => Operators.shiftLeft
      case "^"          => Operators.xor
      case "=="         => Operators.equals
      case "==="        => Operators.equals
      case "!="         => Operators.notEquals
      case "!=="        => Operators.notEquals
      case "in"         => Operators.in
      case ">"          => Operators.greaterThan
      case "<"          => Operators.lessThan
      case ">="         => Operators.greaterEqualsThan
      case "<="         => Operators.lessEqualsThan
      case "instanceof" => Operators.instanceOf
      case "||"         => Operators.logicalOr
      case "|"          => Operators.or
      case "&&"         => Operators.logicalAnd
      // special case (see: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Nullish_coalescing_operator)
      case "??"   => Operators.logicalOr
      case "case" => "<operator>.case"
      case other  =>
        logger.warn(s"Unknown binary operator: '$other'")
        Operators.assignment
    }

    val lhsAst = astForNodeWithFunctionReference(left)
    val rhsAst = astForNodeWithFunctionReference(right)

    val node =
      callNode(binExpr, binExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(lhsAst, rhsAst)
    callAst(node, argAsts)
  }

  protected def astForUpdateExpression(updateExpr: UpdateExpression): Ast = {
    val isPrefix = updateExpr.prefix
    val op       = updateExpr.operator match {
      case "++" if isPrefix => Operators.preIncrement
      case "++"             => Operators.postIncrement
      case "--" if isPrefix => Operators.preIncrement
      case "--"             => Operators.postIncrement
      case other            =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAst = astForNodeWithFunctionReference(updateExpr.argument)

    val node    = callNode(updateExpr, updateExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(argumentAst)
    callAst(node, argAsts)
  }

  protected def astForUnaryExpression(unaryExpr: UnaryExpression): Ast = {
    val argumentAst = astForNodeWithFunctionReference(unaryExpr.argument)
    unaryExpr.operator match {
      case "throw" => throwAst(unaryExpr, List(argumentAst))
      case op      =>
        val operator = op match {
          case "void"   => "<operator>.void"
          case "delete" => Operators.delete
          case "!"      => Operators.logicalNot
          case "+"      => Operators.plus
          case "-"      => Operators.minus
          case "~"      => "<operator>.bitNot"
          case "typeof" => Operators.instanceOf
          case other    =>
            logger.warn(s"Unknown update operator: '$other'")
            Operators.assignment
        }
        callAst(callNode(unaryExpr, unaryExpr.code, operator, DispatchTypes.STATIC_DISPATCH), List(argumentAst))
    }
  }

  protected def astForSequenceExpression(seq: SequenceExpression): Ast = {
    val blockNode_ = blockNode(seq, seq.code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.expressions)
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode_, sequenceExpressionAsts)
  }

  protected def astForAwaitExpression(awaitExpr: AwaitExpression): Ast = {
    val node    = callNode(awaitExpr, awaitExpr.code, "<operator>.await", DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(astForNodeWithFunctionReference(awaitExpr.argument))
    callAst(node, argAsts)
  }

  protected def astForArrayExpression(arrExpr: ArrayExpression | ArrayPattern): Ast =
    arrExpr match {
      case a: ArrayExpression => astForArrayElements(a, a.elements)
      case a: ArrayPattern    => astForArrayElements(a, a.elements)
    }

  private def astForArrayElements(arrExpr: BabelNode, allElements: Seq[Option[BabelNode]]): Ast = {
    val MAX_INITIALIZERS = 1000
    val elements         = allElements.slice(0, MAX_INITIALIZERS)
    if (elements.isEmpty) {
      Ast(
        callNode(arrExpr, s"${EcmaBuiltins.arrayFactory}()", EcmaBuiltins.arrayFactory, DispatchTypes.STATIC_DISPATCH)
      )
    } else {
      val blockNode_ = blockNode(arrExpr, arrExpr.code, Defines.Any)
      scope.pushNewBlockScope(blockNode_)
      localAstParentStack.push(blockNode_)

      val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
      val localTmpNode = localNode(arrExpr, tmpName, tmpName, Defines.Any).order(0)
      val tmpArrayNode = identifierNode(arrExpr, tmpName)
      diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
      scope.addVariableReference(tmpName, tmpArrayNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

      val arrayCallNode =
        callNode(arrExpr, s"${EcmaBuiltins.arrayFactory}()", EcmaBuiltins.arrayFactory, DispatchTypes.STATIC_DISPATCH)

      val lineNumber                 = arrExpr.lineNumber
      val columnNumber               = arrExpr.columnNumber
      val assignmentCode             = s"${localTmpNode.code} = ${arrayCallNode.code}"
      val assignmentTmpArrayCallNode =
        createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

      val elementAsts = elements.flatMap {
        case Some(element) =>
          val elementLineNumber   = element.lineNumber
          val elementColumnNumber = element.columnNumber
          val elementNode         = element match {
            case rest: RestElement =>
              val arg1Ast = Ast(identifierNode(arrExpr, tmpName))
              astForSpreadOrRestElement(rest, Option(arg1Ast))
            case _ =>
              astForNodeWithFunctionReference(element)
          }

          val elementCode  = elementNode.root.map(codeOf).getOrElse(element.code)
          val pushCallNode =
            callNode(element, s"$tmpName.push($elementCode)", "", DispatchTypes.DYNAMIC_DISPATCH)

          val baseNode     = identifierNode(element, tmpName)
          val memberNode   = fieldIdentifierNode(element, "push", "push")
          val receiverNode = createFieldAccessCallAst(baseNode, memberNode, elementLineNumber, elementColumnNumber)
          val thisPushNode = identifierNode(element, tmpName)

          Option(
            callAst(pushCallNode, List(elementNode), receiver = Option(receiverNode), base = Option(Ast(thisPushNode)))
          )
        case None => None // skip
      }

      val tmpArrayReturnNode = identifierNode(arrExpr, tmpName)

      scope.popScope()
      localAstParentStack.pop()

      val blockChildrenAsts = if (allElements.sizeIs > MAX_INITIALIZERS) {
        val placeholder = literalNode(arrExpr, "<too-many-initializers>", Defines.Any)
        assignmentTmpArrayCallNode +: elementAsts :+ Ast(placeholder) :+ Ast(tmpArrayReturnNode)
      } else { assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode) }
      blockAst(blockNode_, blockChildrenAsts.toList)
    }
  }

  private def handleTemplateExpressionArgs(
    templateExpr: TaggedTemplateExpression,
    callExpressionInfo: CallExpressionInfo
  ): Ast = {
    val quasi          = templateExpr.quasi
    val expressionArgs = quasi.expressions.map(astForNodeWithFunctionReference)
    val quasisArg      = astForArrayElements(quasi, quasi.quasis.map(Option(_)))
    val callNode_      =
      callNode(templateExpr, templateExpr.code, callExpressionInfo.callName, DispatchTypes.DYNAMIC_DISPATCH)
    callAst(
      callNode_,
      quasisArg +: expressionArgs,
      receiver = Option(callExpressionInfo.receiverAst),
      base = Option(Ast(callExpressionInfo.baseNode))
    )
  }

  /** Lowering from expressions like, x`a ${1+1} b` to x(["a ", " b"], 1+1)
    */
  def astForTemplateExpression(templateExpr: TaggedTemplateExpression): Ast = {
    val callExpressionInfo = callExpressionInfoForCallLikeExpr(templateExpr.tag)
    handleTemplateExpressionArgs(templateExpr, callExpressionInfo)
  }

  protected def astForObjectExpression(objExpr: ObjectExpression | ObjectPattern): Ast = {
    val blockNode_ = blockNode(objExpr, objExpr.code, Defines.Any)

    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(objExpr, tmpName, tmpName, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)

    val properties: Seq[ObjectMethod | ObjectProperty | SpreadElement | RestElement] = objExpr match {
      case o: ObjectExpression => o.properties
      case o: ObjectPattern    => o.properties
    }
    val propertiesAsts = properties.map {
      case property @ (_: SpreadElement | _: RestElement) =>
        val arg1Ast = Ast(identifierNode(property, tmpName))
        astForSpreadOrRestElement(property, Option(arg1Ast))
      case property: (ObjectMethod | ObjectProperty) =>
        val (lhsAst, rhsAst) = property match {
          case objectMethod: ObjectMethod =>
            val objectMethodAst = astForFunctionDeclaration(objectMethod, shouldCreateFunctionReference = true)
            val keyName         = objectMethod.key match {
              case id: Identifier => id.name
              case other          => other.code
            }
            val keyAst = objectMethodAst.root match {
              case Some(_: NewMethodRef) if !objectMethod.key.isInstanceOf[Identifier] =>
                Ast(literalNode(property, keyName, keyName))
              case _ =>
                val fieldName = stripQuotes(keyName)
                Ast(fieldIdentifierNode(property, fieldName, fieldName))
            }
            (keyAst, objectMethodAst)
          case objectProperty: ObjectProperty =>
            val keyAst =
              if (objectProperty.computed) {
                astForNode(objectProperty.key)
              } else {
                val fieldName = stripQuotes(objectProperty.key.code)
                Ast(fieldIdentifierNode(property, fieldName, fieldName))
              }
            val ast = astForNodeWithFunctionReference(objectProperty.value)
            (keyAst, ast)
        }

        val leftHandSideTmpNode   = identifierNode(property, tmpName)
        val leftHandSideAccessAst = lhsAst.root match {
          case Some(f: NewFieldIdentifier) =>
            createFieldAccessCallAst(leftHandSideTmpNode, f, property.lineNumber, property.columnNumber)
          case _ =>
            createIndexAccessCallAst(Ast(leftHandSideTmpNode), lhsAst, property.lineNumber, property.columnNumber)
        }

        createAssignmentCallAst(
          leftHandSideAccessAst,
          rhsAst,
          s"${codeOf(leftHandSideAccessAst.nodes.head)} = ${codeOf(rhsAst.nodes.head)}",
          property.lineNumber,
          property.columnNumber
        )
    }

    val tmpNode = identifierNode(objExpr, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val childrenAsts = (propertiesAsts.iterator ++ Iterator.single(Ast(tmpNode))).toList
    blockAst(blockNode_, childrenAsts)
  }

  protected def astForTSSatisfiesExpression(satisfiesExpr: TSSatisfiesExpression): Ast = {
    // Ignores the type, i.e. `x satisfies T` is understood as `x`.
    astForNode(satisfiesExpr.expression)
  }
}
