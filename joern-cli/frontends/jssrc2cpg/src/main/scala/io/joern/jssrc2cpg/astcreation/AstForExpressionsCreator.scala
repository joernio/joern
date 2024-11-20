package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.x2cpg.Ast
import io.joern.x2cpg.ValidationMode
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.frontendspecific.jssrc2cpg.GlobalBuiltins
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewIdentifier
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.util.Try

trait AstForExpressionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForExpressionStatement(exprStmt: BabelNodeInfo): Ast =
    astForNodeWithFunctionReference(exprStmt.json("expression"))

  private def createBuiltinStaticCall(callExpr: BabelNodeInfo, callee: BabelNodeInfo, fullName: String): Ast = {
    val callName = callee.node match {
      case MemberExpression => code(callee.json("property"))
      case _                => callee.code
    }
    val callNode = createStaticCallNode(callExpr.code, callName, fullName, callee.lineNumber, callee.columnNumber)
    val argAsts  = astForNodes(callExpr.json("arguments").arr.toList)
    callAst(callNode, argAsts)
  }

  private case class CallExpressionInfo(receiverAst: Ast, baseNode: NewIdentifier, callName: String)

  private def handleCallNodeArgs(callExpr: BabelNodeInfo, callExpressionInfo: CallExpressionInfo): Ast = {
    val args      = astForNodes(callExpr.json("arguments").arr.toList)
    val callNode_ = callNode(callExpr, callExpr.code, callExpressionInfo.callName, DispatchTypes.DYNAMIC_DISPATCH)
    // If the callee is a function itself, e.g. closure, then resolve this locally, if possible
    callExpr.json.obj
      .get("callee")
      .map(createBabelNodeInfo)
      .flatMap {
        case callee if callee.node.isInstanceOf[FunctionLike] => functionNodeToNameAndFullName.get(callee)
        case _                                                => None
      }
      .foreach { case (name, fullName) => callNode_.name(name).methodFullName(fullName) }
    callAst(
      callNode_,
      args,
      receiver = Option(callExpressionInfo.receiverAst),
      base = Option(Ast(callExpressionInfo.baseNode))
    )
  }

  private def callExpressionInfoForCallLikeExpr(callLike: BabelNodeInfo): CallExpressionInfo = {
    callLike.node match {
      case MemberExpression =>
        val base   = createBabelNodeInfo(callLike.json("object"))
        val member = createBabelNodeInfo(callLike.json("property"))
        base.node match {
          case ThisExpression =>
            val receiverAst = astForNodeWithFunctionReference(callLike.json)
            val baseNode    = identifierNode(base, base.code).dynamicTypeHintFullName(typeHintForThisExpression())
            scope.addVariableReference(base.code, baseNode)
            CallExpressionInfo(receiverAst, baseNode, member.code)
          case Identifier =>
            val receiverAst = astForNodeWithFunctionReference(callLike.json)
            val baseNode    = identifierNode(base, base.code)
            scope.addVariableReference(base.code, baseNode)
            CallExpressionInfo(receiverAst, baseNode, member.code)
          case _ =>
            val tmpVarName  = generateUnusedVariableName(usedVariableNames, "_tmp")
            val baseTmpNode = identifierNode(base, tmpVarName)
            scope.addVariableReference(tmpVarName, baseTmpNode)
            val baseAst = astForNodeWithFunctionReference(base.json)
            val code    = s"(${codeOf(baseTmpNode)} = ${base.code})"
            val tmpAssignmentAst =
              createAssignmentCallAst(Ast(baseTmpNode), baseAst, code, base.lineNumber, base.columnNumber)
            val memberNode = createFieldIdentifierNode(member.code, member.lineNumber, member.columnNumber)
            val fieldAccessAst =
              createFieldAccessCallAst(tmpAssignmentAst, memberNode, callLike.lineNumber, callLike.columnNumber)
            val thisTmpNode = identifierNode(callLike, tmpVarName)
            scope.addVariableReference(tmpVarName, thisTmpNode)
            CallExpressionInfo(fieldAccessAst, thisTmpNode, member.code)
        }
      case _ =>
        val receiverAst = astForNodeWithFunctionReference(callLike.json)
        val thisNode    = identifierNode(callLike, "this").dynamicTypeHintFullName(typeHintForThisExpression())
        scope.addVariableReference(thisNode.name, thisNode)
        CallExpressionInfo(receiverAst, thisNode, callLike.code)
    }
  }

  protected def astForCallExpression(callExpr: BabelNodeInfo): Ast = {
    val callee     = createBabelNodeInfo(callExpr.json("callee"))
    val calleeCode = callee.code
    if (GlobalBuiltins.builtins.contains(calleeCode)) {
      createBuiltinStaticCall(callExpr, callee, calleeCode)
    } else {
      val callExpressionInfo = callExpressionInfoForCallLikeExpr(callee)
      handleCallNodeArgs(callExpr, callExpressionInfo)
    }
  }

  protected def astForThisExpression(thisExpr: BabelNodeInfo): Ast = {
    val dynamicTypeOption = typeHintForThisExpression(Option(thisExpr)).headOption
    val thisNode          = identifierNode(thisExpr, thisExpr.code, dynamicTypeOption.toList)
    scope.addVariableReference(thisExpr.code, thisNode)
    Ast(thisNode)
  }

  protected def astForNewExpression(newExpr: BabelNodeInfo): Ast = {
    val callee    = newExpr.json("callee")
    val blockNode = createBlockNode(newExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpAllocName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpAllocNode = localNode(newExpr, tmpAllocName, tmpAllocName, Defines.Any).order(0)
    val tmpAllocNode1     = identifierNode(newExpr, tmpAllocName)
    diffGraph.addEdge(localAstParentStack.head, localTmpAllocNode, EdgeTypes.AST)
    scope.addVariableReference(tmpAllocName, tmpAllocNode1)

    val allocCallNode = callNode(newExpr, ".alloc", Operators.alloc, DispatchTypes.STATIC_DISPATCH)
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
    setArgumentIndices(blockChildren)
    blockAst(blockNode, blockChildren)
  }

  protected def astForMetaProperty(metaProperty: BabelNodeInfo): Ast = {
    val metaAst        = astForIdentifier(createBabelNodeInfo(metaProperty.json("meta")))
    val memberNodeInfo = createBabelNodeInfo(metaProperty.json("property"))
    val memberAst = Ast(
      createFieldIdentifierNode(memberNodeInfo.code, memberNodeInfo.lineNumber, memberNodeInfo.columnNumber)
    )
    createFieldAccessCallAst(metaAst, memberAst.nodes.head, metaProperty.lineNumber, metaProperty.columnNumber)
  }

  protected def astForMemberExpression(memberExpr: BabelNodeInfo): Ast = {
    val baseAst          = astForNodeWithFunctionReference(memberExpr.json("object"))
    val memberIsComputed = memberExpr.json("computed").bool
    val memberNodeInfo   = createBabelNodeInfo(memberExpr.json("property"))
    if (memberIsComputed) {
      val memberAst = astForNode(memberNodeInfo.json)
      createIndexAccessCallAst(baseAst, memberAst, memberExpr.lineNumber, memberExpr.columnNumber)
    } else {
      val memberAst = Ast(
        createFieldIdentifierNode(memberNodeInfo.code, memberNodeInfo.lineNumber, memberNodeInfo.columnNumber)
      )
      createFieldAccessCallAst(baseAst, memberAst.nodes.head, memberExpr.lineNumber, memberExpr.columnNumber)
    }
  }

  protected def astForAssignmentExpression(assignment: BabelNodeInfo): Ast = {
    val op = if (hasKey(assignment.json, "operator")) {
      assignment.json("operator").str match {
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
        case other =>
          logger.warn(s"Unknown assignment operator: '$other'")
          Operators.assignment
      }
    } else Operators.assignment

    val nodeInfo = createBabelNodeInfo(assignment.json("left"))
    nodeInfo.node match {
      case ObjectPattern | ArrayPattern =>
        val rhsAst = astForNodeWithFunctionReference(assignment.json("right"))
        astForDeconstruction(nodeInfo, rhsAst, assignment.code)
      case _ =>
        val lhsAst = astForNode(assignment.json("left"))
        val rhsAst = astForNodeWithFunctionReference(assignment.json("right"))
        val callNode_ =
          callNode(assignment, assignment.code, op, DispatchTypes.STATIC_DISPATCH)
        val argAsts = List(lhsAst, rhsAst)
        callAst(callNode_, argAsts)
    }
  }

  protected def astForConditionalExpression(ternary: BabelNodeInfo): Ast = {
    val testAst       = astForNodeWithFunctionReference(ternary.json("test"))
    val consequentAst = astForNodeWithFunctionReference(ternary.json("consequent"))
    val alternateAst  = astForNodeWithFunctionReference(ternary.json("alternate"))
    createTernaryCallAst(testAst, consequentAst, alternateAst, ternary.lineNumber, ternary.columnNumber)
  }

  protected def astForLogicalExpression(logicalExpr: BabelNodeInfo): Ast =
    astForBinaryExpression(logicalExpr)

  protected def astForTSNonNullExpression(nonNullExpr: BabelNodeInfo): Ast = {
    val op        = Operators.notNullAssert
    val callNode_ = callNode(nonNullExpr, nonNullExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts   = List(astForNodeWithFunctionReference(nonNullExpr.json("expression")))
    callAst(callNode_, argAsts)
  }

  protected def astForCastExpression(castExpr: BabelNodeInfo): Ast = {
    val op            = Operators.cast
    val lhsNode       = castExpr.json("typeAnnotation")
    val rhsAst        = astForNodeWithFunctionReference(castExpr.json("expression"))
    val possibleTypes = Seq(typeFor(castExpr))
    val lhsAst        = Ast(literalNode(castExpr, code(lhsNode), None).possibleTypes(possibleTypes))
    val node    = callNode(castExpr, castExpr.code, op, DispatchTypes.STATIC_DISPATCH).possibleTypes(possibleTypes)
    val argAsts = List(lhsAst, rhsAst)
    callAst(node, argAsts)
  }

  protected def astForBinaryExpression(binExpr: BabelNodeInfo): Ast = {
    val op = binExpr.json("operator").str match {
      case "+"          => Operators.addition
      case "-"          => Operators.subtraction
      case "/"          => Operators.division
      case "%"          => Operators.modulo
      case "*"          => Operators.multiplication
      case "**"         => Operators.exponentiation
      case "&"          => Operators.and
      case ">>"         => Operators.arithmeticShiftRight
      case ">>>"        => Operators.arithmeticShiftRight
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
      case other =>
        logger.warn(s"Unknown binary operator: '$other'")
        Operators.assignment
    }

    val lhsAst = astForNodeWithFunctionReference(binExpr.json("left"))
    val rhsAst = astForNodeWithFunctionReference(binExpr.json("right"))

    val node =
      callNode(binExpr, binExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(lhsAst, rhsAst)
    callAst(node, argAsts)
  }

  protected def astForUpdateExpression(updateExpr: BabelNodeInfo): Ast = {
    val isPrefix = updateExpr.json("prefix").bool
    val op = updateExpr.json("operator").str match {
      case "++" if isPrefix => Operators.preIncrement
      case "++"             => Operators.postIncrement
      case "--" if isPrefix => Operators.preIncrement
      case "--"             => Operators.postIncrement
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAst = astForNodeWithFunctionReference(updateExpr.json("argument"))

    val node    = callNode(updateExpr, updateExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(argumentAst)
    callAst(node, argAsts)
  }

  protected def astForUnaryExpression(unaryExpr: BabelNodeInfo): Ast = {
    val op = unaryExpr.json("operator").str match {
      case "void"   => "<operator>.void"
      case "throw"  => "<operator>.throw"
      case "delete" => Operators.delete
      case "!"      => Operators.logicalNot
      case "+"      => Operators.plus
      case "-"      => Operators.minus
      case "~"      => "<operator>.bitNot"
      case "typeof" => Operators.instanceOf
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAst = astForNodeWithFunctionReference(unaryExpr.json("argument"))

    val node    = callNode(unaryExpr, unaryExpr.code, op, DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(argumentAst)
    callAst(node, argAsts)
  }

  protected def astForSequenceExpression(seq: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(seq)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.json("expressions"))
    setArgumentIndices(sequenceExpressionAsts)
    localAstParentStack.pop()
    scope.popScope()
    blockAst(blockNode, sequenceExpressionAsts)
  }

  protected def astForAwaitExpression(awaitExpr: BabelNodeInfo): Ast = {
    val node    = callNode(awaitExpr, awaitExpr.code, "<operator>.await", DispatchTypes.STATIC_DISPATCH)
    val argAsts = List(astForNodeWithFunctionReference(awaitExpr.json("argument")))
    callAst(node, argAsts)
  }

  protected def astForArrayExpression(arrExpr: BabelNodeInfo, elementsKey: String = "elements"): Ast = {
    val MAX_INITIALIZERS = 1000
    val elementsJsons    = Try(arrExpr.json(elementsKey).arr).toOption.toList.flatten
    val elements         = elementsJsons.slice(0, MAX_INITIALIZERS)
    if (elements.isEmpty) {
      Ast(
        callNode(arrExpr, s"${EcmaBuiltins.arrayFactory}()", EcmaBuiltins.arrayFactory, DispatchTypes.STATIC_DISPATCH)
      )
    } else {
      val blockNode = createBlockNode(arrExpr)
      scope.pushNewBlockScope(blockNode)
      localAstParentStack.push(blockNode)

      val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
      val localTmpNode = localNode(arrExpr, tmpName, tmpName, Defines.Any).order(0)
      val tmpArrayNode = identifierNode(arrExpr, tmpName)
      diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
      scope.addVariableReference(tmpName, tmpArrayNode)

      val arrayCallNode =
        callNode(arrExpr, s"${EcmaBuiltins.arrayFactory}()", EcmaBuiltins.arrayFactory, DispatchTypes.STATIC_DISPATCH)

      val lineNumber     = arrExpr.lineNumber
      val columnNumber   = arrExpr.columnNumber
      val assignmentCode = s"${localTmpNode.code} = ${arrayCallNode.code}"
      val assignmentTmpArrayCallNode =
        createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

      val elementAsts = elements.flatMap {
        case element if !element.isNull =>
          val elementNodeInfo     = createBabelNodeInfo(element)
          val elementLineNumber   = elementNodeInfo.lineNumber
          val elementColumnNumber = elementNodeInfo.columnNumber
          val elementNode = elementNodeInfo.node match {
            case RestElement =>
              val arg1Ast = Ast(identifierNode(arrExpr, tmpName))
              astForSpreadOrRestElement(elementNodeInfo, Option(arg1Ast))
            case _ =>
              astForNodeWithFunctionReference(element)
          }

          val elementCode = elementNode.root.map(codeOf).getOrElse(elementNodeInfo.code)
          val pushCallNode =
            callNode(elementNodeInfo, s"$tmpName.push($elementCode)", "", DispatchTypes.DYNAMIC_DISPATCH)

          val baseNode     = identifierNode(elementNodeInfo, tmpName)
          val memberNode   = createFieldIdentifierNode("push", elementLineNumber, elementColumnNumber)
          val receiverNode = createFieldAccessCallAst(baseNode, memberNode, elementLineNumber, elementColumnNumber)
          val thisPushNode = identifierNode(elementNodeInfo, tmpName)

          Option(
            callAst(pushCallNode, List(elementNode), receiver = Option(receiverNode), base = Option(Ast(thisPushNode)))
          )
        case _ => None // skip
      }

      val tmpArrayReturnNode = identifierNode(arrExpr, tmpName)

      scope.popScope()
      localAstParentStack.pop()

      val blockChildrenAsts = if (elementsJsons.sizeIs > MAX_INITIALIZERS) {
        val placeholder = literalNode(arrExpr, "<too-many-initializers>", Defines.Any)
        assignmentTmpArrayCallNode +: elementAsts :+ Ast(placeholder) :+ Ast(tmpArrayReturnNode)
      } else { assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode) }
      setArgumentIndices(blockChildrenAsts)
      blockAst(blockNode, blockChildrenAsts)
    }
  }

  private def handleTemplateExpressionArgs(templateExpr: BabelNodeInfo, callExpressionInfo: CallExpressionInfo): Ast = {
    val expressionArgs = templateExpr.json("quasi")("expressions").arr.toList.map(astForNodeWithFunctionReference)
    val quasisArg      = astForArrayExpression(createBabelNodeInfo(templateExpr.json("quasi")), "quasis")
    val callNode_ =
      callNode(templateExpr, templateExpr.code, callExpressionInfo.callName, DispatchTypes.DYNAMIC_DISPATCH)
    // If the callee is a function itself, e.g. closure, then resolve this locally, if possible
    templateExpr.json.obj
      .get("callee")
      .map(createBabelNodeInfo)
      .flatMap {
        case callee if callee.node.isInstanceOf[FunctionLike] => functionNodeToNameAndFullName.get(callee)
        case _                                                => None
      }
      .foreach { case (name, fullName) => callNode_.name(name).methodFullName(fullName) }
    callAst(
      callNode_,
      quasisArg +: expressionArgs,
      receiver = Option(callExpressionInfo.receiverAst),
      base = Option(Ast(callExpressionInfo.baseNode))
    )
  }

  /** Lowering from expressions like, x`a ${1+1} b` to x(["a ", " b"], 1+1)
    */
  def astForTemplateExpression(templateExpr: BabelNodeInfo): Ast = {
    val callee             = createBabelNodeInfo(templateExpr.json("tag"))
    val callExpressionInfo = callExpressionInfoForCallLikeExpr(callee)
    handleTemplateExpressionArgs(templateExpr, callExpressionInfo)
  }

  protected def astForObjectExpression(objExpr: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(objExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(objExpr, tmpName, tmpName, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)

    val propertiesAsts = objExpr.json("properties").arr.toList.map { property =>
      val nodeInfo = createBabelNodeInfo(property)
      nodeInfo.node match {
        case SpreadElement | RestElement =>
          val arg1Ast = Ast(identifierNode(nodeInfo, tmpName))
          astForSpreadOrRestElement(nodeInfo, Option(arg1Ast))
        case _ =>
          val (lhsNode, rhsAst) = nodeInfo.node match {
            case ObjectMethod =>
              val keyName =
                if (hasKey(nodeInfo.json("key"), "name")) nodeInfo.json("key")("name").str
                else code(nodeInfo.json("key"))
              val keyNode = createFieldIdentifierNode(keyName, nodeInfo.lineNumber, nodeInfo.columnNumber)
              (keyNode, astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true))
            case ObjectProperty =>
              val key = createBabelNodeInfo(nodeInfo.json("key"))
              val keyName = key.node match {
                case Identifier if nodeInfo.json("computed").bool =>
                  key.code
                case _ if nodeInfo.json("computed").bool =>
                  generateUnusedVariableName(usedVariableNames, "_computed_object_property")
                case _ => key.code
              }
              val keyNode = createFieldIdentifierNode(keyName, nodeInfo.lineNumber, nodeInfo.columnNumber)
              val ast     = astForNodeWithFunctionReference(nodeInfo.json("value"))
              (keyNode, ast)
            case _ =>
              // can't happen as per https://github.com/babel/babel/blob/main/packages/babel-types/src/ast-types/generated/index.ts#L573
              // just to make the compiler happy here.
              ???
          }

          val leftHandSideTmpNode = identifierNode(nodeInfo, tmpName)
          val leftHandSideFieldAccessAst =
            createFieldAccessCallAst(leftHandSideTmpNode, lhsNode, nodeInfo.lineNumber, nodeInfo.columnNumber)

          createAssignmentCallAst(
            leftHandSideFieldAccessAst,
            rhsAst,
            s"$tmpName.${lhsNode.canonicalName} = ${codeOf(rhsAst.nodes.head)}",
            nodeInfo.lineNumber,
            nodeInfo.columnNumber
          )
      }
    }

    val tmpNode = identifierNode(objExpr, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val childrenAsts = propertiesAsts :+ Ast(tmpNode)
    setArgumentIndices(childrenAsts)
    blockAst(blockNode, childrenAsts)
  }

  protected def astForTSSatisfiesExpression(satisfiesExpr: BabelNodeInfo): Ast = {
    // Ignores the type, i.e. `x satisfies T` is understood as `x`.
    astForNode(satisfiesExpr.json("expression"))
  }
}
