package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.{Defines, EcmaBuiltins, GlobalBuiltins}
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators, nodes}

import scala.util.Try

trait AstForExpressionsCreator { this: AstCreator =>

  protected def astForExpressionStatement(exprStmt: BabelNodeInfo): Ast =
    astForNodeWithFunctionReference(exprStmt.json("expression"))

  private def createBuiltinStaticCall(callExpr: BabelNodeInfo, callee: BabelNodeInfo, fullName: String): Ast = {
    val callName = callee.node match {
      case MemberExpression => code(callee.json("property"))
      case _                => callee.code
    }
    val callNode =
      createStaticCallNode(callExpr.code, callName, fullName, callee.lineNumber, callee.columnNumber)
    val argAsts = astForNodes(callExpr.json("arguments").arr.toList)
    callAst(callNode, argAsts)
  }

  private def handleCallNodeArgs(
    callExpr: BabelNodeInfo,
    receiverAst: Ast,
    baseNode: NewNode,
    callName: String
  ): Ast = {
    val args = astForNodes(callExpr.json("arguments").arr.toList)
    val callNode =
      createCallNode(
        callExpr.code,
        callName,
        DispatchTypes.DYNAMIC_DISPATCH,
        callExpr.lineNumber,
        callExpr.columnNumber
      )
    callAst(callNode, args, receiver = Option(receiverAst), base = Option(Ast(baseNode)))
  }

  protected def astForCallExpression(callExpr: BabelNodeInfo): Ast = {
    val callee     = createBabelNodeInfo(callExpr.json("callee"))
    val calleeCode = callee.code
    if (GlobalBuiltins.builtins.contains(calleeCode)) {
      createBuiltinStaticCall(callExpr, callee, calleeCode)
    } else {
      val (receiverAst, baseNode, callName) = callee.node match {
        case MemberExpression =>
          val base   = createBabelNodeInfo(callee.json("object"))
          val member = createBabelNodeInfo(callee.json("property"))
          base.node match {
            case Identifier | ThisExpression =>
              val receiverAst = astForNodeWithFunctionReference(callee.json)
              val baseNode    = createIdentifierNode(base.code, base)
              scope.addVariableReference(base.code, baseNode)
              (receiverAst, baseNode, member.code)
            case _ =>
              val tmpVarName  = generateUnusedVariableName(usedVariableNames, "_tmp")
              val baseTmpNode = createIdentifierNode(tmpVarName, base)
              scope.addVariableReference(tmpVarName, baseTmpNode)
              val baseAst = astForNodeWithFunctionReference(base.json)
              val code    = s"(${codeOf(baseTmpNode)} = ${base.code})"
              val tmpAssignmentAst =
                createAssignmentCallAst(Ast(baseTmpNode), baseAst, code, base.lineNumber, base.columnNumber)
              val memberNode = createFieldIdentifierNode(member.code, member.lineNumber, member.columnNumber)
              val fieldAccessAst =
                createFieldAccessCallAst(tmpAssignmentAst, memberNode, callee.lineNumber, callee.columnNumber)
              val thisTmpNode = createIdentifierNode(tmpVarName, callee)
              scope.addVariableReference(tmpVarName, thisTmpNode)

              (fieldAccessAst, thisTmpNode, member.code)
          }
        case _ =>
          val receiverAst = astForNodeWithFunctionReference(callee.json)
          val thisNode    = createIdentifierNode("this", callee)
          scope.addVariableReference(thisNode.name, thisNode)
          (receiverAst, thisNode, calleeCode)
      }
      handleCallNodeArgs(callExpr, receiverAst, baseNode, callName)
    }
  }

  protected def astForThisExpression(thisExpr: BabelNodeInfo): Ast = {
    val dynamicTypeOption = dynamicInstanceTypeStack.headOption match {
      case Some(tpe) => Option(tpe)
      case None =>
        typeFor(thisExpr) match {
          case t if t != Defines.Any => Option(t)
          case _ if methodAstParentStack.collect { case n: nodes.NewMethod if n.name == ":program" => n }.nonEmpty =>
            methodAstParentStack.collectFirst { case n: NewMethod if n.name == ":program" => n.fullName }
          case _ => None
        }
    }
    val thisNode = createIdentifierNode(thisExpr.code, dynamicTypeOption, thisExpr.lineNumber, thisExpr.columnNumber)
    scope.addVariableReference(thisExpr.code, thisNode)
    Ast(thisNode)
  }

  protected def astForNewExpression(newExpr: BabelNodeInfo): Ast = {
    val callee    = newExpr.json("callee")
    val blockNode = createBlockNode(newExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpAllocName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpAllocNode = createLocalNode(tmpAllocName, Defines.Any)
    val tmpAllocNode1     = createIdentifierNode(tmpAllocName, newExpr)
    diffGraph.addEdge(localAstParentStack.head, localTmpAllocNode, EdgeTypes.AST)
    scope.addVariableReference(tmpAllocName, tmpAllocNode1)

    val allocCallNode =
      createCallNode(".alloc", Operators.alloc, DispatchTypes.STATIC_DISPATCH, newExpr.lineNumber, newExpr.columnNumber)

    val assignmentTmpAllocCallNode =
      createAssignmentCallAst(
        tmpAllocNode1,
        allocCallNode,
        s"$tmpAllocName = ${allocCallNode.code}",
        newExpr.lineNumber,
        newExpr.columnNumber
      )

    val tmpAllocNode2 = createIdentifierNode(tmpAllocName, newExpr)

    val receiverNode = astForNodeWithFunctionReference(callee)

    // TODO: place "<operator>.new" into the schema
    val callNode = handleCallNodeArgs(newExpr, receiverNode, tmpAllocNode2, "<operator>.new")

    val tmpAllocReturnNode = Ast(createIdentifierNode(tmpAllocName, newExpr))

    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = List(assignmentTmpAllocCallNode, callNode, tmpAllocReturnNode)
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
        val callNode =
          createCallNode(
            assignment.code,
            op,
            DispatchTypes.STATIC_DISPATCH,
            assignment.lineNumber,
            assignment.columnNumber
          )
        val argAsts = List(lhsAst, rhsAst)
        callAst(callNode, argAsts)
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
    val op = Operators.notNullAssert
    val callNode =
      createCallNode(
        nonNullExpr.code,
        op,
        DispatchTypes.STATIC_DISPATCH,
        nonNullExpr.lineNumber,
        nonNullExpr.columnNumber
      )
    val argAsts = List(astForNodeWithFunctionReference(nonNullExpr.json("expression")))
    callAst(callNode, argAsts)
  }

  protected def astForCastExpression(castExpr: BabelNodeInfo): Ast = {
    val op      = Operators.cast
    val lhsNode = castExpr.json("typeAnnotation")
    val lhsAst  = Ast(createLiteralNode(code(lhsNode), None, line(lhsNode), column(lhsNode)))
    val rhsAst  = astForNodeWithFunctionReference(castExpr.json("expression"))

    val callNode =
      createCallNode(castExpr.code, op, DispatchTypes.STATIC_DISPATCH, castExpr.lineNumber, castExpr.columnNumber)
    val argAsts = List(lhsAst, rhsAst)
    callAst(callNode, argAsts)
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

    val callNode =
      createCallNode(binExpr.code, op, DispatchTypes.STATIC_DISPATCH, binExpr.lineNumber, binExpr.columnNumber)
    val argAsts = List(lhsAst, rhsAst)
    callAst(callNode, argAsts)
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

    val callNode =
      createCallNode(updateExpr.code, op, DispatchTypes.STATIC_DISPATCH, updateExpr.lineNumber, updateExpr.columnNumber)
    val argAsts = List(argumentAst)
    callAst(callNode, argAsts)
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

    val callNode =
      createCallNode(unaryExpr.code, op, DispatchTypes.STATIC_DISPATCH, unaryExpr.lineNumber, unaryExpr.columnNumber)
    val argAsts = List(argumentAst)
    callAst(callNode, argAsts)
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
    val callNode = createCallNode(
      awaitExpr.code,
      "<operator>.await",
      DispatchTypes.STATIC_DISPATCH,
      awaitExpr.lineNumber,
      awaitExpr.columnNumber
    )
    val argAsts = List(astForNodeWithFunctionReference(awaitExpr.json("argument")))
    callAst(callNode, argAsts)
  }

  protected def astForArrayExpression(arrExpr: BabelNodeInfo): Ast = {
    val lineNumber   = arrExpr.lineNumber
    val columnNumber = arrExpr.columnNumber
    val elements     = Try(arrExpr.json("elements").arr).toOption.toList.flatten
    if (elements.isEmpty) {
      Ast(
        createCallNode(
          s"${EcmaBuiltins.arrayFactory}()",
          EcmaBuiltins.arrayFactory,
          DispatchTypes.STATIC_DISPATCH,
          lineNumber,
          columnNumber
        )
      )
    } else {
      val blockNode = createBlockNode(arrExpr)
      scope.pushNewBlockScope(blockNode)
      localAstParentStack.push(blockNode)

      val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
      val localTmpNode = createLocalNode(tmpName, Defines.Any)
      val tmpArrayNode = createIdentifierNode(tmpName, arrExpr)
      diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
      scope.addVariableReference(tmpName, tmpArrayNode)

      val arrayCallNode = createCallNode(
        s"${EcmaBuiltins.arrayFactory}()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH,
        lineNumber,
        columnNumber
      )

      val assignmentCode = s"${localTmpNode.code} = ${arrayCallNode.code}"
      val assignmentTmpArrayCallNode =
        createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

      val elementAsts = elements.flatMap {
        case element if !element.isNull =>
          val elementNodeInfo     = createBabelNodeInfo(element)
          val elementLineNumber   = elementNodeInfo.lineNumber
          val elementColumnNumber = elementNodeInfo.columnNumber
          val elementCode         = elementNodeInfo.code
          val elementNode = elementNodeInfo.node match {
            case RestElement =>
              val arg1Ast = Ast(createIdentifierNode(tmpName, arrExpr))
              astForSpreadOrRestElement(elementNodeInfo, Option(arg1Ast))
            case _ =>
              astForNodeWithFunctionReference(element)
          }

          val pushCallNode = createCallNode(
            s"$tmpName.push($elementCode)",
            "",
            DispatchTypes.DYNAMIC_DISPATCH,
            elementLineNumber,
            elementColumnNumber
          )

          val baseNode     = createIdentifierNode(tmpName, elementNodeInfo)
          val memberNode   = createFieldIdentifierNode("push", elementLineNumber, elementColumnNumber)
          val receiverNode = createFieldAccessCallAst(baseNode, memberNode, elementLineNumber, elementColumnNumber)
          val thisPushNode = createIdentifierNode(tmpName, elementNodeInfo)

          Option(
            callAst(pushCallNode, List(elementNode), receiver = Option(receiverNode), base = Option(Ast(thisPushNode)))
          )
        case _ => None // skip
      }

      val tmpArrayReturnNode = createIdentifierNode(tmpName, arrExpr)

      scope.popScope()
      localAstParentStack.pop()

      val blockChildrenAsts = assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode)
      setArgumentIndices(blockChildrenAsts)
      blockAst(blockNode, blockChildrenAsts)
    }
  }

  def astForTemplateExpression(templateExpr: BabelNodeInfo): Ast = {
    val argumentAst = astForNodeWithFunctionReference(templateExpr.json("quasi"))
    val callName    = code(templateExpr.json("tag"))
    val callCode    = s"$callName(${codeOf(argumentAst.nodes.head)})"
    val templateExprCall =
      createCallNode(
        callCode,
        callName,
        DispatchTypes.STATIC_DISPATCH,
        templateExpr.lineNumber,
        templateExpr.columnNumber
      )
    val argAsts = List(argumentAst)
    callAst(templateExprCall, argAsts)
  }

  protected def astForObjectExpression(objExpr: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(objExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpName   = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localNode = createLocalNode(tmpName, Defines.Any)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    val propertiesAsts = objExpr.json("properties").arr.toList.map { property =>
      val nodeInfo = createBabelNodeInfo(property)
      nodeInfo.node match {
        case SpreadElement | RestElement =>
          val arg1Ast = Ast(createIdentifierNode(tmpName, nodeInfo))
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

          val leftHandSideTmpNode = createIdentifierNode(tmpName, nodeInfo)
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

    val tmpNode = createIdentifierNode(tmpName, objExpr)

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
