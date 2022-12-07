package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.{Defines, EcmaBuiltins, GlobalBuiltins}
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, Operators}
import io.shiftleft.codepropertygraph.generated.nodes.NewNode

import scala.util.Try

trait AstForExpressionsCreator { this: AstCreator =>

  protected def astForExpressionStatement(exprStmt: BabelNodeInfo): Ast =
    astForNode(exprStmt.json("expression"))

  private def createBuiltinStaticCall(callExpr: BabelNodeInfo, callee: BabelNodeInfo, fullName: String): Ast = {
    val callName = callee.node match {
      case MemberExpression => code(callee.json("property"))
      case _                => callee.code
    }
    val callNode =
      createStaticCallNode(callExpr.code, callName, fullName, callee.lineNumber, callee.columnNumber)
    val argAsts = astForNodes(callExpr.json("arguments").arr.toList)
    createCallAst(callNode, argAsts)
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
    createCallAst(callNode, args, Some(receiverAst), Some(Ast(baseNode)))
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
            case Identifier =>
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
          (receiverAst, thisNode, callee.code)
      }
      handleCallNodeArgs(callExpr, receiverAst, baseNode, callName)
    }
  }

  protected def astForThisExpression(thisExpr: BabelNodeInfo): Ast = {
    val thisNode = createIdentifierNode(
      thisExpr.code,
      dynamicInstanceTypeStack.headOption.orElse(Some(Defines.ANY)),
      thisExpr.lineNumber,
      thisExpr.columnNumber
    )
    scope.addVariableReference(thisExpr.code, thisNode)
    Ast(thisNode)
  }

  protected def astForNewExpression(newExpr: BabelNodeInfo): Ast = {
    val callee    = newExpr.json("callee")
    val blockNode = createBlockNode(newExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpAllocName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpAllocNode = createLocalNode(tmpAllocName, Defines.ANY)
    val tmpAllocNode1     = createIdentifierNode(tmpAllocName, newExpr)
    diffGraph.addEdge(localAstParentStack.head, localTmpAllocNode, EdgeTypes.AST)
    scope.addVariable(tmpAllocName, tmpAllocNode1, BlockScope)
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

    setArgIndices(List(assignmentTmpAllocCallNode, callNode, tmpAllocReturnNode))
    Ast(blockNode).withChild(assignmentTmpAllocCallNode).withChild(callNode).withChild(tmpAllocReturnNode)
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
    val op = assignment.json("operator").str match {
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
        createCallAst(callNode, argAsts)
    }
  }

  protected def astForConditionalExpression(ternary: BabelNodeInfo): Ast = {
    val testAst       = astForNode(ternary.json("test"))
    val consequentAst = astForNodeWithFunctionReference(ternary.json("consequent"))
    val alternateAst  = astForNodeWithFunctionReference(ternary.json("alternate"))
    createTernaryCallAst(testAst, consequentAst, alternateAst, ternary.lineNumber, ternary.columnNumber)
  }

  protected def astForLogicalExpression(logicalExpr: BabelNodeInfo): Ast =
    astForBinaryExpression(logicalExpr)

  protected def astForCastExpression(castExpr: BabelNodeInfo): Ast = {
    val op      = Operators.cast
    val lhsNode = castExpr.json("typeAnnotation")
    val lhsAst  = Ast(createLiteralNode(code(lhsNode), None, line(lhsNode), column(lhsNode)))
    val rhsAst  = astForNodeWithFunctionReference(castExpr.json("expression"))

    val callNode =
      createCallNode(castExpr.code, op, DispatchTypes.STATIC_DISPATCH, castExpr.lineNumber, castExpr.columnNumber)
    val argAsts = List(lhsAst, rhsAst)
    createCallAst(callNode, argAsts)
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

    val lhsAst = astForNode(binExpr.json("left"))
    val rhsAst = astForNodeWithFunctionReference(binExpr.json("right"))

    val callNode =
      createCallNode(binExpr.code, op, DispatchTypes.STATIC_DISPATCH, binExpr.lineNumber, binExpr.columnNumber)
    val argAsts = List(lhsAst, rhsAst)
    createCallAst(callNode, argAsts)
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

    val argumentAst = astForNode(updateExpr.json("argument"))

    val callNode =
      createCallNode(updateExpr.code, op, DispatchTypes.STATIC_DISPATCH, updateExpr.lineNumber, updateExpr.columnNumber)
    val argAsts = List(argumentAst)
    createCallAst(callNode, argAsts)
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

    val argumentAst = astForNode(unaryExpr.json("argument"))

    val callNode =
      createCallNode(unaryExpr.code, op, DispatchTypes.STATIC_DISPATCH, unaryExpr.lineNumber, unaryExpr.columnNumber)
    val argAsts = List(argumentAst)
    createCallAst(callNode, argAsts)
  }

  protected def astForSequenceExpression(seq: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(seq)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.json("expressions"))
    setArgIndices(sequenceExpressionAsts)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(sequenceExpressionAsts)
  }

  protected def astForAwaitExpression(awaitExpr: BabelNodeInfo): Ast = {
    val callNode = createCallNode(
      awaitExpr.code,
      "<operator>.await",
      DispatchTypes.STATIC_DISPATCH,
      awaitExpr.lineNumber,
      awaitExpr.columnNumber
    )
    val argAsts = List(astForNode(awaitExpr.json("argument")))
    createCallAst(callNode, argAsts)
  }

  protected def astForArrayExpression(arrExpr: BabelNodeInfo): Ast = {
    val lineNumber   = arrExpr.lineNumber
    val columnNumber = arrExpr.columnNumber
    val elements     = Try(arrExpr.json("elements").arr).toOption.toSeq.flatten
    if (elements.isEmpty) {
      Ast(
        createCallNode(
          EcmaBuiltins.arrayFactory + "()",
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
      val localTmpNode = createLocalNode(tmpName, Defines.ANY)
      val tmpArrayNode = createIdentifierNode(tmpName, arrExpr)
      diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
      scope.addVariable(tmpName, tmpArrayNode, BlockScope)
      scope.addVariableReference(tmpName, tmpArrayNode)

      val arrayCallNode = createCallNode(
        EcmaBuiltins.arrayFactory + "()",
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
          val elementNode         = astForNode(element)

          val pushCallNode = createCallNode(
            tmpName + s".push($elementCode)",
            "",
            DispatchTypes.DYNAMIC_DISPATCH,
            elementLineNumber,
            elementColumnNumber
          )

          val baseNode     = createIdentifierNode(tmpName, elementNodeInfo)
          val memberNode   = createFieldIdentifierNode("push", elementLineNumber, elementColumnNumber)
          val receiverNode = createFieldAccessCallAst(baseNode, memberNode, elementLineNumber, elementColumnNumber)
          val thisPushNode = createIdentifierNode(tmpName, elementNodeInfo)

          Some(
            createCallAst(
              pushCallNode,
              List(elementNode),
              receiver = Some(receiverNode),
              base = Some(Ast(thisPushNode))
            )
          )
        case _ => None // skip
      }

      val tmpArrayReturnNode = createIdentifierNode(tmpName, arrExpr)

      scope.popScope()
      localAstParentStack.pop()

      val blockChildrenAsts = List(assignmentTmpArrayCallNode) ++ elementAsts :+ Ast(tmpArrayReturnNode)
      setArgIndices(blockChildrenAsts)
      Ast(blockNode).withChildren(blockChildrenAsts)
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
    createCallAst(templateExprCall, argAsts)
  }

  protected def astForObjectExpression(objExpr: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(objExpr)

    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val tmpName   = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localNode = createLocalNode(tmpName, Defines.ANY)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    val propertiesAsts = objExpr.json("properties").arr.toList.map { property =>
      val nodeInfo = createBabelNodeInfo(property)
      val (lhsNode, rhsAst) = nodeInfo.node match {
        case ObjectMethod =>
          val keyName = nodeInfo.json("key")("name").str
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
        case SpreadElement =>
          val ast         = astForNodeWithFunctionReferenceAndCall(nodeInfo.json("argument"))
          val defaultName = codeForNodes(ast.root.toSeq)
          val keyName     = nameForBabelNodeInfo(nodeInfo, defaultName)
          val keyNode     = createFieldIdentifierNode(keyName, nodeInfo.lineNumber, nodeInfo.columnNumber)
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

    val tmpNode = createIdentifierNode(tmpName, objExpr)

    scope.popScope()
    localAstParentStack.pop()

    val allBlockChildren = propertiesAsts :+ Ast(tmpNode)
    setArgIndices(propertiesAsts)
    Ast(blockNode).withChildren(allBlockChildren)
  }
}
