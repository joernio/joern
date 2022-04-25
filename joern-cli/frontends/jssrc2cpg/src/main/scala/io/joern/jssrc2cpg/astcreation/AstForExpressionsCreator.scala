package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.jssrc2cpg.passes.GlobalBuiltins
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewFieldIdentifier
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.Operators

import scala.util.Try

trait AstForExpressionsCreator {

  this: AstCreator =>

  protected def astForExpressionStatement(exprStmt: BabelNodeInfo): Ast =
    astForNode(exprStmt.json("expression"))

  private def createBuiltinStaticCall(callExpr: BabelNodeInfo, callee: BabelNodeInfo, methodFullName: String): Ast = {
    val methodName = callee.node match {
      case BabelAst.MemberExpression =>
        code(callee.json("property"))
      case BabelAst.Identifier =>
        callee.code
      case _ => callee.code
    }
    val callNode =
      createStaticCallNode(callExpr.code, methodName, methodFullName, callee.lineNumber, callee.columnNumber)
    val args = astForNodes(callExpr.json("arguments").arr.toList)
    callAst(callNode, args)
  }

  private def handleCallNodeArgs(
    callExpr: BabelNodeInfo,
    receiverNode: NewNode,
    baseNode: NewNode,
    functionBaseNode: NewNode,
    functionPropertyNode: Option[NewFieldIdentifier]
  ): Ast = {
    val args = astForNodes(callExpr.json("arguments").arr.toList)

    val baseCode = codeOf(functionBaseNode)
    val propertyCode = functionPropertyNode match {
      case Some(id) => "." + codeOf(id)
      case None     => ""
    }

    val argsCode = args.map(a => codeOf(a.nodes.head)).mkString("(", ", ", ")")
    val code     = s"$baseCode$propertyCode$argsCode"

    val callNode = createCallNode(code, "", DispatchTypes.DYNAMIC_DISPATCH, callExpr.lineNumber, callExpr.columnNumber)
    callAst(callNode, Ast(baseNode) +: args, Some(Ast(receiverNode)))
  }

  protected def astForCallExpression(callExpr: BabelNodeInfo): Ast = {
    val callee         = createBabelNodeInfo(callExpr.json("callee"))
    val methodFullName = callee.code
    val callNode = if (GlobalBuiltins.builtins.contains(methodFullName)) {
      createBuiltinStaticCall(callExpr, callee, methodFullName)
    } else {
      val (functionBaseAst, functionPropertyNode, receiverAst, baseNode) = callee.node match {
        case BabelAst.MemberExpression =>
          // "this" argument is coming from source.
          val base = createBabelNodeInfo(callee.json("object"))
          base.node match {
            case BabelAst.Identifier =>
              val receiverAst = astForNode(callee.json)
              Ast.storeInDiffGraph(receiverAst, diffGraph)
              val baseNode = createIdentifierNode(base.code, base).order(1).argumentIndex(1)
              scope.addVariableReference(base.code, baseNode)
              (receiverAst, None, receiverAst, baseNode)
            case _ =>
              // TODO: check for used nodes
              val tmpVarName  = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")
              val baseTmpNode = createIdentifierNode(tmpVarName, base)
              scope.addVariableReference(tmpVarName, baseTmpNode)
              val baseAst = astForNode(base.json)
              Ast.storeInDiffGraph(baseAst, diffGraph)
              val code = s"(${codeOf(baseTmpNode)} = ${base.code})"
              val tmpAssignmentAst =
                createAssignment(baseTmpNode, baseAst.nodes.head, code, base.lineNumber, base.columnNumber)
              val member     = createBabelNodeInfo(callee.json("property"))
              val memberNode = createFieldIdentifierNode(member.code, member.lineNumber, member.columnNumber)
              val fieldAccessAst =
                createFieldAccess(tmpAssignmentAst.nodes.head, memberNode, callee.lineNumber, callee.columnNumber)
              val thisTmpNode = createIdentifierNode(tmpVarName, callee)
              scope.addVariableReference(tmpVarName, thisTmpNode)

              Ast.storeInDiffGraph(tmpAssignmentAst, diffGraph)
              Ast.storeInDiffGraph(fieldAccessAst, diffGraph)

              (baseAst, Some(memberNode), fieldAccessAst, thisTmpNode)
          }
        case _ =>
          val receiverAst = astForNode(callee.json)
          Ast.storeInDiffGraph(receiverAst, diffGraph)
          val thisNode = createIdentifierNode("this", callee)
          scope.addVariableReference(thisNode.name, thisNode)
          (receiverAst, None, receiverAst, thisNode)
      }
      handleCallNodeArgs(callExpr, receiverAst.nodes.head, baseNode, functionBaseAst.nodes.head, functionPropertyNode)
    }
    callNode
  }

  protected def astForMemberExpression(memberExpr: BabelNodeInfo): Ast = {
    val baseAst = astForNode(memberExpr.json("object"))
    Ast.storeInDiffGraph(baseAst, diffGraph)
    val memberNode =
      createFieldIdentifierNode(code(memberExpr.json("property")), memberExpr.lineNumber, memberExpr.columnNumber)
    val accessAst =
      createFieldAccess(baseAst.nodes.head, memberNode, memberExpr.lineNumber, memberExpr.columnNumber)
    accessAst
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

    val lhsAst = astForNode(assignment.json("left"))
    val rhsAst = astForNode(assignment.json("right"))

    val callNode =
      createCallNode(assignment.code, op, DispatchTypes.STATIC_DISPATCH, assignment.lineNumber, assignment.columnNumber)
    callAst(callNode, List(lhsAst, rhsAst))
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
      case "case"       => "<operator>.case"
      case other =>
        logger.warn(s"Unknown binary operator: '$other'")
        Operators.assignment
    }

    val lhsAst = astForNode(binExpr.json("left"))
    val rhsAst = astForNode(binExpr.json("right"))

    val callNode =
      createCallNode(binExpr.code, op, DispatchTypes.STATIC_DISPATCH, binExpr.lineNumber, binExpr.columnNumber)
    callAst(callNode, List(lhsAst, rhsAst))
  }

  protected def astForUpdateExpression(updateExpr: BabelNodeInfo): Ast = {
    val op = updateExpr.json("operator").str match {
      case "++" => Operators.preIncrement
      case "--" => Operators.preDecrement
      case other =>
        logger.warn(s"Unknown update operator: '$other'")
        Operators.assignment
    }

    val argumentAst = astForNode(updateExpr.json("argument"))

    val callNode =
      createCallNode(updateExpr.code, op, DispatchTypes.STATIC_DISPATCH, updateExpr.lineNumber, updateExpr.columnNumber)
    callAst(callNode, List(argumentAst))
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
    callAst(callNode, List(argumentAst))
  }

  protected def astForSequenceExpression(seq: BabelNodeInfo): Ast = {
    val blockNode = createBlockNode(seq.code, seq.lineNumber, seq.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val sequenceExpressionAsts = createBlockStatementAsts(seq.json("expressions"))
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(sequenceExpressionAsts)
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
      val blockNode = createBlockNode(arrExpr.code, lineNumber, columnNumber)
      scope.pushNewBlockScope(blockNode)
      localAstParentStack.push(blockNode)

      val tmpName      = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")
      val localTmpNode = createLocalNode(tmpName, Defines.ANY.label)
      diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)

      val tmpArrayNode = createIdentifierNode(tmpName, arrExpr)

      val arrayCallNode = createCallNode(
        EcmaBuiltins.arrayFactory + "()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH,
        lineNumber,
        columnNumber
      )

      val assignmentCode = s"${localTmpNode.code} = ${arrayCallNode.code}"
      val assignmentTmpArrayCallNode =
        createAssignment(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)
      Ast.storeInDiffGraph(assignmentTmpArrayCallNode, diffGraph)
      diffGraph.addEdge(blockNode, assignmentTmpArrayCallNode.nodes.head, EdgeTypes.AST)

      var index = 1
      elements.foreach {
        case element if !element.isNull =>
          val elementNodeInfo     = createBabelNodeInfo(element)
          val elementLineNumber   = elementNodeInfo.lineNumber
          val elementColumnNumber = elementNodeInfo.columnNumber
          val elementCode         = elementNodeInfo.code
          val elementNode         = astForNode(element)

          val pushCallNode =
            createCallNode(
              tmpName + s".push($elementCode)",
              "",
              DispatchTypes.DYNAMIC_DISPATCH,
              elementLineNumber,
              elementColumnNumber
            ).argumentIndex(index)

          val baseNode     = createIdentifierNode(tmpName, elementNodeInfo)
          val memberNode   = createFieldIdentifierNode("push", elementLineNumber, elementColumnNumber)
          val receiverNode = createFieldAccess(baseNode, memberNode, elementLineNumber, elementColumnNumber)
          val thisPushNode = createIdentifierNode(tmpName, elementNodeInfo)

          val callNode = callAst(pushCallNode, List(Ast(thisPushNode), elementNode), Some(receiverNode))
          Ast.storeInDiffGraph(callNode, diffGraph)
          diffGraph.addEdge(blockNode, pushCallNode, EdgeTypes.AST)
          index = index + 1
        case _ => // skip
      }

      val tmpArrayReturnNode = createIdentifierNode(tmpName, arrExpr)
      diffGraph.addEdge(blockNode, tmpArrayReturnNode, EdgeTypes.AST)

      scope.popScope()
      localAstParentStack.pop()

      Ast(blockNode)
    }
  }

}
