package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewMethodParameterIn
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Arr
import ujson.Value

import scala.collection.mutable

trait AstForFunctionsCreator {

  this: AstCreator =>

  private def handleParameters(
    parameters: Seq[Value],
    additionalBlockStatements: mutable.ArrayBuffer[Ast],
    createLocals: Boolean = true
  ): Seq[NewMethodParameterIn] = withIndex(parameters) { case (param, index) =>
    createBabelNodeInfo(param) match {
      case rest @ BabelNodeInfo(BabelAst.RestElement) =>
        val paramName = rest.code.replace("...", "")
        if (createLocals) {
          val localNode = createLocalNode(paramName, Defines.ANY.label)
          diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
        }
        createParameterInNode(paramName, rest.code, index, isVariadic = true, rest.lineNumber, rest.columnNumber)
      case assignmentPattern @ BabelNodeInfo(BabelAst.AssignmentPattern) =>
        val lhsElement = assignmentPattern.json("left")
        val rhsElement = assignmentPattern.json("right")
        createBabelNodeInfo(lhsElement) match {
          case objPattern @ BabelNodeInfo(BabelAst.ObjectPattern) =>
            val name = generateUnusedVariableName(usedVariableNames, Set.empty, s"param$index")
            val param = createParameterInNode(
              name,
              assignmentPattern.code,
              index,
              isVariadic = false,
              assignmentPattern.lineNumber,
              assignmentPattern.columnNumber
            )
            val rhsAst = astForNodeWithFunctionReference(rhsElement)
            Ast.storeInDiffGraph(rhsAst, diffGraph)
            additionalBlockStatements.addOne(astForDeconstruction(objPattern, rhsAst, Some(name)))
            param
          case arrPattern @ BabelNodeInfo(BabelAst.ArrayPattern) =>
            val name = generateUnusedVariableName(usedVariableNames, Set.empty, s"param$index")
            val param = createParameterInNode(
              name,
              assignmentPattern.code,
              index,
              isVariadic = false,
              assignmentPattern.lineNumber,
              assignmentPattern.columnNumber
            )
            val rhsAst = astForNodeWithFunctionReference(rhsElement)
            Ast.storeInDiffGraph(rhsAst, diffGraph)
            additionalBlockStatements.addOne(astForDeconstruction(arrPattern, rhsAst, Some(name)))
            param
          case other =>
            additionalBlockStatements.addOne(convertParamWithDefault(assignmentPattern))
            createParameterInNode(
              other.code,
              other.code,
              index,
              isVariadic = false,
              other.lineNumber,
              other.columnNumber
            )
        }
      case arrPattern @ BabelNodeInfo(BabelAst.ArrayPattern) =>
        val name = generateUnusedVariableName(usedVariableNames, Set.empty, s"param$index")
        val param = createParameterInNode(
          name,
          arrPattern.code,
          index,
          isVariadic = false,
          arrPattern.lineNumber,
          arrPattern.columnNumber
        )
        additionalBlockStatements.addAll(arrPattern.json("elements").arr.toList.map {
          case element if !element.isNull =>
            createBabelNodeInfo(element) match {
              case ident @ BabelNodeInfo(BabelAst.Identifier) =>
                val paramName      = code(ident.json)
                val localParamNode = createIdentifierNode(paramName, ident)
                val paramNode      = createIdentifierNode(name, ident)
                val keyNode        = createFieldIdentifierNode(paramName, ident.lineNumber, ident.columnNumber)
                val accessAst      = createFieldAccessCallAst(paramNode, keyNode, ident.lineNumber, ident.columnNumber)
                Ast.storeInDiffGraph(accessAst, diffGraph)
                createAssignmentCallAst(
                  localParamNode,
                  accessAst.nodes.head,
                  s"$paramName = ${codeOf(accessAst.nodes.head)}",
                  ident.lineNumber,
                  ident.columnNumber
                )
              case other => astForNode(other.json)
            }
          case _ => Ast()
        })
        param
      case objPattern @ BabelNodeInfo(BabelAst.ObjectPattern) =>
        val name = generateUnusedVariableName(usedVariableNames, Set.empty, s"param$index")
        val param = createParameterInNode(
          name,
          objPattern.code,
          index,
          isVariadic = false,
          objPattern.lineNumber,
          objPattern.columnNumber
        )
        additionalBlockStatements.addAll(objPattern.json("properties").arr.toList.map { element =>
          createBabelNodeInfo(element) match {
            case prop @ BabelNodeInfo(BabelAst.ObjectProperty) =>
              val paramName      = code(prop.json("key"))
              val localParamNode = createIdentifierNode(paramName, prop)
              val paramNode      = createIdentifierNode(name, prop)
              val keyNode        = createFieldIdentifierNode(paramName, prop.lineNumber, prop.columnNumber)
              val accessAst      = createFieldAccessCallAst(paramNode, keyNode, prop.lineNumber, prop.columnNumber)
              Ast.storeInDiffGraph(accessAst, diffGraph)
              createAssignmentCallAst(
                localParamNode,
                accessAst.nodes.head,
                s"$paramName = ${codeOf(accessAst.nodes.head)}",
                prop.lineNumber,
                prop.columnNumber
              )
            case other => astForNode(other.json)
          }
        })
        param
      case id @ BabelNodeInfo(BabelAst.Identifier) =>
        createParameterInNode(id.json("name").str, id.code, index, isVariadic = false, id.lineNumber, id.columnNumber)
      case other =>
        createParameterInNode(other.code, other.code, index, isVariadic = false, other.lineNumber, other.columnNumber)
    }
  }

  private def convertParamWithDefault(element: BabelNodeInfo): Ast = {
    val lhsElement = element.json("left")
    val rhsElement = element.json("right")

    val rhsAst = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsAst, diffGraph)

    val lhsAst = astForNode(lhsElement)
    Ast.storeInDiffGraph(lhsAst, diffGraph)

    val testAst = {
      val keyNode = createIdentifierNode(codeOf(lhsAst.nodes.head), element)
      val voidCallNode = createCallNode(
        "void 0",
        "<operator>.void",
        DispatchTypes.STATIC_DISPATCH,
        element.lineNumber,
        element.columnNumber
      )
      val equalsCallAst = createEqualsCallAst(keyNode, voidCallNode, element.lineNumber, element.columnNumber)
      equalsCallAst
    }
    Ast.storeInDiffGraph(testAst, diffGraph)
    val falseNode = createIdentifierNode(codeOf(lhsAst.nodes.head), element)
    val ternaryNodeAst =
      createTernaryCallAst(testAst.nodes.head, rhsAst.nodes.head, falseNode, element.lineNumber, element.columnNumber)
    Ast.storeInDiffGraph(ternaryNodeAst, diffGraph)
    createAssignmentCallAst(
      lhsAst.nodes.head,
      ternaryNodeAst.nodes.head,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  protected def createMethodDefinitionNode(func: BabelNodeInfo): NewMethod = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodNode                   = createMethodNode(methodName, methodFullName, func)
    val virtualModifierNode          = NewModifier().modifierType(ModifierTypes.VIRTUAL)
    methodAstParentStack.push(methodNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes =
      handleParameters(func.json("parameters").arr.toSeq, mutable.ArrayBuffer.empty[Ast], createLocals = false)

    val methodReturnNode = createMethodReturnNode(func)

    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        methodNode,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst = methodStubAst(methodNode, thisNode +: paramNodes, methodReturnNode).withChild(Ast(virtualModifierNode))

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    methodNode
  }

  protected def createMethodAstAndNode(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): (Ast, NewMethod) = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodRefNode = if (!shouldCreateFunctionReference) {
      None
    } else { Some(createMethodRefNode(methodName, methodFullName, func)) }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = createIdentifierNode(methodName, func)
      val idLocal = createLocalNode(methodName, methodFullName)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      scope.addVariableReference(methodName, idNode)
      val code       = s"function $methodName = ${func.code}"
      val assignment = createAssignmentCallAst(idNode, methodRefNode.get, code, func.lineNumber, func.columnNumber)
      assignment
    } else {
      Ast()
    }

    val methodNode          = createMethodNode(methodName, methodFullName, func)
    val virtualModifierNode = NewModifier().modifierType(ModifierTypes.VIRTUAL)

    methodAstParentStack.push(methodNode)

    val block             = func.json("body")
    val blockLineNumber   = line(block)
    val blockColumnNumber = column(block)
    val blockCode         = code(block)
    val blockNode = NewBlock()
      .typeFullName(Defines.ANY.label)
      .code(blockCode)
      .lineNumber(blockLineNumber)
      .columnNumber(blockColumnNumber)
    val blockAst                  = Ast(blockNode)
    val additionalBlockStatements = mutable.ArrayBuffer.empty[Ast]

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode
      } else {
        metaTypeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)
    localAstParentStack.push(blockNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = handleParameters(func.json("params").arr.toSeq, additionalBlockStatements)

    val bodyStmtAsts = func match {
      case BabelNodeInfo(BabelAst.ArrowFunctionExpression) => createBlockStatementAsts(Arr(block))
      case _                                               => createBlockStatementAsts(block("body"))
    }
    setIndices(additionalBlockStatements.toList ++ bodyStmtAsts)

    val methodReturnNode = createMethodReturnNode(func)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        methodNode,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst =
      methodAst(
        methodNode,
        thisNode +: paramNodes,
        blockAst.withChildren(additionalBlockStatements ++ bodyStmtAsts),
        methodReturnNode
      )
        .withChild(Ast(virtualModifierNode))

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    val ast = methodRefNode match {
      case Some(ref) if callAst.nodes.isEmpty => Ast(ref)
      case _                                  => callAst
    }
    (ast, methodNode)
  }

  protected def astForFunctionDeclaration(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = createMethodAstAndNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall)._1

}
