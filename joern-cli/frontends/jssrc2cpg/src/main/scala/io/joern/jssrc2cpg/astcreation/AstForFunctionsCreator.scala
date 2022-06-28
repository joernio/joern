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
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
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
    val nodeInfo = createBabelNodeInfo(param)
    nodeInfo.node match {
      case BabelAst.RestElement =>
        val paramName = nodeInfo.code.replace("...", "")
        val tpe       = typeFor(nodeInfo)
        if (createLocals) {
          val localNode = createLocalNode(paramName, tpe)
          diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
        }
        createParameterInNode(
          paramName,
          nodeInfo.code,
          index,
          isVariadic = true,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber,
          Some(tpe)
        )
      case BabelAst.AssignmentPattern =>
        val lhsElement  = nodeInfo.json("left")
        val rhsElement  = nodeInfo.json("right")
        val lhsNodeInfo = createBabelNodeInfo(lhsElement)
        lhsNodeInfo.node match {
          case BabelAst.ObjectPattern =>
            val name = generateUnusedVariableName(usedVariableNames, s"param$index")
            val param = createParameterInNode(
              name,
              nodeInfo.code,
              index,
              isVariadic = false,
              nodeInfo.lineNumber,
              nodeInfo.columnNumber
            )
            val rhsAst = astForNodeWithFunctionReference(rhsElement)
            Ast.storeInDiffGraph(rhsAst, diffGraph)
            additionalBlockStatements.addOne(astForDeconstruction(lhsNodeInfo, rhsAst, Some(name)))
            param
          case BabelAst.ArrayPattern =>
            val name = generateUnusedVariableName(usedVariableNames, s"param$index")
            val param = createParameterInNode(
              name,
              nodeInfo.code,
              index,
              isVariadic = false,
              nodeInfo.lineNumber,
              nodeInfo.columnNumber
            )
            val rhsAst = astForNodeWithFunctionReference(rhsElement)
            Ast.storeInDiffGraph(rhsAst, diffGraph)
            additionalBlockStatements.addOne(astForDeconstruction(lhsNodeInfo, rhsAst, Some(name)))
            param
          case _ =>
            additionalBlockStatements.addOne(convertParamWithDefault(nodeInfo))
            val tpe = typeFor(lhsNodeInfo)
            createParameterInNode(
              lhsNodeInfo.code,
              lhsNodeInfo.code,
              index,
              isVariadic = false,
              lhsNodeInfo.lineNumber,
              lhsNodeInfo.columnNumber,
              Some(tpe)
            )
        }
      case BabelAst.ArrayPattern =>
        val name = generateUnusedVariableName(usedVariableNames, s"param$index")
        val tpe  = typeFor(nodeInfo)
        val param = createParameterInNode(
          name,
          nodeInfo.code,
          index,
          isVariadic = false,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber,
          Some(tpe)
        )
        additionalBlockStatements.addAll(nodeInfo.json("elements").arr.toList.map {
          case element if !element.isNull =>
            val elementNodeInfo = createBabelNodeInfo(element)
            elementNodeInfo.node match {
              case BabelAst.Identifier =>
                val paramName      = code(elementNodeInfo.json)
                val tpe            = typeFor(elementNodeInfo)
                val localParamNode = createIdentifierNode(paramName, elementNodeInfo)
                localParamNode.typeFullName = tpe
                val paramNode = createIdentifierNode(name, elementNodeInfo)
                val keyNode =
                  createFieldIdentifierNode(paramName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                val accessAst =
                  createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                Ast.storeInDiffGraph(accessAst, diffGraph)
                createAssignmentCallAst(
                  localParamNode,
                  accessAst.nodes.head,
                  s"$paramName = ${codeOf(accessAst.nodes.head)}",
                  elementNodeInfo.lineNumber,
                  elementNodeInfo.columnNumber
                )
              case _ => astForNode(elementNodeInfo.json)
            }
          case _ => Ast()
        })
        param
      case BabelAst.ObjectPattern =>
        val name = generateUnusedVariableName(usedVariableNames, s"param$index")
        val tpe  = typeFor(nodeInfo)
        val param = createParameterInNode(
          name,
          nodeInfo.code,
          index,
          isVariadic = false,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber,
          Some(tpe)
        )
        additionalBlockStatements.addAll(nodeInfo.json("properties").arr.toList.map { element =>
          val elementNodeInfo = createBabelNodeInfo(element)
          elementNodeInfo.node match {
            case BabelAst.ObjectProperty =>
              val paramName      = code(elementNodeInfo.json("key"))
              val tpe            = typeFor(elementNodeInfo)
              val localParamNode = createIdentifierNode(paramName, elementNodeInfo)
              localParamNode.typeFullName = tpe
              val paramNode = createIdentifierNode(name, elementNodeInfo)
              val keyNode =
                createFieldIdentifierNode(paramName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
              val accessAst =
                createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
              Ast.storeInDiffGraph(accessAst, diffGraph)
              createAssignmentCallAst(
                localParamNode,
                accessAst.nodes.head,
                s"$paramName = ${codeOf(accessAst.nodes.head)}",
                elementNodeInfo.lineNumber,
                elementNodeInfo.columnNumber
              )
            case _ => astForNode(elementNodeInfo.json)
          }
        })
        param
      case BabelAst.Identifier =>
        val tpe = typeFor(nodeInfo)
        createParameterInNode(
          nodeInfo.json("name").str,
          nodeInfo.code,
          index,
          isVariadic = false,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber,
          Some(tpe)
        )
      case _ =>
        val tpe = typeFor(nodeInfo)
        createParameterInNode(
          nodeInfo.code,
          nodeInfo.code,
          index,
          isVariadic = false,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber,
          Some(tpe)
        )
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

  private def getParentTypeDecl: NewTypeDecl = {
    methodAstParentStack.collectFirst { case n: NewTypeDecl => n }.getOrElse(rootTypeDecl.head)
  }

  protected def astForTSDeclareFunction(func: BabelNodeInfo): Ast = {
    val functionNode = createMethodDefinitionNode(func)
    val bindingNode  = createBindingNode()
    diffGraph.addEdge(getParentTypeDecl, bindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
    addModifier(functionNode, func.json)
    Ast(functionNode)
  }

  protected def createMethodDefinitionNode(func: BabelNodeInfo): NewMethod = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)
    val methodNode                   = createMethodNode(methodName, methodFullName, func)
    val virtualModifierNode          = NewModifier().modifierType(ModifierTypes.VIRTUAL)
    methodAstParentStack.push(methodNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = if (hasKey(func.json, "parameters")) {
      handleParameters(func.json("parameters").arr.toSeq, mutable.ArrayBuffer.empty[Ast], createLocals = false)
    } else {
      handleParameters(func.json("params").arr.toSeq, mutable.ArrayBuffer.empty[Ast], createLocals = false)
    }

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

    val bodyStmtAsts = func.node match {
      case BabelAst.ArrowFunctionExpression => createBlockStatementAsts(Arr(block))
      case _                                => createBlockStatementAsts(block("body"))
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
  ): Ast = {
    val (ast, _) = createMethodAstAndNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall)
    ast
  }

}
