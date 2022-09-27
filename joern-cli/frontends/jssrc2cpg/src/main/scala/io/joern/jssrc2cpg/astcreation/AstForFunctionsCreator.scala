package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, ModifierTypes}
import io.shiftleft.codepropertygraph.generated.nodes.{
  IdentifierBase,
  NewIdentifier,
  NewMethod,
  NewMethodParameterIn,
  NewModifier,
  NewTypeDecl,
  TypeRefBase
}
import io.shiftleft.codepropertygraph.generated.nodes.NewBlock
import ujson.{Arr, Value}

import scala.collection.mutable

trait AstForFunctionsCreator { this: AstCreator =>

  case class MethodAst(ast: Ast, methodNode: NewMethod, methodAst: Ast)

  private def handleRestInParameters(
    elementNodeInfo: BabelNodeInfo,
    paramNodeInfo: BabelNodeInfo,
    paramName: String
  ): Ast = {
    val ast = astForNodeWithFunctionReferenceAndCall(elementNodeInfo.json("argument"))
    val defaultName = ast.nodes.collectFirst {
      case id: IdentifierBase => id.name.replace("...", "")
      case clazz: TypeRefBase => clazz.code.stripPrefix("class ")
    }
    val restName = codeForExportObject(paramNodeInfo, defaultName).headOption
      .getOrElse {
        if (defaultName.isEmpty) {
          val tmpName   = generateUnusedVariableName(usedVariableNames, "_tmp")
          val localNode = createLocalNode(tmpName, Defines.ANY.label)
          diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)
          tmpName
        } else { defaultName.get }
      }
      .replace("...", "")

    ast.root match {
      case Some(_: NewIdentifier) =>
        val keyNode = Ast(createFieldIdentifierNode(restName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber))
        val tpe     = typeFor(elementNodeInfo)
        val localParamNode = createIdentifierNode(restName, elementNodeInfo)
        localParamNode.typeFullName = tpe
        val paramNode = createIdentifierNode(paramName, elementNodeInfo)
        val accessAst =
          createFieldAccessCallAst(
            paramNode,
            keyNode.nodes.head,
            elementNodeInfo.lineNumber,
            elementNodeInfo.columnNumber
          )
        Ast.storeInDiffGraph(accessAst, diffGraph)
        createAssignmentCallAst(
          localParamNode,
          accessAst.nodes.head,
          s"$restName = ${codeOf(accessAst.nodes.head)}",
          elementNodeInfo.lineNumber,
          elementNodeInfo.columnNumber
        )
      case _ =>
        Ast.storeInDiffGraph(ast, diffGraph)
        val localParamNode = createIdentifierNode(restName, elementNodeInfo)
        createAssignmentCallAst(
          localParamNode,
          ast.nodes.head,
          s"$restName = ${codeOf(ast.nodes.head)}",
          elementNodeInfo.lineNumber,
          elementNodeInfo.columnNumber
        )
    }
  }

  private def handleParameters(
    parameters: Seq[Value],
    additionalBlockStatements: mutable.ArrayBuffer[Ast],
    createLocals: Boolean = true
  ): Seq[NewMethodParameterIn] = withIndex(parameters) { case (param, index) =>
    val nodeInfo = createBabelNodeInfo(param)
    nodeInfo.node match {
      case RestElement =>
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
      case AssignmentPattern =>
        val lhsElement  = nodeInfo.json("left")
        val rhsElement  = nodeInfo.json("right")
        val lhsNodeInfo = createBabelNodeInfo(lhsElement)
        lhsNodeInfo.node match {
          case ObjectPattern | ArrayPattern =>
            val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
            val param = createParameterInNode(
              paramName,
              nodeInfo.code,
              index,
              isVariadic = false,
              nodeInfo.lineNumber,
              nodeInfo.columnNumber
            )
            val rhsAst = astForNodeWithFunctionReference(rhsElement)
            Ast.storeInDiffGraph(rhsAst, diffGraph)
            additionalBlockStatements.addOne(astForDeconstruction(lhsNodeInfo, rhsAst, Some(paramName)))
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
      case ArrayPattern =>
        val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
        val tpe       = typeFor(nodeInfo)
        val param = createParameterInNode(
          paramName,
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
              case Identifier =>
                val elemName       = code(elementNodeInfo.json)
                val tpe            = typeFor(elementNodeInfo)
                val localParamNode = createIdentifierNode(elemName, elementNodeInfo)
                localParamNode.typeFullName = tpe
                val paramNode = createIdentifierNode(paramName, elementNodeInfo)
                val keyNode =
                  createFieldIdentifierNode(elemName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                val accessAst =
                  createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                Ast.storeInDiffGraph(accessAst, diffGraph)
                createAssignmentCallAst(
                  localParamNode,
                  accessAst.nodes.head,
                  s"$elemName = ${codeOf(accessAst.nodes.head)}",
                  elementNodeInfo.lineNumber,
                  elementNodeInfo.columnNumber
                )
              case RestElement => handleRestInParameters(elementNodeInfo, nodeInfo, paramName)
              case _           => astForNode(elementNodeInfo.json)
            }
          case _ => Ast()
        })
        param
      case ObjectPattern =>
        val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
        val tpe       = typeFor(nodeInfo)
        val param = createParameterInNode(
          paramName,
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
            case ObjectProperty =>
              val elemName       = code(elementNodeInfo.json("key"))
              val tpe            = typeFor(elementNodeInfo)
              val localParamNode = createIdentifierNode(elemName, elementNodeInfo)
              localParamNode.typeFullName = tpe
              val paramNode = createIdentifierNode(paramName, elementNodeInfo)
              val keyNode =
                createFieldIdentifierNode(elemName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
              val accessAst =
                createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
              Ast.storeInDiffGraph(accessAst, diffGraph)
              createAssignmentCallAst(
                localParamNode,
                accessAst.nodes.head,
                s"$elemName = ${codeOf(accessAst.nodes.head)}",
                elementNodeInfo.lineNumber,
                elementNodeInfo.columnNumber
              )
            case RestElement => handleRestInParameters(elementNodeInfo, nodeInfo, paramName)
            case _           => astForNode(elementNodeInfo.json)
          }
        })
        param
      case Identifier =>
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

  private def getParentTypeDecl: NewTypeDecl =
    methodAstParentStack.collectFirst { case n: NewTypeDecl => n }.getOrElse(rootTypeDecl.head)

  protected def astForTSDeclareFunction(func: BabelNodeInfo): Ast = {
    val functionNode = createMethodDefinitionNode(func)
    val bindingNode  = createBindingNode()
    diffGraph.addEdge(getParentTypeDecl, bindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
    addModifier(functionNode, func.json)
    Ast(functionNode)
  }

  protected def createMethodDefinitionNode(
    func: BabelNodeInfo,
    methodBlockContent: List[Ast] = List.empty
  ): NewMethod = {
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

    val mAst = if (methodBlockContent.isEmpty) {
      methodStubAst(methodNode, thisNode +: paramNodes, methodReturnNode, List(virtualModifierNode))
    } else {
      setIndices(methodBlockContent)
      val bodyAst = blockAst(NewBlock(), methodBlockContent)
      methodAst(methodNode, thisNode +: paramNodes, bodyAst, methodReturnNode)
    }

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    methodNode
  }

  protected def createMethodAstAndNode(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false,
    methodBlockContent: List[Ast] = List.empty
  ): MethodAst = {
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

    val blockJson                 = func.json("body")
    val blockNodeInfo             = createBabelNodeInfo(blockJson)
    val blockNode                 = createBlockNode(blockNodeInfo)
    val blockAst                  = Ast(blockNode)
    val additionalBlockStatements = mutable.ArrayBuffer.empty[Ast]

    val capturingRefNode =
      if (shouldCreateFunctionReference) {
        methodRefNode
      } else {
        typeRefIdStack.headOption
      }
    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)
    localAstParentStack.push(blockNode)

    val thisNode =
      createParameterInNode("this", "this", 0, isVariadic = false, line = func.lineNumber, column = func.columnNumber)

    val paramNodes = handleParameters(func.json("params").arr.toSeq, additionalBlockStatements)

    val bodyStmtAsts = func.node match {
      case ArrowFunctionExpression => createBlockStatementAsts(Arr(blockJson))
      case _                       => createBlockStatementAsts(blockJson("body"))
    }
    setIndices(methodBlockContent ++ additionalBlockStatements.toList ++ bodyStmtAsts)

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
        blockAst.withChildren(methodBlockContent ++ additionalBlockStatements ++ bodyStmtAsts),
        methodReturnNode,
        List(virtualModifierNode)
      )

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode, EdgeTypes.AST)

    methodRefNode match {
      case Some(ref) if callAst.nodes.isEmpty =>
        MethodAst(Ast(ref), methodNode, mAst)
      case _ =>
        MethodAst(callAst, methodNode, mAst)
    }
  }

  protected def astForFunctionDeclaration(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = createMethodAstAndNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall).ast

}
