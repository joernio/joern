package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.{BlockScope, MethodScope}
import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.NodeBuilders.{newBindingNode, newModifierNode}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{Identifier as _, *}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, EvaluationStrategies, ModifierTypes}
import ujson.Value

import scala.collection.mutable
import scala.util.Try

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  case class MethodAst(ast: Ast, methodNode: NewMethod, methodAst: Ast)

  private def handleRestInParameters(
    elementNodeInfo: BabelNodeInfo,
    paramNodeInfo: BabelNodeInfo,
    paramName: String
  ): Ast = {
    val ast         = astForNodeWithFunctionReferenceAndCall(elementNodeInfo.json("argument"))
    val defaultName = codeForNodes(ast.nodes.toSeq)
    val restName    = nameForBabelNodeInfo(paramNodeInfo, defaultName)
    ast.root match {
      case Some(_: NewIdentifier) =>
        val keyNode   = createFieldIdentifierNode(restName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
        val paramNode = identifierNode(elementNodeInfo, paramName)
        val accessAst =
          createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
        createAssignmentCallAst(
          ast,
          accessAst,
          s"$restName = ${codeOf(accessAst.nodes.head)}",
          elementNodeInfo.lineNumber,
          elementNodeInfo.columnNumber
        )
      case _ =>
        val localParamNode = identifierNode(elementNodeInfo, restName)
        createAssignmentCallAst(
          Ast(localParamNode),
          ast,
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
  ): Seq[NewMethodParameterIn] =
    withIndex(parameters) { case (param, index) =>
      val nodeInfo = createBabelNodeInfo(param)
      val paramNode = nodeInfo.node match {
        case RestElement =>
          val paramName     = nodeInfo.code.replace("...", "")
          val tpe           = typeFor(nodeInfo)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          if (createLocals) {
            val localTmpNode = localNode(nodeInfo, paramName, paramName, typeFullName)
              .order(0)
              .possibleTypes(possibleTypes)
            diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
          }
          parameterInNode(nodeInfo, paramName, nodeInfo.code, index, true, EvaluationStrategies.BY_VALUE, typeFullName)
            .possibleTypes(possibleTypes)
        case AssignmentPattern =>
          val lhsElement  = nodeInfo.json("left")
          val rhsElement  = nodeInfo.json("right")
          val lhsNodeInfo = createBabelNodeInfo(lhsElement)
          lhsNodeInfo.node match {
            case ObjectPattern | ArrayPattern =>
              val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
              val param = parameterInNode(
                nodeInfo,
                paramName,
                nodeInfo.code,
                index,
                isVariadic = false,
                EvaluationStrategies.BY_VALUE
              )
              scope.addVariable(paramName, param, MethodScope)

              val rhsAst = astForNodeWithFunctionReference(rhsElement)
              additionalBlockStatements.addOne(
                astForDeconstruction(lhsNodeInfo, rhsAst, nodeInfo.code, Option(paramName))
              )
              param
            case _ =>
              additionalBlockStatements.addOne(convertParamWithDefault(nodeInfo))
              val possibleTypes = Seq(typeFor(lhsNodeInfo))
              parameterInNode(lhsNodeInfo, lhsNodeInfo.code, nodeInfo.code, index, false, EvaluationStrategies.BY_VALUE)
                .possibleTypes(possibleTypes)
          }
        case ArrayPattern =>
          val paramName     = generateUnusedVariableName(usedVariableNames, s"param$index")
          val tpe           = typeFor(nodeInfo)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          val param = parameterInNode(
            nodeInfo,
            paramName,
            nodeInfo.code,
            index,
            isVariadic = false,
            EvaluationStrategies.BY_VALUE,
            typeFullName
          ).possibleTypes(possibleTypes)
          additionalBlockStatements.addAll(nodeInfo.json("elements").arr.toList.map {
            case element if !element.isNull =>
              val elementNodeInfo = createBabelNodeInfo(element)
              elementNodeInfo.node match {
                case Identifier =>
                  val elemName       = code(elementNodeInfo.json)
                  val tpe            = typeFor(elementNodeInfo)
                  val typeFullName   = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
                  val possibleTypes  = Seq(tpe)
                  val localParamNode = identifierNode(elementNodeInfo, elemName).possibleTypes(possibleTypes)
                  val localTmpNode = localNode(elementNodeInfo, elemName, elemName, typeFullName)
                    .order(0)
                    .possibleTypes(possibleTypes)
                  diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
                  scope.addVariable(elemName, localTmpNode, MethodScope)

                  val paramNode = identifierNode(elementNodeInfo, paramName)
                  scope.addVariableReference(paramName, paramNode)

                  val keyNode =
                    createFieldIdentifierNode(elemName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                  val accessAst =
                    createFieldAccessCallAst(
                      paramNode,
                      keyNode,
                      elementNodeInfo.lineNumber,
                      elementNodeInfo.columnNumber
                    )
                  createAssignmentCallAst(
                    Ast(localParamNode),
                    accessAst,
                    s"$elemName = ${codeOf(accessAst.nodes.head)}",
                    elementNodeInfo.lineNumber,
                    elementNodeInfo.columnNumber
                  )
                case RestElement => handleRestInParameters(elementNodeInfo, nodeInfo, paramName)
                case _           => astForNodeWithFunctionReference(elementNodeInfo.json)
              }
            case _ => Ast()
          })
          param
        case ObjectPattern =>
          val paramName = generateUnusedVariableName(usedVariableNames, s"param$index")
          // Handle de-structured parameters declared as `{ username: string; password: string; }`
          val typeDeclAst = astForTypeAlias(nodeInfo)
          Ast.storeInDiffGraph(typeDeclAst, diffGraph)

          val tpe          = typeFor(nodeInfo)
          var typeFullName = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any

          val possibleTypes =
            Seq(
              typeDeclAst.root
                .collect { case t: NewTypeDecl =>
                  typeFullName = t.fullName
                  t.fullName
                }
                .getOrElse(tpe)
            )
          val param = parameterInNode(
            nodeInfo,
            paramName,
            nodeInfo.code,
            index,
            isVariadic = false,
            EvaluationStrategies.BY_VALUE,
            typeFullName
          ).possibleTypes(possibleTypes)
          scope.addVariable(paramName, param, MethodScope)

          additionalBlockStatements.addAll(nodeInfo.json("properties").arr.toList.map { element =>
            val elementNodeInfo = createBabelNodeInfo(element)
            elementNodeInfo.node match {
              case ObjectProperty =>
                val elemName = code(elementNodeInfo.json("key"))

                val tpe           = typeFor(elementNodeInfo)
                val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
                val possibleTypes = Seq(tpe)

                val localParamNode =
                  identifierNode(elementNodeInfo, elemName).typeFullName(typeFullName).possibleTypes(possibleTypes)
                val localTmpNode =
                  localNode(elementNodeInfo, elemName, elemName, typeFullName).order(0).possibleTypes(possibleTypes)
                diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
                scope.addVariable(elemName, localTmpNode, MethodScope)

                val paramNode = identifierNode(elementNodeInfo, paramName)
                scope.addVariableReference(paramName, paramNode)

                val keyNode =
                  createFieldIdentifierNode(elemName, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                val accessAst =
                  createFieldAccessCallAst(paramNode, keyNode, elementNodeInfo.lineNumber, elementNodeInfo.columnNumber)
                val assignmentCallAst = createAssignmentCallAst(
                  Ast(localParamNode),
                  accessAst,
                  s"$elemName = ${codeOf(accessAst.nodes.head)}",
                  elementNodeInfo.lineNumber,
                  elementNodeInfo.columnNumber
                )
                // Handle identifiers referring to locals created by destructured parameters
                assignmentCallAst.nodes
                  .collect { case i: NewIdentifier if localTmpNode.name == i.name => i }
                  .map { i => assignmentCallAst.withRefEdge(i, localTmpNode) }
                  .reduce(_ `merge` _)
              case RestElement => handleRestInParameters(elementNodeInfo, nodeInfo, paramName)
              case _           => astForNodeWithFunctionReference(elementNodeInfo.json)
            }
          })
          param
        case Identifier =>
          // Handle types declared as `credentials: { username: string; password: string; }`
          val tpe           = typeFor(nodeInfo)
          var typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          val possibleType = Try(createBabelNodeInfo(nodeInfo.json("typeAnnotation")("typeAnnotation")))
            .map(x =>
              x.node match {
                case TSTypeLiteral =>
                  val typeDecl = astForTypeAlias(x)
                  Ast.storeInDiffGraph(typeDecl, diffGraph)
                  typeDecl.root
                    .collect { case t: NewTypeDecl =>
                      typeFullName = t.fullName
                      t.fullName
                    }
                    .getOrElse(tpe)
                case _ => tpe
              }
            )
            .getOrElse(tpe)

          val name = nodeInfo.json("name").str
          val node =
            parameterInNode(nodeInfo, name, nodeInfo.code, index, false, EvaluationStrategies.BY_VALUE, typeFullName)
              .possibleTypes(Seq(possibleType))
          scope.addVariable(name, node, MethodScope)
          node
        case TSParameterProperty =>
          val unpackedParam = createBabelNodeInfo(nodeInfo.json("parameter"))

          val tpe           = typeFor(unpackedParam)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)

          val name = unpackedParam.node match {
            case AssignmentPattern => createBabelNodeInfo(unpackedParam.json("left")).code
            case _                 => unpackedParam.json("name").str
          }
          val node =
            parameterInNode(nodeInfo, name, nodeInfo.code, index, false, EvaluationStrategies.BY_VALUE, typeFullName)
              .possibleTypes(possibleTypes)
          scope.addVariable(name, node, MethodScope)
          node
        case _ =>
          val tpe           = typeFor(nodeInfo)
          val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
          val possibleTypes = Seq(tpe)
          val node =
            parameterInNode(
              nodeInfo,
              nodeInfo.code,
              nodeInfo.code,
              index,
              isVariadic = false,
              EvaluationStrategies.BY_VALUE,
              typeFullName
            ).possibleTypes(possibleTypes)
          scope.addVariable(nodeInfo.code, node, MethodScope)
          node
      }
      val decoratorAsts = astsForDecorators(nodeInfo)
      decoratorAsts.foreach { decoratorAst =>
        Ast.storeInDiffGraph(decoratorAst, diffGraph)
        decoratorAst.root.foreach(diffGraph.addEdge(paramNode, _, EdgeTypes.AST))
      }
      paramNode
    }

  private def convertParamWithDefault(element: BabelNodeInfo): Ast = {
    val lhsElement = element.json("left")
    val rhsElement = element.json("right")
    val rhsAst     = astForNodeWithFunctionReference(rhsElement)
    val lhsAst     = astForNode(lhsElement)

    val testAst = {
      val keyNode = identifierNode(element, codeOf(lhsAst.nodes.head))
      val voidCallNode =
        callNode(element, "void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH)
      val equalsCallAst = createEqualsCallAst(Ast(keyNode), Ast(voidCallNode), element.lineNumber, element.columnNumber)
      equalsCallAst
    }

    val falseNode = identifierNode(element, codeOf(lhsAst.nodes.head))

    val ternaryNodeAst = createTernaryCallAst(testAst, rhsAst, Ast(falseNode), element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      lhsAst,
      ternaryNodeAst,
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
    val bindingNode  = newBindingNode("", "", "")
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
    val methodNode_ = methodNode(func, methodName, func.code, methodFullName, None, parserResult.filename)
    val lambdaModifier = if (methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix)) {
      newModifierNode(ModifierTypes.LAMBDA) :: Nil
    } else {
      Nil
    }
    val modifiers = newModifierNode(ModifierTypes.VIRTUAL) :: lambdaModifier
    methodAstParentStack.push(methodNode_)

    val thisNode =
      parameterInNode(func, "this", "this", 0, false, EvaluationStrategies.BY_VALUE)
        .dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariable("this", thisNode, MethodScope)

    val paramNodes = if (hasKey(func.json, "parameters")) {
      handleParameters(func.json("parameters").arr.toSeq, mutable.ArrayBuffer.empty[Ast], createLocals = false)
    } else {
      handleParameters(func.json("params").arr.toSeq, mutable.ArrayBuffer.empty[Ast], createLocals = false)
    }

    val methodReturnNode = createMethodReturnNode(func)

    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        func,
        methodNode_,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst = if (methodBlockContent.isEmpty) {
      methodStubAst(methodNode_, (thisNode +: paramNodes).map(Ast(_)), methodReturnNode, modifiers)
    } else {
      setArgumentIndices(methodBlockContent)
      val bodyAst = blockAst(NewBlock(), methodBlockContent)
      methodAstWithAnnotations(
        methodNode_,
        (thisNode +: paramNodes).map(Ast(_)),
        bodyAst,
        methodReturnNode,
        modifiers,
        astsForDecorators(func)
      )
    }

    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodNode_
  }

  protected def createMethodAstAndNode(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false,
    methodBlockContent: List[Ast] = List.empty
  ): MethodAst = {
    val (methodName, methodFullName) = calcMethodNameAndFullName(func)

    val methodRefNode_ = if (!shouldCreateFunctionReference) { None }
    else { Option(methodRefNode(func, methodName, methodFullName, methodFullName)) }

    val callAst = if (shouldCreateAssignmentCall && shouldCreateFunctionReference) {
      val idNode  = identifierNode(func, methodName)
      val idLocal = localNode(func, methodName, methodName, methodFullName).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(methodName, idLocal, BlockScope)
      scope.addVariableReference(methodName, idNode)
      val code       = s"function $methodName = ${func.code}"
      val assignment = createAssignmentCallAst(idNode, methodRefNode_.get, code, func.lineNumber, func.columnNumber)
      assignment
    } else {
      Ast()
    }

    val methodNode_ = methodNode(func, methodName, func.code, methodFullName, None, parserResult.filename)
    val lambdaModifier = if (methodName.startsWith(io.joern.x2cpg.Defines.ClosurePrefix)) {
      newModifierNode(ModifierTypes.LAMBDA) :: Nil
    } else {
      Nil
    }
    val modifierNodes = newModifierNode(ModifierTypes.VIRTUAL) :: lambdaModifier

    methodAstParentStack.push(methodNode_)

    val bodyJson                  = func.json("body")
    val bodyNodeInfo              = createBabelNodeInfo(bodyJson)
    val blockNode                 = createBlockNode(bodyNodeInfo)
    val additionalBlockStatements = mutable.ArrayBuffer.empty[Ast]

    val capturingRefNode = if (shouldCreateFunctionReference) { methodRefNode_ }
    else { typeRefIdStack.headOption }

    scope.pushNewMethodScope(methodFullName, methodName, blockNode, capturingRefNode)
    localAstParentStack.push(blockNode)

    val thisNode = parameterInNode(func, "this", "this", 0, false, EvaluationStrategies.BY_VALUE)
      .dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariable("this", thisNode, MethodScope)

    val paramNodes = handleParameters(func.json("params").arr.toSeq, additionalBlockStatements)

    val bodyStmtAsts = func.node match {
      case ArrowFunctionExpression =>
        bodyNodeInfo.node match {
          case BlockStatement =>
            // when body contains more than one statement, use bodyJson("body")) to avoid double Block node
            createBlockStatementAsts(bodyJson("body"))
          case _ =>
            // when body is just one expression like const foo = () => 42, generate a Return node
            val retCode = bodyNodeInfo.code.stripSuffix(";")
            returnAst(returnNode(bodyNodeInfo, retCode), List(astForNodeWithFunctionReference(bodyJson))) :: Nil
        }
      case _ => createBlockStatementAsts(bodyJson("body"))
    }
    val methodBlockChildren = methodBlockContent ++ additionalBlockStatements.toList ++ bodyStmtAsts
    setArgumentIndices(methodBlockChildren)

    val methodReturnNode = createMethodReturnNode(func)

    localAstParentStack.pop()
    scope.popScope()
    methodAstParentStack.pop()

    val functionTypeAndTypeDeclAst =
      createFunctionTypeAndTypeDeclAst(
        func,
        methodNode_,
        methodAstParentStack.head,
        methodName,
        methodFullName,
        parserResult.filename
      )

    val mAst =
      methodAstWithAnnotations(
        methodNode_,
        (thisNode +: paramNodes).map(Ast(_)),
        blockAst(blockNode, methodBlockChildren),
        methodReturnNode,
        modifierNodes,
        astsForDecorators(func)
      )
    Ast.storeInDiffGraph(mAst, diffGraph)
    Ast.storeInDiffGraph(functionTypeAndTypeDeclAst, diffGraph)
    diffGraph.addEdge(methodAstParentStack.head, methodNode_, EdgeTypes.AST)

    methodRefNode_ match {
      case Some(ref) if callAst.nodes.isEmpty =>
        MethodAst(Ast(ref), methodNode_, mAst)
      case _ =>
        MethodAst(callAst, methodNode_, mAst)
    }
  }

  protected def astForFunctionDeclaration(
    func: BabelNodeInfo,
    shouldCreateFunctionReference: Boolean = false,
    shouldCreateAssignmentCall: Boolean = false
  ): Ast = createMethodAstAndNode(func, shouldCreateFunctionReference, shouldCreateAssignmentCall).ast

}
