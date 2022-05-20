package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.jssrc2cpg.datastructures.MethodScope
import io.joern.jssrc2cpg.datastructures.ScopeType
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Value

import scala.util.Try

trait AstForDeclarationsCreator {

  this: AstCreator =>

  protected def astForVariableDeclaration(declaration: BabelNodeInfo): Ast = {
    val scopeType = if (declaration.json("kind").str == "let") {
      BlockScope
    } else {
      MethodScope
    }
    declaration.json("declarations").arr.foldLeft(Ast()) { (ast, d) =>
      ast.merge(astForVariableDeclarator(d, scopeType))
    }
  }

  private def handleRequireCallForDependencies(lhs: Value, rhs: Value): Unit = {
    val rhsCode = code(rhs)
    val groupId = rhsCode.substring(rhsCode.indexOf("require(") + 9, rhsCode.indexOf(")") - 1)
    val names = createBabelNodeInfo(lhs) match {
      case arrayPattern @ BabelNodeInfo(BabelAst.ArrayPattern) =>
        arrayPattern.json("elements").arr.toList.map(code)
      case objectPattern @ BabelNodeInfo(BabelAst.ObjectPattern) =>
        objectPattern.json("properties").arr.toList.map(code)
      case _ => List(code(lhs))
    }
    names.foreach(name => diffGraph.addNode(createDependencyNode(name, groupId, "require")))
  }

  private def astForVariableDeclarator(declarator: Value, scopeType: ScopeType): Ast = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(BabelAst.FunctionExpression)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case Some(f @ BabelNodeInfo(BabelAst.FunctionDeclaration)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case Some(f @ BabelNodeInfo(BabelAst.ArrowFunctionExpression)) =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => Defines.ANY.label
    }

    val localNode = createLocalNode(id.code, typeFullName)
    scope.addVariable(id.code, localNode, scopeType)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    if (init.isEmpty) {
      Ast()
    } else {
      createBabelNodeInfo(id.json) match {
        case objPattern @ BabelNodeInfo(BabelAst.ObjectPattern) =>
          val sourceAst = init.get match {
            case requireCall if requireCall.code.startsWith("require(") =>
              handleRequireCallForDependencies(id.json, init.get.json)
              astForNodeWithFunctionReference(requireCall.json)
            case initExpr =>
              astForNodeWithFunctionReference(initExpr.json)
          }
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          astForDeconstruction(objPattern, sourceAst)
        case arrPattern @ BabelNodeInfo(BabelAst.ArrayPattern) =>
          val sourceAst = init.get match {
            case requireCall if requireCall.code.startsWith("require(") =>
              handleRequireCallForDependencies(id.json, init.get.json)
              astForNodeWithFunctionReference(requireCall.json)
            case initExpr =>
              astForNodeWithFunctionReference(initExpr.json)
          }
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          astForDeconstruction(arrPattern, sourceAst)
        case _ =>
          val destAst = astForNode(id.json)
          val sourceAst = init.get match {
            case requireCall if requireCall.code.startsWith("require(") =>
              handleRequireCallForDependencies(id.json, init.get.json)
              astForNodeWithFunctionReference(requireCall.json)
            case initExpr =>
              astForNodeWithFunctionReference(initExpr.json)
          }
          val assigmentCallAst =
            createAssignmentCallAst(
              destAst.nodes.head,
              sourceAst.nodes.head,
              code(declarator),
              line = line(declarator),
              column = column(declarator)
            )
          Ast.storeInDiffGraph(destAst, diffGraph)
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          assigmentCallAst
      }
    }
  }

  protected def astForImportDeclaration(impDecl: BabelNodeInfo): Ast = {
    val source     = impDecl.json("source")("value").str
    val specifiers = impDecl.json("specifiers").arr

    if (specifiers.isEmpty) {
      diffGraph.addNode(createDependencyNode(source, source, "import"))
      createImportNodeAndAttachToAst(impDecl, source, source)
      Ast()
    } else {
      val depNodes = impDecl.json("specifiers").arr.map { importSpecifier =>
        val importedName = importSpecifier("local")("name").str
        createImportNodeAndAttachToAst(impDecl, source, importedName)
        createDependencyNode(importedName, source, "import")
      }
      depNodes.foreach(diffGraph.addNode)
      Ast()
    }
  }

  private def createImportNodeAndAttachToAst(
    impDecl: BabelNodeInfo,
    importedEntity: String,
    importedAs: String
  ): Unit = {
    val impNode = createImportNode(impDecl, Some(importedEntity).filter(_.trim.nonEmpty), importedAs)
    methodAstParentStack.collectFirst { case namespaceBlockNode: NewNamespaceBlock =>
      diffGraph.addEdge(namespaceBlockNode, impNode, EdgeTypes.AST)
    }
  }

  private def convertDestructingObjectElement(element: BabelNodeInfo, key: BabelNodeInfo, localTmpName: String): Ast = {
    val valueAst = astForNode(element.json)
    Ast.storeInDiffGraph(valueAst, diffGraph)
    val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
    val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
    val accessAst = createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    Ast.storeInDiffGraph(accessAst, diffGraph)
    createAssignmentCallAst(
      valueAst.nodes.head,
      accessAst.nodes.head,
      s"${codeOf(valueAst.nodes.head)} = ${codeOf(accessAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingArrayElement(element: BabelNodeInfo, index: Int, localTmpName: String): Ast = {
    val valueAst = astForNode(element.json)
    Ast.storeInDiffGraph(valueAst, diffGraph)
    val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
    val keyNode =
      createLiteralNode(index.toString, Some(Defines.NUMBER.label), element.lineNumber, element.columnNumber)
    val accessAst = createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    Ast.storeInDiffGraph(accessAst, diffGraph)
    createAssignmentCallAst(
      valueAst.nodes.head,
      accessAst.nodes.head,
      s"${codeOf(valueAst.nodes.head)} = ${codeOf(accessAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingArrayElementWithDefault(
    element: BabelNodeInfo,
    index: Int,
    localTmpName: String
  ): Ast = {
    val lhsElement = element.json("left")
    val rhsElement = element.json("right")
    val lhsAst     = astForNode(lhsElement)
    Ast.storeInDiffGraph(lhsAst, diffGraph)
    val rhsAst = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsAst, diffGraph)
    val testAst = {
      val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
      val keyNode =
        createLiteralNode(index.toString, Some(Defines.NUMBER.label), element.lineNumber, element.columnNumber)
      val accessAst =
        createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      Ast.storeInDiffGraph(accessAst, diffGraph)
      val voidCallNode = createCallNode(
        "void 0",
        "<operator>.void",
        DispatchTypes.STATIC_DISPATCH,
        element.lineNumber,
        element.columnNumber
      )
      createEqualsCallAst(accessAst.nodes.head, voidCallNode, element.lineNumber, element.columnNumber)
    }
    Ast.storeInDiffGraph(testAst, diffGraph)
    val falseAst = {
      val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
      val keyNode =
        createLiteralNode(index.toString, Some(Defines.NUMBER.label), element.lineNumber, element.columnNumber)
      createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    }
    Ast.storeInDiffGraph(falseAst, diffGraph)
    val ternaryNodeAst =
      createTernaryCallAst(
        testAst.nodes.head,
        rhsAst.nodes.head,
        falseAst.nodes.head,
        element.lineNumber,
        element.columnNumber
      )
    Ast.storeInDiffGraph(ternaryNodeAst, diffGraph)
    createAssignmentCallAst(
      lhsAst.nodes.head,
      ternaryNodeAst.nodes.head,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  protected def convertDestructingObjectElementWithDefault(
    element: BabelNodeInfo,
    key: BabelNodeInfo,
    localTmpName: String
  ): Ast = {
    val lhsElement = element.json("left")
    val rhsElement = element.json("right")
    val lhsAst     = astForNode(lhsElement)
    Ast.storeInDiffGraph(lhsAst, diffGraph)
    val rhsAst = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsAst, diffGraph)
    val testAst = {
      val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
      val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
      val accessAst =
        createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      Ast.storeInDiffGraph(accessAst, diffGraph)
      val voidCallNode = createCallNode(
        "void 0",
        "<operator>.void",
        DispatchTypes.STATIC_DISPATCH,
        element.lineNumber,
        element.columnNumber
      )
      createEqualsCallAst(accessAst.nodes.head, voidCallNode, element.lineNumber, element.columnNumber)
    }
    Ast.storeInDiffGraph(testAst, diffGraph)
    val falseAst = {
      val fieldAccessTmpNode = createIdentifierNode(localTmpName, element)
      val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
      createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    }
    Ast.storeInDiffGraph(falseAst, diffGraph)
    val ternaryNodeAst =
      createTernaryCallAst(
        testAst.nodes.head,
        rhsAst.nodes.head,
        falseAst.nodes.head,
        element.lineNumber,
        element.columnNumber
      )
    Ast.storeInDiffGraph(ternaryNodeAst, diffGraph)
    createAssignmentCallAst(
      lhsAst.nodes.head,
      ternaryNodeAst.nodes.head,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def createParamAst(pattern: BabelNodeInfo, keyName: String, sourceAst: Ast): Ast = {
    val testAst = {
      val lhsNode = createIdentifierNode(keyName, pattern)
      scope.addVariableReference(keyName, lhsNode)
      val rhsNode = createCallNode(
        "void 0",
        "<operator>.void",
        DispatchTypes.STATIC_DISPATCH,
        pattern.lineNumber,
        pattern.columnNumber
      )
      createEqualsCallAst(lhsNode, rhsNode, pattern.lineNumber, pattern.columnNumber)
    }
    Ast.storeInDiffGraph(testAst, diffGraph)

    val falseNode = {
      val initNode = createIdentifierNode(keyName, pattern)
      scope.addVariableReference(keyName, initNode)
      initNode
    }
    createTernaryCallAst(testAst.nodes.head, sourceAst.nodes.head, falseNode, pattern.lineNumber, pattern.columnNumber)
  }

  protected def astForDeconstruction(pattern: BabelNodeInfo, sourceAst: Ast, paramName: Option[String] = None): Ast = {
    val localTmpName = generateUnusedVariableName(usedVariableNames, Set.empty, "_tmp")

    val blockNode = createBlockNode(pattern.code, pattern.lineNumber, pattern.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val localNode = createLocalNode(localTmpName, Defines.ANY.label)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    val tmpNode = createIdentifierNode(localTmpName, pattern)

    val rhsAssignmentAst = paramName.map(createParamAst(pattern, _, sourceAst)).getOrElse(sourceAst)
    Ast.storeInDiffGraph(rhsAssignmentAst, diffGraph)
    val assignmentTmpCallAst =
      createAssignmentCallAst(
        tmpNode,
        rhsAssignmentAst.nodes.head,
        s"$localTmpName = ${codeOf(rhsAssignmentAst.nodes.head)}",
        pattern.lineNumber,
        pattern.columnNumber
      )

    val subTreeAsts = pattern match {
      case BabelNodeInfo(BabelAst.ObjectPattern) =>
        pattern.json("properties").arr.toList.map { element =>
          createBabelNodeInfo(element) match {
            case rest @ BabelNodeInfo(BabelAst.RestElement) => astForNode(rest.json)
            case _ =>
              createBabelNodeInfo(element("value")) match {
                case ident @ BabelNodeInfo(BabelAst.Identifier) =>
                  convertDestructingObjectElement(ident, createBabelNodeInfo(element("key")), localTmpName)
                case assignment @ BabelNodeInfo(BabelAst.AssignmentPattern) =>
                  convertDestructingObjectElementWithDefault(
                    assignment,
                    createBabelNodeInfo(element("key")),
                    localTmpName
                  )
                case other => astForNode(other.json)
              }
          }
        }
      case BabelNodeInfo(BabelAst.ArrayPattern) =>
        pattern.json("elements").arr.toList.zipWithIndex.map {
          case (element, index) if !element.isNull =>
            createBabelNodeInfo(element) match {
              case ident @ BabelNodeInfo(BabelAst.Identifier) =>
                convertDestructingArrayElement(ident, index, localTmpName)
              case assignment @ BabelNodeInfo(BabelAst.AssignmentPattern) =>
                convertDestructingArrayElementWithDefault(assignment, index, localTmpName)
              case other => astForNode(other.json)
            }
          case _ => Ast()
        }
      case other =>
        List(convertDestructingObjectElement(other, other, localTmpName))
    }

    val returnTmpNode = createIdentifierNode(localTmpName, pattern)
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = assignmentTmpCallAst +: subTreeAsts :+ Ast(returnTmpNode)
    setIndices(blockChildren)
    Ast(blockNode).withChildren(blockChildren)
  }

}
