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
import ujson.Obj
import ujson.Value

import scala.util.Try

trait AstForDeclarationsCreator {

  this: AstCreator =>

  private val DEFAULTS_KEY    = "default"
  private val EXPORT_KEYWORD  = "exports"
  private val REQUIRE_KEYWORD = "require"
  private val IMPORT_KEYWORD  = "import"

  private def codeForExportObject(obj: BabelNodeInfo): Seq[String] = obj.node match {
    case BabelAst.VariableDeclaration    => obj.json("declarations").arr.toSeq.map(d => code(d("id")))
    case BabelAst.FunctionDeclaration    => Seq(code(obj.json("id")))
    case BabelAst.AssignmentExpression   => Seq(code(obj.json("left")))
    case BabelAst.ClassDeclaration       => Seq(code(obj.json("id")))
    case BabelAst.Identifier             => Seq(obj.code)
    case BabelAst.TSTypeAliasDeclaration => Seq(code(obj.json("id")))
    case BabelAst.TSInterfaceDeclaration => Seq(code(obj.json("id")))
    case BabelAst.TSEnumDeclaration      => Seq(code(obj.json("id")))
    case _ =>
      notHandledYet(obj, "Lowering export declaration")
      Seq.empty
  }

  private def createExportCallAst(name: String, exportName: String, declaration: BabelNodeInfo): Ast = {
    val exportCallAst = if (name == DEFAULTS_KEY) {
      createIndexAccessCallAst(
        createIdentifierNode(exportName, declaration),
        createLiteralNode(
          s"\"$DEFAULTS_KEY\"",
          Some(Defines.STRING.label),
          declaration.lineNumber,
          declaration.columnNumber
        ),
        declaration.lineNumber,
        declaration.columnNumber
      )
    } else {
      createFieldAccessCallAst(
        createIdentifierNode(exportName, declaration),
        createIdentifierNode(name, declaration),
        declaration.lineNumber,
        declaration.columnNumber
      )
    }
    Ast.storeInDiffGraph(exportCallAst, diffGraph)
    exportCallAst
  }

  private def createExportAssignmentCallAst(name: String, exportCallAst: Ast, declaration: BabelNodeInfo): Ast =
    createAssignmentCallAst(
      exportCallAst.nodes.head,
      createIdentifierNode(name, declaration),
      s"${codeOf(exportCallAst.nodes.head)} = $name",
      declaration.lineNumber,
      declaration.columnNumber
    )

  private def extractDeclarationsFromExportDecl(declaration: BabelNodeInfo): Option[(Ast, Seq[String])] =
    safeObj(declaration.json, "declaration")
      .map { d =>
        val nodeInfo = createBabelNodeInfo(d)
        val ast = nodeInfo.node match {
          case BabelAst.FunctionDeclaration =>
            astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true)
          case BabelAst.FunctionExpression =>
            astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true)
          case BabelAst.ArrowFunctionExpression =>
            astForFunctionDeclaration(nodeInfo, shouldCreateFunctionReference = true, shouldCreateAssignmentCall = true)
          case _ => astForNode(d)
        }
        val names = codeForExportObject(createBabelNodeInfo(Obj(d)))
        (ast, names)
      }

  private def extractExportFromNameFromExportDecl(declaration: BabelNodeInfo): String =
    safeObj(declaration.json, "source")
      .map { d => s"_${code(d).stripPrefix("\"").stripSuffix("\"")}" }
      .getOrElse(EXPORT_KEYWORD)

  private def createAstForFrom(exportName: String, declaration: BabelNodeInfo): Ast = {
    if (exportName == EXPORT_KEYWORD) {
      Ast()
    } else {
      val id        = createIdentifierNode(exportName, declaration)
      val localNode = createLocalNode(id.code, Defines.ANY.label)
      scope.addVariable(id.code, localNode, BlockScope)
      diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

      val destAst = Ast(id)
      val sourceCallArgNode =
        createLiteralNode(s"\"${exportName.stripPrefix("_")}\"", None, declaration.lineNumber, declaration.columnNumber)
      val sourceCall = createCallNode(
        s"$REQUIRE_KEYWORD(${sourceCallArgNode.code})",
        REQUIRE_KEYWORD,
        DispatchTypes.STATIC_DISPATCH,
        declaration.lineNumber,
        declaration.columnNumber
      )
      val sourceAst =
        createCallAst(sourceCall, List(Ast(sourceCallArgNode)))
      val assigmentCallAst =
        createAssignmentCallAst(
          destAst.nodes.head,
          sourceAst.nodes.head,
          s"${codeOf(destAst.nodes.head)} = ${codeOf(sourceAst.nodes.head)}",
          declaration.lineNumber,
          declaration.columnNumber
        )
      Ast.storeInDiffGraph(destAst, diffGraph)
      Ast.storeInDiffGraph(sourceAst, diffGraph)
      assigmentCallAst
    }
  }

  protected def astForExportNamedDeclaration(declaration: BabelNodeInfo): Ast = {
    val specifiers = declaration
      .json("specifiers")
      .arr
      .toSeq
      .map { spec =>
        val exported = createBabelNodeInfo(spec("exported"))
        val local = if (hasKey(spec, "local")) {
          createBabelNodeInfo(spec("local"))
        } else {
          exported
        }
        (local, exported)
      }

    val exportName = extractExportFromNameFromExportDecl(declaration)
    val fromAst    = createAstForFrom(exportName, declaration)

    val declAstAndNames = extractDeclarationsFromExportDecl(declaration)
    val declAsts = declAstAndNames.map { case (ast, names) =>
      ast +: names.map { name =>
        if (exportName != EXPORT_KEYWORD)
          diffGraph.addNode(createDependencyNode(name, exportName.stripPrefix("_"), REQUIRE_KEYWORD))
        val exportCallAst = createExportCallAst(name, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration)
      }
    }

    val specifierAst = specifiers.map { case (name, alias) =>
      if (exportName != EXPORT_KEYWORD)
        diffGraph.addNode(createDependencyNode(alias.code, exportName.stripPrefix("_"), REQUIRE_KEYWORD))
      val exportCallAst = createExportCallAst(alias.code, exportName, declaration)
      createExportAssignmentCallAst(name.code, exportCallAst, declaration)
    }

    val blockNode = createBlockNode(declaration.code, declaration.lineNumber, declaration.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val asts = fromAst +: (specifierAst ++ declAsts.toSeq.flatten)
    setIndices(asts.toList)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(asts)
  }

  protected def astForExportDefaultDeclaration(declaration: BabelNodeInfo): Ast = {
    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration)
    val declAsts = declAstAndNames.map { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(DEFAULTS_KEY, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration)
      }
    }
    val blockNode = createBlockNode(declaration.code, declaration.lineNumber, declaration.columnNumber)
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)
    val asts = declAsts.toSeq.flatten
    setIndices(asts.toList)
    localAstParentStack.pop()
    scope.popScope()
    Ast(blockNode).withChildren(asts)
  }

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
    val rhsCode  = code(rhs)
    val groupId  = rhsCode.substring(rhsCode.indexOf(s"$REQUIRE_KEYWORD(") + 9, rhsCode.indexOf(")") - 1)
    val nodeInfo = createBabelNodeInfo(lhs)
    val names = nodeInfo.node match {
      case BabelAst.ArrayPattern  => nodeInfo.json("elements").arr.toList.map(code)
      case BabelAst.ObjectPattern => nodeInfo.json("properties").arr.toList.map(code)
      case _                      => List(code(lhs))
    }
    names.foreach(name => diffGraph.addNode(createDependencyNode(name, groupId, REQUIRE_KEYWORD)))
  }

  private def astForVariableDeclarator(declarator: Value, scopeType: ScopeType): Ast = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f: BabelNodeInfo) if f.node == BabelAst.FunctionExpression =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case Some(f: BabelNodeInfo) if f.node == BabelAst.FunctionDeclaration =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case Some(f: BabelNodeInfo) if f.node == BabelAst.ArrowFunctionExpression =>
        val (_, methodFullName) = calcMethodNameAndFullName(f)
        methodFullName
      case _ => init.map(typeFor).getOrElse(Defines.ANY.label)
    }

    val localNode = createLocalNode(id.code, typeFullName)
    scope.addVariable(id.code, localNode, scopeType)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    if (init.isEmpty) {
      Ast()
    } else {
      val sourceAst = init.get match {
        case requireCall if requireCall.code.startsWith(s"$REQUIRE_KEYWORD(") =>
          handleRequireCallForDependencies(id.json, init.get.json)
          astForNodeWithFunctionReference(requireCall.json)
        case initExpr =>
          astForNodeWithFunctionReference(initExpr.json)
      }
      val nodeInfo = createBabelNodeInfo(id.json)
      nodeInfo.node match {
        case BabelAst.ObjectPattern =>
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          astForDeconstruction(nodeInfo, sourceAst)
        case BabelAst.ArrayPattern =>
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          astForDeconstruction(nodeInfo, sourceAst)
        case _ =>
          val destAst = astForNode(id.json)
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
      diffGraph.addNode(createDependencyNode(source, source, IMPORT_KEYWORD))
      createImportNodeAndAttachToAst(impDecl, source, source)
      Ast()
    } else {
      val depNodes = impDecl.json("specifiers").arr.map { importSpecifier =>
        val importedName = importSpecifier("local")("name").str
        createImportNodeAndAttachToAst(impDecl, source, importedName)
        createDependencyNode(importedName, source, IMPORT_KEYWORD)
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
    val rhsElement = element.json("right")
    val rhsAst     = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsAst, diffGraph)

    val lhsElement = element.json("left")
    val nodeInfo   = createBabelNodeInfo(lhsElement)
    val lhsAst = nodeInfo.node match {
      case BabelAst.ObjectPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        astForDeconstruction(nodeInfo, sourceAst)
      case BabelAst.ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        astForDeconstruction(nodeInfo, sourceAst)
      case _ => astForNode(lhsElement)
    }
    Ast.storeInDiffGraph(lhsAst, diffGraph)

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
    val rhsElement = element.json("right")
    val rhsAst     = astForNodeWithFunctionReference(rhsElement)
    Ast.storeInDiffGraph(rhsAst, diffGraph)

    val lhsElement = element.json("left")
    val nodeInfo   = createBabelNodeInfo(lhsElement)
    val lhsAst = nodeInfo.node match {
      case BabelAst.ObjectPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        astForDeconstruction(nodeInfo, sourceAst)
      case BabelAst.ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        Ast.storeInDiffGraph(sourceAst, diffGraph)
        astForDeconstruction(nodeInfo, sourceAst)
      case _ => astForNode(lhsElement)
    }
    Ast.storeInDiffGraph(lhsAst, diffGraph)

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
    val localTmpName = generateUnusedVariableName(usedVariableNames, "_tmp")

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

    val subTreeAsts = pattern.node match {
      case BabelAst.ObjectPattern =>
        pattern.json("properties").arr.toList.map { element =>
          val nodeInfo = createBabelNodeInfo(element)
          nodeInfo.node match {
            case BabelAst.RestElement => astForNode(nodeInfo.json)
            case _ =>
              val nodeInfo = createBabelNodeInfo(element("value"))
              nodeInfo.node match {
                case BabelAst.Identifier =>
                  convertDestructingObjectElement(nodeInfo, createBabelNodeInfo(element("key")), localTmpName)
                case BabelAst.AssignmentPattern =>
                  convertDestructingObjectElementWithDefault(
                    nodeInfo,
                    createBabelNodeInfo(element("key")),
                    localTmpName
                  )
                case _ => astForNode(nodeInfo.json)
              }
          }
        }
      case BabelAst.ArrayPattern =>
        pattern.json("elements").arr.toList.zipWithIndex.map {
          case (element, index) if !element.isNull =>
            val nodeInfo = createBabelNodeInfo(element)
            nodeInfo.node match {
              case BabelAst.Identifier =>
                convertDestructingArrayElement(nodeInfo, index, localTmpName)
              case BabelAst.AssignmentPattern =>
                convertDestructingArrayElementWithDefault(nodeInfo, index, localTmpName)
              case _ => astForNode(nodeInfo.json)
            }
          case _ => Ast()
        }
      case _ =>
        List(convertDestructingObjectElement(pattern, pattern, localTmpName))
    }

    val returnTmpNode = createIdentifierNode(localTmpName, pattern)
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = assignmentTmpCallAst +: subTreeAsts :+ Ast(returnTmpNode)
    setIndices(blockChildren)
    Ast(blockNode).withChildren(blockChildren)
  }

}
