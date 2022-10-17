package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.jssrc2cpg.datastructures.MethodScope
import io.joern.jssrc2cpg.datastructures.ScopeType
import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.joern.x2cpg.datastructures.Stack._
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.{IdentifierBase, NewImport, NewNamespaceBlock, TypeRefBase}
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import ujson.Value

import scala.util.Try

trait AstForDeclarationsCreator { this: AstCreator =>

  private val DEFAULTS_KEY    = "default"
  private val EXPORT_KEYWORD  = "exports"
  private val REQUIRE_KEYWORD = "require"
  private val IMPORT_KEYWORD  = "import"

  private def hasNoName(json: Value): Boolean = !hasKey(json, "id") || json("id").isNull

  protected def codeForExportObject(obj: BabelNodeInfo, defaultName: Option[String]): Seq[String] = obj.node match {
    case Identifier                                 => Seq(obj.code)
    case VariableDeclaration                        => obj.json("declarations").arr.toSeq.map(d => code(d("id")))
    case AssignmentExpression                       => Seq(code(obj.json("left")))
    case ClassDeclaration                           => Seq(code(obj.json("id")))
    case TSTypeAliasDeclaration                     => Seq(code(obj.json("id")))
    case TSInterfaceDeclaration                     => Seq(code(obj.json("id")))
    case TSEnumDeclaration                          => Seq(code(obj.json("id")))
    case TSModuleDeclaration                        => Seq(code(obj.json("id")))
    case TSDeclareFunction if hasNoName(obj.json)   => Seq(code(obj.json("id")))
    case TSDeclareFunction                          => defaultName.toSeq
    case FunctionDeclaration if hasNoName(obj.json) => Seq(code(obj.json("id")))
    case FunctionDeclaration                        => defaultName.toSeq
    case FunctionExpression if hasNoName(obj.json)  => Seq(code(obj.json("id")))
    case FunctionExpression                         => defaultName.toSeq
    case ClassExpression if hasNoName(obj.json)     => Seq(code(obj.json("id")))
    case ClassExpression                            => defaultName.toSeq
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
        createFieldIdentifierNode(name, declaration.lineNumber, declaration.columnNumber),
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

  private def extractDeclarationsFromExportDecl(declaration: BabelNodeInfo, key: String): Option[(Ast, Seq[String])] =
    safeObj(declaration.json, key)
      .map { d =>
        val nodeInfo = createBabelNodeInfo(d)
        val ast      = astForNodeWithFunctionReferenceAndCall(d)
        val defaultName = ast.nodes.collectFirst {
          case id: IdentifierBase =>
            // we will have the Identifier in the assignment call generated for a function (see above)
            id.name
          case clazz: TypeRefBase =>
            // we will have a TypeRef for an exported class
            clazz.code.stripPrefix("class ")
        }
        val names = codeForExportObject(nodeInfo, defaultName)
        (ast, names)
      }

  private def extractExportFromNameFromExportDecl(declaration: BabelNodeInfo): String =
    safeObj(declaration.json, "source")
      .map { d => s"_${code(d).stripPrefix("\"").stripSuffix("\"")}" }
      .getOrElse(EXPORT_KEYWORD)

  private def cleanImportName(name: String): String = if (name.contains("/")) {
    val stripped = name.stripSuffix("/")
    stripped.substring(stripped.lastIndexOf("/") + 1)
  } else name

  private def createAstForFrom(fromName: String, declaration: BabelNodeInfo): Ast = {
    if (fromName == EXPORT_KEYWORD) {
      Ast()
    } else {
      val strippedCode = cleanImportName(fromName).stripPrefix("_")
      val id           = createIdentifierNode(s"_$strippedCode", declaration)
      val localNode    = createLocalNode(id.code, Defines.ANY.label)
      scope.addVariable(id.code, localNode, BlockScope)
      diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

      val destAst = Ast(id)
      val sourceCallArgNode =
        createLiteralNode(s"\"${fromName.stripPrefix("_")}\"", None, declaration.lineNumber, declaration.columnNumber)
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
          s"var ${codeOf(destAst.nodes.head)} = ${codeOf(sourceAst.nodes.head)}",
          declaration.lineNumber,
          declaration.columnNumber
        )
      Ast.storeInDiffGraph(sourceAst, diffGraph)
      assigmentCallAst
    }
  }

  protected def astForExportNamedDeclaration(declaration: BabelNodeInfo): Ast = {
    val specifiers = declaration
      .json("specifiers")
      .arr
      .toList
      .map { spec =>
        if (createBabelNodeInfo(spec).node == ExportNamespaceSpecifier) {
          val exported = createBabelNodeInfo(spec("exported"))
          (None, Some(exported))
        } else {
          val exported = createBabelNodeInfo(spec("exported"))
          val local = if (hasKey(spec, "local")) {
            createBabelNodeInfo(spec("local"))
          } else {
            exported
          }
          (Some(local), Some(exported))
        }
      }

    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val fromAst         = createAstForFrom(exportName, declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration, "declaration")
    val declAsts = declAstAndNames.toList.map { case (ast, names) =>
      ast +: names.map { name =>
        if (exportName != EXPORT_KEYWORD)
          diffGraph.addNode(createDependencyNode(name, exportName.stripPrefix("_"), REQUIRE_KEYWORD))
        val exportCallAst = createExportCallAst(name, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration)
      }
    }

    val specifierAsts = specifiers.map {
      case (Some(name), Some(alias)) =>
        if (exportName != EXPORT_KEYWORD)
          diffGraph.addNode(createDependencyNode(alias.code, exportName.stripPrefix("_"), REQUIRE_KEYWORD))
        val exportCallAst = createExportCallAst(alias.code, exportName, declaration)
        createExportAssignmentCallAst(name.code, exportCallAst, declaration)
      case (None, Some(alias)) =>
        diffGraph.addNode(createDependencyNode(alias.code, exportName.stripPrefix("_"), REQUIRE_KEYWORD))
        val exportCallAst = createExportCallAst(alias.code, EXPORT_KEYWORD, declaration)
        createExportAssignmentCallAst(exportName, exportCallAst, declaration)
    }

    val asts = fromAst +: (specifierAsts ++ declAsts.flatten)
    setIndices(asts)
    blockAst(createBlockNode(declaration), asts)
  }

  protected def astForExportAssignment(assignment: BabelNodeInfo): Ast = {
    val expressionAstWithNames = extractDeclarationsFromExportDecl(assignment, "expression")
    val declAsts = expressionAstWithNames.map { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(name, EXPORT_KEYWORD, assignment)
        createExportAssignmentCallAst(name, exportCallAst, assignment)
      }
    }

    val asts = declAsts.toList.flatten
    setIndices(asts)
    blockAst(createBlockNode(assignment), asts)
  }

  protected def astForExportDefaultDeclaration(declaration: BabelNodeInfo): Ast = {
    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration, "declaration")

    val declAsts = declAstAndNames.map { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(DEFAULTS_KEY, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration)
      }
    }

    val asts = declAsts.toList.flatten
    setIndices(asts)
    blockAst(createBlockNode(declaration), asts)
  }

  protected def astForExportAllDeclaration(declaration: BabelNodeInfo): Ast = {
    val exportName = extractExportFromNameFromExportDecl(declaration)
    val depGroupId = code(declaration.json("source")).stripPrefix("\"").stripSuffix("\"")
    val name       = cleanImportName(depGroupId)
    if (exportName != EXPORT_KEYWORD) {
      diffGraph.addNode(createDependencyNode(name, depGroupId, REQUIRE_KEYWORD))
    }

    val fromCallAst   = createAstForFrom(exportName, declaration)
    val exportCallAst = createExportCallAst(name, EXPORT_KEYWORD, declaration)
    Ast.storeInDiffGraph(exportCallAst, diffGraph)
    val assignmentCallAst = createExportAssignmentCallAst(s"_$name", exportCallAst, declaration)

    val asts = List(fromCallAst, exportCallAst, assignmentCallAst)
    setIndices(asts)
    blockAst(createBlockNode(declaration), asts)
  }

  protected def astForVariableDeclaration(declaration: BabelNodeInfo): Ast = {
    val kind = declaration.json("kind").str
    val scopeType = if (kind == "let") {
      BlockScope
    } else {
      MethodScope
    }
    val declAsts = declaration.json("declarations").arr.toList.map(astForVariableDeclarator(_, scopeType, kind))
    declAsts match {
      case head :: tail =>
        setIndices(declAsts)
        tail.foreach { declAst =>
          declAst.root.foreach(diffGraph.addEdge(localAstParentStack.head, _, EdgeTypes.AST))
          Ast.storeInDiffGraph(declAst, diffGraph)
        }
        head
      case Nil => Ast()
    }
  }

  private def handleRequireCallForDependencies(declarator: BabelNodeInfo, lhs: Value, rhs: Value): Unit = {
    val rhsCode  = code(rhs)
    val groupId  = rhsCode.substring(rhsCode.indexOf(s"$REQUIRE_KEYWORD(") + 9, rhsCode.indexOf(")") - 1)
    val nodeInfo = createBabelNodeInfo(lhs)
    val names = nodeInfo.node match {
      case ArrayPattern  => nodeInfo.json("elements").arr.toList.map(code)
      case ObjectPattern => nodeInfo.json("properties").arr.toList.map(code)
      case _             => List(code(lhs))
    }
    names.foreach { name =>
      val dependencyNode = createDependencyNode(name, groupId, REQUIRE_KEYWORD)
      diffGraph.addNode(dependencyNode)
      val importNode = createImportNodeAndAttachToAst(declarator, groupId, name)
      diffGraph.addEdge(importNode, dependencyNode, EdgeTypes.IMPORTS)
    }
  }

  private def astForVariableDeclarator(declarator: Value, scopeType: ScopeType, kind: String): Ast = {
    val id   = createBabelNodeInfo(declarator("id"))
    val init = Try(createBabelNodeInfo(declarator("init"))).toOption

    val typeFullName = init match {
      case Some(f @ BabelNodeInfo(_: FunctionLike, _, _, _, _, _, _)) =>
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
          handleRequireCallForDependencies(createBabelNodeInfo(declarator), id.json, init.get.json)
          astForNodeWithFunctionReference(requireCall.json)
        case initExpr =>
          astForNodeWithFunctionReference(initExpr.json)
      }
      val nodeInfo = createBabelNodeInfo(id.json)
      nodeInfo.node match {
        case ObjectPattern | ArrayPattern =>
          astForDeconstruction(nodeInfo, sourceAst)
        case _ =>
          val destAst = id.node match {
            case Identifier => astForIdentifier(id, Some(typeFullName))
            case _          => astForNode(id.json)
          }

          val assigmentCallAst =
            createAssignmentCallAst(
              destAst.nodes.head,
              sourceAst.nodes.head,
              s"$kind ${code(declarator)}",
              line = line(declarator),
              column = column(declarator)
            )
          Ast.storeInDiffGraph(destAst, diffGraph)
          Ast.storeInDiffGraph(sourceAst, diffGraph)
          assigmentCallAst
      }
    }
  }

  protected def astForTSImportEqualsDeclaration(impDecl: BabelNodeInfo): Ast = {
    val name          = impDecl.json("id")("name").str
    val referenceNode = createBabelNodeInfo(impDecl.json("moduleReference"))
    val referenceName = referenceNode.node match {
      case TSExternalModuleReference => referenceNode.json("expression")("value").str
      case _                         => referenceNode.code
    }
    val dependencyNode = createDependencyNode(name, referenceName, IMPORT_KEYWORD)
    diffGraph.addNode(dependencyNode)
    val importNode = createImportNodeAndAttachToAst(impDecl, referenceName, name)
    diffGraph.addEdge(importNode, dependencyNode, EdgeTypes.IMPORTS)
    astForRequireCallFromImport(name, None, referenceName, isImportN = false, impDecl)
  }

  private def astForRequireCallFromImport(
    name: String,
    alias: Option[String],
    from: String,
    isImportN: Boolean,
    nodeInfo: BabelNodeInfo
  ): Ast = {
    val destName  = alias.getOrElse(name)
    val destNode  = createIdentifierNode(destName, nodeInfo)
    val localNode = createLocalNode(destName, Defines.ANY.label)
    scope.addVariable(destName, localNode, BlockScope)
    diffGraph.addEdge(localAstParentStack.head, localNode, EdgeTypes.AST)

    val destAst           = Ast(destNode)
    val sourceCallArgNode = createLiteralNode(s"\"$from\"", None, nodeInfo.lineNumber, nodeInfo.columnNumber)
    val sourceCall = createCallNode(
      s"$REQUIRE_KEYWORD(${sourceCallArgNode.code})",
      REQUIRE_KEYWORD,
      DispatchTypes.STATIC_DISPATCH,
      nodeInfo.lineNumber,
      nodeInfo.columnNumber
    )

    val sourceAst = if (isImportN) {
      val callAst = createCallAst(sourceCall, List(Ast(sourceCallArgNode)))
      Ast.storeInDiffGraph(callAst, diffGraph)
      val fieldAccessCall = createFieldAccessCallAst(
        callAst.nodes.head,
        createFieldIdentifierNode(name, nodeInfo.lineNumber, nodeInfo.columnNumber),
        nodeInfo.lineNumber,
        nodeInfo.columnNumber
      )
      Ast.storeInDiffGraph(fieldAccessCall, diffGraph)
      fieldAccessCall
    } else {
      val callAst = createCallAst(sourceCall, List(Ast(sourceCallArgNode)))
      Ast.storeInDiffGraph(callAst, diffGraph)
      callAst
    }
    val assigmentCallAst =
      createAssignmentCallAst(
        destAst.nodes.head,
        sourceAst.nodes.head,
        s"var ${codeOf(destAst.nodes.head)} = ${codeOf(sourceAst.nodes.head)}",
        nodeInfo.lineNumber,
        nodeInfo.columnNumber
      )
    assigmentCallAst
  }

  protected def astForImportDeclaration(impDecl: BabelNodeInfo): Ast = {
    val source     = impDecl.json("source")("value").str
    val specifiers = impDecl.json("specifiers").arr

    if (specifiers.isEmpty) {
      val dependencyNode = createDependencyNode(source, source, IMPORT_KEYWORD)
      diffGraph.addNode(dependencyNode)
      val importNode = createImportNodeAndAttachToAst(impDecl, source, source)
      diffGraph.addEdge(importNode, dependencyNode, EdgeTypes.IMPORTS)
      astForRequireCallFromImport(source, None, source, isImportN = false, impDecl)
    } else {
      val specs = impDecl.json("specifiers").arr.toList
      val depNodes = specs.map { importSpecifier =>
        val importedName = importSpecifier("local")("name").str
        val importNode = createImportNodeAndAttachToAst(impDecl, source, importedName)
        val dependencyNode = createDependencyNode(importedName, source, IMPORT_KEYWORD)
        diffGraph.addEdge(importNode, dependencyNode, EdgeTypes.IMPORTS)
        dependencyNode
      }
      depNodes.foreach(diffGraph.addNode)
      val requireCalls = specs.map { importSpecifier =>
        val name = importSpecifier("local")("name").str
        val isImportN = createBabelNodeInfo(importSpecifier).node match {
          case ImportSpecifier => true
          case _               => false
        }
        val (alias, reqName) = if (hasKey(importSpecifier, "imported")) {
          (Some(name), importSpecifier("imported")("name").str)
        } else {
          (None, name)
        }
        astForRequireCallFromImport(reqName, alias, source, isImportN = isImportN, impDecl)
      }
      if (requireCalls.isEmpty) {
        Ast()
      } else if (requireCalls.size == 1) {
        requireCalls.head
      } else {
        blockAst(createBlockNode(impDecl), requireCalls)
      }
    }
  }

  private def createImportNodeAndAttachToAst(
    impDecl: BabelNodeInfo,
    importedEntity: String,
    importedAs: String
  ): NewImport = {
    val impNode = createImportNode(impDecl, Some(importedEntity).filter(_.trim.nonEmpty), importedAs)
    methodAstParentStack.collectFirst { case namespaceBlockNode: NewNamespaceBlock =>
      diffGraph.addEdge(namespaceBlockNode, impNode, EdgeTypes.AST)
    }
    impNode
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
      case ObjectPattern | ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
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
      val voidCallNode = createVoidCallNode(element.lineNumber, element.columnNumber)
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
      case ObjectPattern | ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
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
      val voidCallNode = createVoidCallNode(element.lineNumber, element.columnNumber)
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

    val blockNode = createBlockNode(pattern)
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
      case ObjectPattern =>
        pattern.json("properties").arr.toList.map { element =>
          val nodeInfo = createBabelNodeInfo(element)
          nodeInfo.node match {
            case RestElement =>
              val restElementNodeInfo = createBabelNodeInfo(nodeInfo.json("argument"))
              convertDestructingObjectElement(restElementNodeInfo, restElementNodeInfo, localTmpName)
            case _ =>
              val nodeInfo = createBabelNodeInfo(element("value"))
              nodeInfo.node match {
                case Identifier =>
                  convertDestructingObjectElement(nodeInfo, createBabelNodeInfo(element("key")), localTmpName)
                case AssignmentPattern =>
                  convertDestructingObjectElementWithDefault(
                    nodeInfo,
                    createBabelNodeInfo(element("key")),
                    localTmpName
                  )
                case _ => astForNode(nodeInfo.json)
              }
          }
        }
      case ArrayPattern =>
        pattern.json("elements").arr.toList.zipWithIndex.map {
          case (element, index) if !element.isNull =>
            val nodeInfo = createBabelNodeInfo(element)
            nodeInfo.node match {
              case RestElement =>
                val restElementNodeInfo = createBabelNodeInfo(nodeInfo.json("argument"))
                convertDestructingArrayElement(restElementNodeInfo, index, localTmpName)
              case Identifier =>
                convertDestructingArrayElement(nodeInfo, index, localTmpName)
              case AssignmentPattern =>
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
