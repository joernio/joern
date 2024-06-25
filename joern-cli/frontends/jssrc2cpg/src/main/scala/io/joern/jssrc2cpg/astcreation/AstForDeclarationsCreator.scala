package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.{BlockScope, MethodScope, ScopeType}
import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.NodeBuilders.newDependencyNode
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewImport}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes}
import ujson.Value

import scala.util.Try

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val DefaultsKey    = "default"
  private val ExportKeyword  = "exports"
  private val RequireKeyword = "require"
  private val ImportKeyword  = "import"

  private def hasName(json: Value): Boolean = hasKey(json, "id") && !json("id").isNull

  protected def codeForBabelNodeInfo(obj: BabelNodeInfo): Seq[String] = {
    val codes = obj.node match {
      case Identifier                               => Seq(obj.code)
      case NumericLiteral                           => Seq(obj.code)
      case StringLiteral                            => Seq(obj.code)
      case AssignmentExpression                     => Seq(code(obj.json("left")))
      case ClassDeclaration                         => Seq(code(obj.json("id")))
      case TSTypeAliasDeclaration                   => Seq(code(obj.json("id")))
      case TSInterfaceDeclaration                   => Seq(code(obj.json("id")))
      case TSEnumDeclaration                        => Seq(code(obj.json("id")))
      case TSModuleDeclaration                      => Seq(code(obj.json("id")))
      case TSDeclareFunction if hasName(obj.json)   => Seq(obj.json("id")("name").str)
      case FunctionDeclaration if hasName(obj.json) => Seq(obj.json("id")("name").str)
      case FunctionExpression if hasName(obj.json)  => Seq(obj.json("id")("name").str)
      case ClassExpression if hasName(obj.json)     => Seq(obj.json("id")("name").str)
      case VariableDeclarator if hasName(obj.json) =>
        createBabelNodeInfo(obj.json("id")).node match {
          case ArrayPattern =>
            obj.json("id")("elements").arr.toSeq.map(createBabelNodeInfo).map(_.code)
          case ObjectPattern =>
            obj.json("id")("properties").arr.toSeq.flatMap(p => codeForBabelNodeInfo(createBabelNodeInfo(p)))
          case _ =>
            Seq(obj.json("id")("name").str)
        }
      case VariableDeclarator => Seq(code(obj.json("id")))
      case MemberExpression   => Seq(code(obj.json("property")))
      case ObjectProperty     => Seq(code(obj.json("key")))
      case ObjectExpression =>
        obj.json("properties").arr.toSeq.flatMap(d => codeForBabelNodeInfo(createBabelNodeInfo(d)))
      case VariableDeclaration =>
        obj.json("declarations").arr.toSeq.flatMap(d => codeForBabelNodeInfo(createBabelNodeInfo(d)))
      case _ => Seq.empty
    }
    codes.map(_.replace("...", ""))
  }

  private def createExportCallAst(name: String, exportName: String, declaration: BabelNodeInfo): Ast = {
    val exportCallAst = if (name == DefaultsKey) {
      createIndexAccessCallAst(
        identifierNode(declaration, exportName),
        literalNode(declaration, s"\"$DefaultsKey\"", Option(Defines.String)),
        declaration.lineNumber,
        declaration.columnNumber
      )
    } else {
      createFieldAccessCallAst(
        identifierNode(declaration, exportName),
        createFieldIdentifierNode(name, declaration.lineNumber, declaration.columnNumber),
        declaration.lineNumber,
        declaration.columnNumber
      )
    }
    exportCallAst
  }

  private def createExportAssignmentCallAst(
    name: String,
    exportCallAst: Ast,
    declaration: BabelNodeInfo,
    from: Option[String]
  ): Ast = {
    from match {
      case Some(value) =>
        val call = createFieldAccessCallAst(
          identifierNode(declaration, value, Seq.empty),
          createFieldIdentifierNode(name, declaration.lineNumber, declaration.columnNumber),
          declaration.lineNumber,
          declaration.columnNumber
        )
        createAssignmentCallAst(
          exportCallAst,
          call,
          s"${codeOf(exportCallAst.nodes.head)} = ${codeOf(call.nodes.head)}",
          declaration.lineNumber,
          declaration.columnNumber
        )
      case None =>
        createAssignmentCallAst(
          exportCallAst,
          Ast(identifierNode(declaration, name)),
          s"${codeOf(exportCallAst.nodes.head)} = $name",
          declaration.lineNumber,
          declaration.columnNumber
        )
    }

  }

  private def extractDeclarationsFromExportDecl(declaration: BabelNodeInfo, key: String): Option[(Ast, Seq[String])] =
    safeObj(declaration.json, key)
      .map { d =>
        val nodeInfo    = createBabelNodeInfo(d)
        val ast         = astForNodeWithFunctionReferenceAndCall(d)
        val defaultName = codeForNodes(ast.nodes.toSeq)
        val codes       = codeForBabelNodeInfo(nodeInfo)
        val names       = if (codes.isEmpty) defaultName.toSeq else codes
        (ast, names)
      }

  private def extractExportFromNameFromExportDecl(declaration: BabelNodeInfo): String =
    safeObj(declaration.json, "source")
      .map(d => s"_${stripQuotes(code(d))}")
      .getOrElse(ExportKeyword)

  private def cleanImportName(name: String): String = if (name.contains("/")) {
    val stripped = name.stripSuffix("/")
    stripped.substring(stripped.lastIndexOf("/") + 1)
  } else name

  private def createAstForFrom(fromName: String, declaration: BabelNodeInfo): Ast = {
    if (fromName == ExportKeyword) {
      Ast()
    } else {
      val strippedCode = cleanImportName(fromName).stripPrefix("_")
      val id           = identifierNode(declaration, s"_$strippedCode")
      val nLocalNode   = localNode(declaration, id.code, id.code, Defines.Any).order(0)
      scope.addVariable(id.code, nLocalNode, BlockScope)
      scope.addVariableReference(id.code, id)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val sourceCallArgNode = literalNode(declaration, s"\"${fromName.stripPrefix("_")}\"", None)
      val sourceCall =
        callNode(
          declaration,
          s"$RequireKeyword(${sourceCallArgNode.code})",
          RequireKeyword,
          DispatchTypes.STATIC_DISPATCH
        )
      val sourceAst =
        callAst(sourceCall, List(Ast(sourceCallArgNode)))
      val assignmentCallAst = createAssignmentCallAst(
        Ast(id),
        sourceAst,
        s"var ${codeOf(id)} = ${codeOf(sourceAst.nodes.head)}",
        declaration.lineNumber,
        declaration.columnNumber
      )
      assignmentCallAst
    }
  }

  protected def astsForDecorators(elem: BabelNodeInfo): Seq[Ast] = {
    if (hasKey(elem.json, "decorators") && !elem.json("decorators").isNull) {
      elem.json("decorators").arr.toList.map(d => astForDecorator(createBabelNodeInfo(d)))
    } else Seq.empty
  }

  private def namesForDecoratorExpression(code: String): (String, String) = {
    val dotLastIndex = code.lastIndexOf(".")
    if (dotLastIndex != -1) {
      (code.substring(dotLastIndex + 1), code)
    } else {
      (code, code)
    }
  }

  private def astForDecorator(decorator: BabelNodeInfo): Ast = {
    val exprNode = createBabelNodeInfo(decorator.json("expression"))
    exprNode.node match {
      case Identifier | MemberExpression =>
        val (name, fullName) = namesForDecoratorExpression(code(exprNode.json))
        annotationAst(annotationNode(decorator, decorator.code, name, fullName), List.empty)
      case CallExpression =>
        val (name, fullName) = namesForDecoratorExpression(code(exprNode.json("callee")))
        val node             = annotationNode(decorator, decorator.code, name, fullName)
        val assignmentAsts = exprNode.json("arguments").arr.toList.map { arg =>
          createBabelNodeInfo(arg).node match {
            case AssignmentExpression =>
              annotationAssignmentAst(code(arg("left")), code(arg), astForNodeWithFunctionReference(arg("right")))
            case _ =>
              annotationAssignmentAst("value", code(arg), astForNodeWithFunctionReference(arg))
          }
        }
        annotationAst(node, assignmentAsts)
      case _ => Ast()
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
          (None, Option(exported))
        } else {
          val exported = createBabelNodeInfo(spec("exported"))
          val local = if (hasKey(spec, "local")) {
            createBabelNodeInfo(spec("local"))
          } else {
            exported
          }
          (Option(local), Option(exported))
        }
      }

    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val fromAst         = createAstForFrom(exportName, declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration, "declaration")
    val declAsts = declAstAndNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        if (exportName != ExportKeyword)
          diffGraph.addNode(newDependencyNode(name, exportName.stripPrefix("_"), RequireKeyword))
        val exportCallAst = createExportCallAst(name, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration, None)
      }
    }

    val specifierAsts = specifiers.map {
      case (Some(name), Some(alias)) =>
        val strippedCode  = cleanImportName(exportName).stripPrefix("_")
        val exportCallAst = createExportCallAst(alias.code, ExportKeyword, declaration)
        if (exportName != ExportKeyword) {
          diffGraph.addNode(newDependencyNode(alias.code, exportName.stripPrefix("_"), RequireKeyword))
          createExportAssignmentCallAst(name.code, exportCallAst, declaration, Option(s"_$strippedCode"))
        } else {
          createExportAssignmentCallAst(name.code, exportCallAst, declaration, None)
        }
      case (None, Some(alias)) =>
        diffGraph.addNode(newDependencyNode(alias.code, exportName.stripPrefix("_"), RequireKeyword))
        val exportCallAst = createExportCallAst(alias.code, ExportKeyword, declaration)
        createExportAssignmentCallAst(exportName, exportCallAst, declaration, None)
      case _ => Ast()
    }

    val asts = fromAst +: (specifierAsts ++ declAsts)
    setArgumentIndices(asts)
    blockAst(createBlockNode(declaration), asts)
  }

  protected def astForExportAssignment(assignment: BabelNodeInfo): Ast = {
    val expressionAstWithNames = extractDeclarationsFromExportDecl(assignment, "expression")
    val declAsts = expressionAstWithNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(name, ExportKeyword, assignment)
        createExportAssignmentCallAst(name, exportCallAst, assignment, None)
      }
    }

    setArgumentIndices(declAsts)
    blockAst(createBlockNode(assignment), declAsts)
  }

  protected def astForExportDefaultDeclaration(declaration: BabelNodeInfo): Ast = {
    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration, "declaration")
    val declAsts = declAstAndNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(DefaultsKey, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration, None)
      }
    }
    setArgumentIndices(declAsts)
    blockAst(createBlockNode(declaration), declAsts)
  }

  protected def astForExportAllDeclaration(declaration: BabelNodeInfo): Ast = {
    val exportName = extractExportFromNameFromExportDecl(declaration)
    val depGroupId = stripQuotes(code(declaration.json("source")))
    val name       = cleanImportName(depGroupId)
    if (exportName != ExportKeyword) {
      diffGraph.addNode(newDependencyNode(name, depGroupId, RequireKeyword))
    }

    val fromCallAst       = createAstForFrom(exportName, declaration)
    val exportCallAst     = createExportCallAst(name, ExportKeyword, declaration)
    val assignmentCallAst = createExportAssignmentCallAst(s"_$name", exportCallAst, declaration, None)

    val childrenAsts = List(fromCallAst, assignmentCallAst)
    setArgumentIndices(childrenAsts)
    blockAst(createBlockNode(declaration), childrenAsts)
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
      case Nil         => Ast()
      case head :: Nil => head
      case _           => blockAst(createBlockNode(declaration), declAsts)
    }
  }

  private def handleRequireCallForDependencies(
    declarator: BabelNodeInfo,
    lhs: Value,
    rhs: Value,
    call: Option[NewCall]
  ): Unit = {
    val rhsCode  = code(rhs)
    val groupId  = rhsCode.substring(rhsCode.indexOf(s"$RequireKeyword(") + 9, rhsCode.indexOf(")") - 1)
    val nodeInfo = createBabelNodeInfo(lhs)
    val names = nodeInfo.node match {
      case ArrayPattern  => nodeInfo.json("elements").arr.toList.map(code)
      case ObjectPattern => nodeInfo.json("properties").arr.toList.map(code)
      case _             => List(code(lhs))
    }
    names.foreach { name =>
      val _dependencyNode = newDependencyNode(name, groupId, RequireKeyword)
      diffGraph.addNode(_dependencyNode)
      val importNode = createImportNodeAndAttachToCall(declarator, groupId, name, call)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
    }
  }

  private def astForVariableDeclarator(declarator: Value, scopeType: ScopeType, kind: String): Ast = {
    val idNodeInfo     = createBabelNodeInfo(declarator("id"))
    val declNodeInfo   = createBabelNodeInfo(declarator)
    val initNodeInfo   = Try(createBabelNodeInfo(declarator("init"))).toOption
    val declaratorCode = s"$kind ${code(declarator)}"
    val tpe            = typeFor(declNodeInfo)
    val typeFullName   = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any

    val idName = idNodeInfo.node match {
      case Identifier => idNodeInfo.json("name").str
      case _          => idNodeInfo.code
    }
    val nLocalNode = localNode(declNodeInfo, idName, idName, typeFullName).order(0).possibleTypes(Seq(tpe))
    scope.addVariable(idName, nLocalNode, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

    if (initNodeInfo.isEmpty) {
      Ast()
    } else {
      val sourceAst = initNodeInfo.get match {
        case requireCall if requireCall.code.startsWith(s"$RequireKeyword(") =>
          val call = astForNodeWithFunctionReference(requireCall.json)
          handleRequireCallForDependencies(
            createBabelNodeInfo(declarator),
            idNodeInfo.json,
            initNodeInfo.get.json,
            call.root.map(_.asInstanceOf[NewCall])
          )
          call
        case initExpr =>
          astForNodeWithFunctionReference(initExpr.json)
      }
      val nodeInfo = createBabelNodeInfo(idNodeInfo.json)
      nodeInfo.node match {
        case ObjectPattern | ArrayPattern =>
          astForDeconstruction(nodeInfo, sourceAst, declaratorCode)
        case _ =>
          val destAst = idNodeInfo.node match {
            case Identifier => astForIdentifier(idNodeInfo, Option(typeFullName))
            case _          => astForNode(idNodeInfo.json)
          }

          val assignmentCallAst =
            createAssignmentCallAst(
              destAst,
              sourceAst,
              declaratorCode,
              line = line(declarator),
              column = column(declarator)
            )
          assignmentCallAst
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
    val _dependencyNode = newDependencyNode(name, referenceName, ImportKeyword)
    diffGraph.addNode(_dependencyNode)
    val assignment = astForRequireCallFromImport(name, None, referenceName, isImportN = false, impDecl)
    val call       = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
    val importNode =
      createImportNodeAndAttachToCall(impDecl, referenceName, name, call)
    diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
    assignment
  }

  private def astForRequireCallFromImport(
    name: String,
    alias: Option[String],
    from: String,
    isImportN: Boolean,
    nodeInfo: BabelNodeInfo
  ): Ast = {
    val destName   = alias.getOrElse(name)
    val destNode   = identifierNode(nodeInfo, destName)
    val nLocalNode = localNode(nodeInfo, destName, destName, Defines.Any).order(0)
    scope.addVariable(destName, nLocalNode, BlockScope)
    scope.addVariableReference(destName, destNode)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

    val destAst           = Ast(destNode)
    val sourceCallArgNode = literalNode(nodeInfo, s"\"$from\"", None)
    val sourceCall =
      callNode(nodeInfo, s"$RequireKeyword(${sourceCallArgNode.code})", RequireKeyword, DispatchTypes.DYNAMIC_DISPATCH)

    val receiverNode = identifierNode(nodeInfo, RequireKeyword)
    val thisNode     = identifierNode(nodeInfo, "this").dynamicTypeHintFullName(typeHintForThisExpression())
    scope.addVariableReference(thisNode.name, thisNode)
    val cAst = callAst(
      sourceCall,
      List(Ast(sourceCallArgNode)),
      receiver = Option(Ast(receiverNode)),
      base = Option(Ast(thisNode))
    )
    val sourceAst = if (isImportN) {
      val fieldAccessCall = createFieldAccessCallAst(
        cAst,
        createFieldIdentifierNode(name, nodeInfo.lineNumber, nodeInfo.columnNumber),
        nodeInfo.lineNumber,
        nodeInfo.columnNumber
      )
      fieldAccessCall
    } else {
      cAst
    }
    val assigmentCallAst =
      createAssignmentCallAst(
        destAst,
        sourceAst,
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
      val _dependencyNode = newDependencyNode(source, source, ImportKeyword)
      diffGraph.addNode(_dependencyNode)
      val assignment = astForRequireCallFromImport(source, None, source, isImportN = false, impDecl)
      val call       = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
      val importNode = createImportNodeAndAttachToCall(impDecl, source, source, call)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
      assignment
    } else {
      val specs = impDecl.json("specifiers").arr.toList
      val requireCalls = specs.map { importSpecifier =>
        val isImportN = createBabelNodeInfo(importSpecifier).node match {
          case ImportSpecifier => true
          case _               => false
        }
        val name             = importSpecifier("local")("name").str
        val (alias, reqName) = reqNameFromImportSpecifier(importSpecifier, name)
        val assignment       = astForRequireCallFromImport(reqName, alias, source, isImportN = isImportN, impDecl)
        val importedName     = importSpecifier("local")("name").str
        val call             = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
        val importNode       = createImportNodeAndAttachToCall(impDecl, s"$source:$reqName", importedName, call)
        val _dependencyNode  = newDependencyNode(importedName, source, ImportKeyword)
        diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
        diffGraph.addNode(_dependencyNode)
        assignment
      }
      if (requireCalls.isEmpty) {
        Ast()
      } else if (requireCalls.sizeIs == 1) {
        requireCalls.head
      } else {
        blockAst(createBlockNode(impDecl), requireCalls)
      }
    }
  }

  private def reqNameFromImportSpecifier(importSpecifier: Value, name: String) = {
    if (hasKey(importSpecifier, "imported")) {
      (Option(name), importSpecifier("imported")("name").str)
    } else {
      (None, name)
    }
  }

  private def createImportNodeAndAttachToCall(
    impDecl: BabelNodeInfo,
    importedEntity: String,
    importedAs: String,
    call: Option[NewCall]
  ): NewImport = {
    createImportNodeAndAttachToCall(impDecl.code.stripSuffix(";"), importedEntity, importedAs, call)
  }

  private def createImportNodeAndAttachToCall(
    code: String,
    importedEntity: String,
    importedAs: String,
    call: Option[NewCall]
  ): NewImport = {
    val impNode = NewImport()
      .code(code)
      .importedEntity(importedEntity)
      .importedAs(importedAs)
      .lineNumber(call.flatMap(_.lineNumber))
      .columnNumber(call.flatMap(_.lineNumber))
    call.foreach { c => diffGraph.addEdge(c, impNode, EdgeTypes.IS_CALL_FOR_IMPORT) }
    impNode
  }

  private def convertDestructingObjectElement(element: BabelNodeInfo, key: BabelNodeInfo, localTmpName: String): Ast = {
    val valueAst = astForNode(element.json)

    val nLocalNode = localNode(element, element.code, element.code, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(element.code, nLocalNode, MethodScope)

    val fieldAccessTmpNode = identifierNode(element, localTmpName)
    val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
    val accessAst = createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      valueAst,
      accessAst,
      s"${codeOf(valueAst.nodes.head)} = ${codeOf(accessAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingArrayElement(element: BabelNodeInfo, index: Int, localTmpName: String): Ast = {
    val valueAst = astForNode(element.json)

    val nLocalNode = localNode(element, element.code, element.code, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(element.code, nLocalNode, MethodScope)

    val fieldAccessTmpNode = identifierNode(element, localTmpName)
    val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
    val accessAst = createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      valueAst,
      accessAst,
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

    val lhsElement = element.json("left")
    val nodeInfo   = createBabelNodeInfo(lhsElement)
    val lhsAst = nodeInfo.node match {
      case ObjectPattern | ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        astForDeconstruction(nodeInfo, sourceAst, element.code)
      case _ => astForNodeWithFunctionReference(lhsElement)
    }

    val testAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
      val accessAst =
        createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      val voidCallNode = createVoidCallNode(element.lineNumber, element.columnNumber)
      createEqualsCallAst(accessAst, Ast(voidCallNode), element.lineNumber, element.columnNumber)
    }
    val falseAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
      createIndexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    }
    val ternaryNodeAst =
      createTernaryCallAst(testAst, rhsAst, falseAst, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      lhsAst,
      ternaryNodeAst,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingObjectElementWithDefault(
    element: BabelNodeInfo,
    key: BabelNodeInfo,
    localTmpName: String
  ): Ast = {
    val rhsElement = element.json("right")
    val rhsAst     = astForNodeWithFunctionReference(rhsElement)

    val lhsElement = element.json("left")
    val nodeInfo   = createBabelNodeInfo(lhsElement)
    val lhsAst = nodeInfo.node match {
      case ObjectPattern | ArrayPattern =>
        val sourceAst = astForNodeWithFunctionReference(createBabelNodeInfo(rhsElement).json)
        astForDeconstruction(nodeInfo, sourceAst, element.code)
      case _ => astForNodeWithFunctionReference(lhsElement)
    }

    val testAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
      val accessAst =
        createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      val voidCallNode = createVoidCallNode(element.lineNumber, element.columnNumber)
      createEqualsCallAst(accessAst, Ast(voidCallNode), element.lineNumber, element.columnNumber)
    }
    val falseAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = createFieldIdentifierNode(key.code, key.lineNumber, key.columnNumber)
      createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    }
    val ternaryNodeAst =
      createTernaryCallAst(testAst, rhsAst, falseAst, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      lhsAst,
      ternaryNodeAst,
      s"${codeOf(lhsAst.nodes.head)} = ${codeOf(ternaryNodeAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def createParamAst(pattern: BabelNodeInfo, keyName: String, sourceAst: Ast): Ast = {
    val testAst = {
      val lhsNode = identifierNode(pattern, keyName)
      scope.addVariableReference(keyName, lhsNode)
      val rhsNode =
        callNode(pattern, "void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH)
      createEqualsCallAst(Ast(lhsNode), Ast(rhsNode), pattern.lineNumber, pattern.columnNumber)
    }

    val falseNode = {
      val initNode = identifierNode(pattern, keyName)
      scope.addVariableReference(keyName, initNode)
      initNode
    }
    createTernaryCallAst(testAst, sourceAst, Ast(falseNode), pattern.lineNumber, pattern.columnNumber)
  }

  protected def astForDeconstruction(
    pattern: BabelNodeInfo,
    sourceAst: Ast,
    code: String,
    paramName: Option[String] = None
  ): Ast = {
    val localTmpName = generateUnusedVariableName(usedVariableNames, "_tmp")

    val blockNode = createBlockNode(pattern, Option(code))
    scope.pushNewBlockScope(blockNode)
    localAstParentStack.push(blockNode)

    val nLocalNode = localNode(pattern, localTmpName, localTmpName, Defines.Any).order(0)
    val tmpNode    = identifierNode(pattern, localTmpName)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(localTmpName, nLocalNode, BlockScope)
    scope.addVariableReference(localTmpName, tmpNode)

    val rhsAssignmentAst = paramName.map(createParamAst(pattern, _, sourceAst)).getOrElse(sourceAst)
    val assignmentTmpCallAst =
      createAssignmentCallAst(
        Ast(tmpNode),
        rhsAssignmentAst,
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
              val arg1Ast = Ast(identifierNode(nodeInfo, localTmpName))
              astForSpreadOrRestElement(nodeInfo, Option(arg1Ast))
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
                case _ => astForNodeWithFunctionReference(nodeInfo.json)
              }
          }
        }
      case ArrayPattern =>
        pattern.json("elements").arr.toList.zipWithIndex.map {
          case (element, index) if !element.isNull =>
            val nodeInfo = createBabelNodeInfo(element)
            nodeInfo.node match {
              case RestElement =>
                val fieldAccessTmpNode = identifierNode(nodeInfo, localTmpName)
                val keyNode            = literalNode(nodeInfo, index.toString, Option(Defines.Number))
                val accessAst =
                  createIndexAccessCallAst(fieldAccessTmpNode, keyNode, nodeInfo.lineNumber, nodeInfo.columnNumber)
                astForSpreadOrRestElement(nodeInfo, Option(accessAst))
              case Identifier =>
                convertDestructingArrayElement(nodeInfo, index, localTmpName)
              case AssignmentPattern =>
                convertDestructingArrayElementWithDefault(nodeInfo, index, localTmpName)
              case _ => astForNodeWithFunctionReference(nodeInfo.json)
            }
          case _ => Ast()
        }
      case _ =>
        List(convertDestructingObjectElement(pattern, pattern, localTmpName))
    }

    val returnTmpNode = identifierNode(pattern, localTmpName)
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = assignmentTmpCallAst +: subTreeAsts :+ Ast(returnTmpNode)
    setArgumentIndices(blockChildren)
    blockAst(blockNode, blockChildren)
  }

}
