package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.AstNodeBuilder.dependencyNode
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewImport, NewMethod, NewNode}
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, PropertyDefaults}
import io.shiftleft.codepropertygraph.generated.EvaluationStrategies
import io.shiftleft.codepropertygraph.generated.Operators

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val DefaultsKey    = "default"
  private val ExportKeyword  = "exports"
  private val RequireKeyword = "require"
  private val ImportKeyword  = "import"

  /** The names a node introduces, e.g. the declared variables or the declared function / class. */
  protected def codesForNode(node: BabelNode): Seq[String] = {
    val codes = node match {
      case _: (Identifier | NumericLiteral | StringLiteral) => Seq(node.code)
      case assignment: AssignmentExpression                 => Seq(assignment.left.code)
      case clazz: ClassDeclaration           => Seq(clazz.id.map(_.code).getOrElse(PropertyDefaults.Code))
      case alias: TSTypeAliasDeclaration     => Seq(alias.id.code)
      case interface: TSInterfaceDeclaration => Seq(interface.id.code)
      case enumeration: TSEnumDeclaration    => Seq(enumeration.id.code)
      case module: TSModuleDeclaration       => Seq(module.id.code)
      case TSDeclareFunction(_) | FunctionDeclaration(_) | FunctionExpression(_) | ClassExpression(_)
          if namedId(node).isDefined =>
        namedId(node).map(_.name).toSeq
      case declarator: VariableDeclarator =>
        declarator.id match {
          case array: ArrayPattern    => array.elements.flatten.map(_.code)
          case obj: ObjectPattern     => obj.properties.flatMap(codesForNode)
          case identifier: Identifier => Seq(identifier.name)
          case other                  => Seq(other.code)
        }
      case member: MemberExpression         => Seq(member.property.code)
      case property: ObjectProperty         => Seq(property.key.code)
      case obj: ObjectExpression            => obj.properties.flatMap(codesForNode)
      case declaration: VariableDeclaration => declaration.declarations.flatMap(codesForNode)
      case _                                => Seq.empty
    }
    codes.map(_.replace("...", ""))
  }

  private def namedId(node: BabelNode): Option[Identifier] = node match {
    case n: TSDeclareFunction   => n.id
    case n: FunctionDeclaration => n.id
    case n: FunctionExpression  => n.id
    case n: ClassExpression     => n.id
    case _                      => None
  }

  protected def decoratorsOf(node: BabelNode): Seq[Decorator] = node match {
    case n: ArrayPattern          => n.decorators
    case n: AssignmentPattern     => n.decorators
    case n: ClassAccessorProperty => n.decorators
    case n: ClassDeclaration      => n.decorators
    case n: ClassExpression       => n.decorators
    case n: ClassMethod           => n.decorators
    case n: ClassPrivateMethod    => n.decorators
    case n: ClassPrivateProperty  => n.decorators
    case n: ClassProperty         => n.decorators
    case n: Identifier            => n.decorators
    case n: ObjectMethod          => n.decorators
    case n: ObjectPattern         => n.decorators
    case n: ObjectProperty        => n.decorators
    case n: Placeholder           => n.decorators
    case n: RestElement           => n.decorators
    case n: TSDeclareMethod       => n.decorators
    case n: TSParameterProperty   => n.decorators
    case _                        => Seq.empty
  }

  /** The expressions of all decorators of `node` that are more than a plain reference, e.g. `@Foo()` but not `@Foo`. */
  protected def decoratorExpressionElements(node: BabelNode): List[BabelNode] =
    decoratorsOf(node)
      .map(_.expression)
      .collect {
        case expression if !expression.isInstanceOf[Identifier] && !expression.isInstanceOf[MemberExpression] =>
          expression
      }
      .toList

  protected def astsForDecorators(node: BabelNode): Seq[Ast] = {
    withIndex(decoratorsOf(node)) { case (decorator, idx) =>
      val decoratorAst = astForDecorator(decorator)
      setOrderExplicitly(decoratorAst, idx)
      decoratorAst
    }
  }

  protected def astForDecorator(decorator: Decorator): Ast = {
    val expression = decorator.expression
    expression match {
      case _: (Identifier | MemberExpression) =>
        val (name, fullName) = namesForDecoratorExpression(expression.code)
        annotationAst(annotationNode(decorator, decorator.code, name, fullName), List.empty)
      case _ =>
        val (name, fullName) = namesForDecoratorExpression(expression.code.takeWhile(_ != '('))
        annotationAst(annotationNode(decorator, decorator.code, name, fullName), List.empty)
    }
  }

  private def namesForDecoratorExpression(code: String): (String, String) = {
    val dotLastIndex = code.lastIndexOf(".")
    if (dotLastIndex != -1) {
      (code.substring(dotLastIndex + 1), code)
    } else {
      (code, code)
    }
  }

  protected def astForExportNamedDeclaration(declaration: ExportNamedDeclaration): Ast = declaration.declaration match {
    case Some(importEquals: TSImportEqualsDeclaration) =>
      astForExportedImportEquals(declaration, importEquals)
    case _ => astForExportNamedDeclarationWithSpecifiers(declaration)
  }

  /** `export import a = b.c` is lowered like any other exported declaration: the import of `a`, followed by the export
    * of the binding, `exports.a = a`.
    */
  private def astForExportedImportEquals(
    declaration: ExportNamedDeclaration,
    importEquals: TSImportEqualsDeclaration
  ): Ast = {
    val name          = importEquals.id.name
    val importAst     = astForTSImportEqualsDeclaration(importEquals, Some(declaration))
    val exportCallAst = createExportCallAst(name, ExportKeyword, declaration)
    val exportAst     = createExportAssignmentCallAst(name, exportCallAst, declaration, None)
    blockAst(blockNode(declaration, declaration.code, Defines.Any), List(importAst, exportAst))
  }

  private def astForExportNamedDeclarationWithSpecifiers(declaration: ExportNamedDeclaration): Ast = {
    val specifiers: Seq[(Option[BabelNode], Option[BabelNode])] = declaration.specifiers.map {
      case spec: ExportNamespaceSpecifier => (None, Option(spec.exported))
      case spec: ExportSpecifier          => (Option(spec.local), Option(spec.exported))
      case spec: ExportDefaultSpecifier   => (Option(spec.exported), Option(spec.exported))
    }

    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val fromAst         = createAstForFrom(exportName, declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(declaration.declaration)
    val declAsts        = declAstAndNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        if (exportName != ExportKeyword)
          diffGraph.addNode(dependencyNode(name, exportName.stripPrefix("_"), RequireKeyword))
        val exportCallAst = createExportCallAst(name, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration, None)
      }
    }

    val specifierAsts = specifiers.map {
      case (Some(name), Some(alias)) =>
        val strippedCode  = cleanImportName(exportName).stripPrefix("_")
        val exportCallAst = createExportCallAst(alias.code, ExportKeyword, declaration)
        if (exportName != ExportKeyword) {
          diffGraph.addNode(dependencyNode(alias.code, exportName.stripPrefix("_"), RequireKeyword))
          createExportAssignmentCallAst(name.code, exportCallAst, declaration, Option(s"_$strippedCode"))
        } else {
          createExportAssignmentCallAst(name.code, exportCallAst, declaration, None)
        }
      case (None, Some(alias)) =>
        diffGraph.addNode(dependencyNode(alias.code, exportName.stripPrefix("_"), RequireKeyword))
        val exportCallAst = createExportCallAst(alias.code, ExportKeyword, declaration)
        createExportAssignmentCallAst(exportName, exportCallAst, declaration, None)
      case _ => Ast()
    }

    val asts = (fromAst +: (specifierAsts ++ declAsts)).toList
    blockAst(blockNode(declaration, declaration.code, Defines.Any), asts)
  }

  protected def astForExportAssignment(assignment: TSExportAssignment): Ast = {
    val expressionAstWithNames = extractDeclarationsFromExportDecl(Option(assignment.expression))
    val declAsts               = expressionAstWithNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(name, ExportKeyword, assignment)
        createExportAssignmentCallAst(name, exportCallAst, assignment, None)
      }
    }
    blockAst(blockNode(assignment, assignment.code, Defines.Any), declAsts)
  }

  protected def astForExportDefaultDeclaration(declaration: ExportDefaultDeclaration): Ast = {
    val exportName      = extractExportFromNameFromExportDecl(declaration)
    val declAstAndNames = extractDeclarationsFromExportDecl(Option(declaration.declaration))
    val declAsts        = declAstAndNames.toList.flatMap { case (ast, names) =>
      ast +: names.map { name =>
        val exportCallAst = createExportCallAst(DefaultsKey, exportName, declaration)
        createExportAssignmentCallAst(name, exportCallAst, declaration, None)
      }
    }
    blockAst(blockNode(declaration, declaration.code, Defines.Any), declAsts)
  }

  protected def astForExportAllDeclaration(declaration: ExportAllDeclaration): Ast = {
    val exportName = extractExportFromNameFromExportDecl(declaration)
    val depGroupId = stripQuotes(declaration.source.code)
    val name       = cleanImportName(depGroupId)
    if (exportName != ExportKeyword) {
      diffGraph.addNode(dependencyNode(name, depGroupId, RequireKeyword))
    }

    val fromCallAst       = createAstForFrom(exportName, declaration)
    val exportCallAst     = createExportCallAst(name, ExportKeyword, declaration)
    val assignmentCallAst = createExportAssignmentCallAst(s"_$name", exportCallAst, declaration, None)

    val childrenAsts = List(fromCallAst, assignmentCallAst)
    blockAst(blockNode(declaration, declaration.code, Defines.Any), childrenAsts)
  }

  private def createExportCallAst(name: String, exportName: String, declaration: BabelNode): Ast = {
    val exportCallAst = if (name == DefaultsKey) {
      indexAccessCallAst(
        identifierNode(declaration, exportName),
        literalNode(declaration, s"\"$DefaultsKey\"", Option(Defines.String)),
        declaration.lineNumber,
        declaration.columnNumber
      )
    } else {
      val fieldName = stripQuotes(name)
      createFieldAccessCallAst(
        identifierNode(declaration, exportName),
        fieldIdentifierNode(declaration, fieldName, fieldName),
        declaration.lineNumber,
        declaration.columnNumber
      )
    }
    exportCallAst
  }

  private def indexAccessCallAst(baseNode: NewNode, partNode: NewNode, line: Option[Int], column: Option[Int]): Ast = {
    val callNode_ = callNode(
      s"${codeOf(baseNode)}[${codeOf(partNode)}]",
      Operators.indexAccess,
      DispatchTypes.STATIC_DISPATCH,
      line,
      column
    )
    val arguments = List(Ast(baseNode), Ast(partNode))
    callAst(callNode_, arguments)
  }

  private def createExportAssignmentCallAst(
    name: String,
    exportCallAst: Ast,
    declaration: BabelNode,
    from: Option[String]
  ): Ast = {
    from match {
      case Some(value) =>
        val identNode = identifierNode(declaration, value)
        scope.addVariableReference(name, identNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        val fieldName = stripQuotes(name)
        val call      = createFieldAccessCallAst(
          identNode,
          fieldIdentifierNode(declaration, fieldName, fieldName),
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
        val identNode = identifierNode(declaration, name)
        scope.addVariableReference(name, identNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        createAssignmentCallAst(
          exportCallAst,
          Ast(identNode),
          s"${codeOf(exportCallAst.nodes.head)} = $name",
          declaration.lineNumber,
          declaration.columnNumber
        )
    }

  }

  private def extractExportFromNameFromExportDecl(
    declaration: ExportNamedDeclaration | ExportDefaultDeclaration | ExportAllDeclaration
  ): String = {
    val source = declaration match {
      case named: ExportNamedDeclaration => named.source
      case all: ExportAllDeclaration     => Option(all.source)
      case _: ExportDefaultDeclaration   => None
    }
    source.map(sourceNode => s"_${stripQuotes(sourceNode.code)}").getOrElse(ExportKeyword)
  }

  private def cleanImportName(name: String): String = if (name.contains("/")) {
    val stripped = name.stripSuffix("/")
    stripped.substring(stripped.lastIndexOf("/") + 1)
  } else name

  /** `require("from")` as generated for imports and re-exports: a statically dispatched call without receiver. */
  private def staticRequireCallAst(node: BabelNode, from: String): Ast = {
    val argNode = literalNode(node, s"\"$from\"", None)
    val call    = callNode(node, s"$RequireKeyword(${argNode.code})", RequireKeyword, DispatchTypes.STATIC_DISPATCH)
    callAst(call, List(Ast(argNode)))
  }

  private def createAstForFrom(fromName: String, declaration: BabelNode): Ast = {
    if (fromName == ExportKeyword) {
      Ast()
    } else {
      val strippedCode = cleanImportName(fromName).stripPrefix("_")
      val id           = identifierNode(declaration, s"_$strippedCode")
      val nLocalNode   = localNode(declaration, id.code, id.code, Defines.Any).order(0)
      scope.addVariable(id.code, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
      scope.addVariableReference(id.code, id, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

      val sourceAst         = staticRequireCallAst(declaration, fromName.stripPrefix("_"))
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

  protected def astForVariableDeclaration(declaration: VariableDeclaration): Ast = {
    val kind      = declaration.kind
    val scopeType = if (kind == "let") { VariableScopeManager.ScopeType.BlockScope }
    else { VariableScopeManager.ScopeType.MethodScope }
    val declAsts = declaration.declarations.map(astForVariableDeclarator(_, scopeType, kind)).toList
    declAsts match {
      case Nil         => Ast()
      case head :: Nil => head
      case _           => blockAst(blockNode(declaration, declaration.code, Defines.Any), declAsts)
    }
  }

  /** For `export import a = b.c`, the export declaration wraps the import-equals declaration; the location is the one
    * of the whole statement, `positionNode`.
    */
  protected def astForTSImportEqualsDeclaration(
    impDecl: TSImportEqualsDeclaration,
    positionNode: Option[BabelNode] = None
  ): Ast = {
    val node          = positionNode.getOrElse(impDecl)
    val name          = impDecl.id.name
    val referenceName = impDecl.moduleReference match {
      case externalModule: TSExternalModuleReference => externalModule.expression.value
      case other                                     => other.code
    }
    val _dependencyNode = dependencyNode(name, referenceName, ImportKeyword)
    diffGraph.addNode(_dependencyNode)
    val assignment = astForRequireCallFromImport(name, None, referenceName, isImportN = false, node)
    val call       = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
    val importNode =
      createImportNodeAndAttachToCall(node, referenceName, name, call)
    diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
    assignment
  }

  protected def astForImportDeclaration(impDecl: ImportDeclaration): Ast = {
    val source     = impDecl.source.value
    val specifiers = impDecl.specifiers

    if (specifiers.isEmpty) {
      val _dependencyNode = dependencyNode(source, source, ImportKeyword)
      diffGraph.addNode(_dependencyNode)
      val assignment = astForRequireCallFromImport(source, None, source, isImportN = false, impDecl)
      val call       = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
      val importNode = createImportNodeAndAttachToCall(impDecl, source, source, call)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
      assignment
    } else {
      val requireCalls = specifiers.map { importSpecifier =>
        val isImportN = importSpecifier.isInstanceOf[ImportSpecifier]
        val name      = importSpecifier match {
          case s: ImportDefaultSpecifier   => s.local.name
          case s: ImportNamespaceSpecifier => s.local.name
          case s: ImportSpecifier          => s.local.name
        }
        val (alias, reqName) = reqNameFromImportSpecifier(importSpecifier, name)
        val assignment       = astForRequireCallFromImport(reqName, alias, source, isImportN = isImportN, impDecl)
        val importedName     = name
        val call             = assignment.nodes.collectFirst { case x: NewCall if x.name == "require" => x }
        val importNode       = createImportNodeAndAttachToCall(impDecl, s"$source:$reqName", importedName, call)
        val _dependencyNode  = dependencyNode(importedName, source, ImportKeyword)
        diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
        diffGraph.addNode(_dependencyNode)
        assignment
      }
      if (requireCalls.isEmpty) {
        Ast()
      } else if (requireCalls.sizeIs == 1) {
        requireCalls.head
      } else {
        blockAst(blockNode(impDecl, impDecl.code, Defines.Any), requireCalls.toList)
      }
    }
  }

  private def astForRequireCallFromImport(
    name: String,
    alias: Option[String],
    from: String,
    isImportN: Boolean,
    node: BabelNode
  ): Ast = {
    val destName   = alias.getOrElse(name)
    val destNode   = identifierNode(node, destName)
    val nLocalNode = localNode(node, destName, destName, Defines.Any).order(0)
    scope.addVariable(destName, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
    scope.addVariableReference(destName, destNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

    val destAst   = Ast(destNode)
    val cAst      = staticRequireCallAst(node, from)
    val sourceAst = if (isImportN) {
      val fieldName       = stripQuotes(name)
      val fieldAccessCall = createFieldAccessCallAst(
        cAst,
        fieldIdentifierNode(node, fieldName, fieldName),
        node.lineNumber,
        node.columnNumber
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
        node.lineNumber,
        node.columnNumber
      )
    assigmentCallAst
  }

  private def reqNameFromImportSpecifier(
    importSpecifier: ImportDefaultSpecifier | ImportNamespaceSpecifier | ImportSpecifier,
    name: String
  ): (Option[String], String) = importSpecifier match {
    case specifier: ImportSpecifier =>
      val imported = specifier.imported match {
        case identifier: Identifier => identifier.name
        case literal: StringLiteral => literal.value
      }
      (Option(name), imported)
    case _ => (None, name)
  }

  private def createImportNodeAndAttachToCall(
    impDecl: BabelNode,
    importedEntity: String,
    importedAs: String,
    call: Option[NewCall]
  ): NewImport = {
    val impNode = newImportNode(
      code = impDecl.code.stripSuffix(";"),
      importedEntity = importedEntity,
      importedAs = importedAs,
      include = impDecl
    )
    call.foreach { _call => diffGraph.addEdge(_call, impNode, EdgeTypes.IS_CALL_FOR_IMPORT) }
    methodAstParentStack
      .collectFirst { case m: NewMethod if m.name == Defines.Program => m }
      .foreach(diffGraph.addEdge(_, impNode, EdgeTypes.AST))
    impNode
  }

  protected def astForDeconstruction(
    pattern: ObjectPattern | ArrayPattern,
    sourceAst: Ast,
    code: String,
    paramName: Option[String] = None
  ): Ast = {
    val localTmpName = generateUnusedVariableName(usedVariableNames, "_tmp")

    val blockNode_ = blockNode(pattern, code, Defines.Any)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val nLocalNode = localNode(pattern, localTmpName, localTmpName, Defines.Any).order(0)
    val tmpNode    = identifierNode(pattern, localTmpName)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(localTmpName, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
    scope.addVariableReference(localTmpName, tmpNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val rhsAssignmentAst     = paramName.map(createParamAst(pattern, _, sourceAst)).getOrElse(sourceAst)
    val assignmentTmpCallAst =
      createAssignmentCallAst(
        Ast(tmpNode),
        rhsAssignmentAst,
        s"$localTmpName = ${codeOf(rhsAssignmentAst.nodes.head)}",
        pattern.lineNumber,
        pattern.columnNumber
      )

    val subTreeAsts: Seq[Ast] = pattern match {
      case objectPattern: ObjectPattern =>
        objectPattern.properties.map {
          case rest: RestElement =>
            val arg1Ast = Ast(identifierNode(rest, localTmpName))
            astForSpreadOrRestElement(rest, Option(arg1Ast))
          case property: ObjectProperty =>
            property.value match {
              case value: Identifier =>
                convertDestructingObjectElement(value, property.key, localTmpName)
              case value: AssignmentPattern =>
                convertDestructingObjectElementWithDefault(value, property.key, localTmpName)
              case value => astForNodeWithFunctionReference(value)
            }
        }
      case arrayPattern: ArrayPattern =>
        arrayPattern.elements.zipWithIndex.map {
          case (Some(element), index) =>
            element match {
              case rest: RestElement =>
                val fieldAccessTmpNode = identifierNode(rest, localTmpName)
                val keyNode            = literalNode(rest, index.toString, Option(Defines.Number))
                val accessAst = indexAccessCallAst(fieldAccessTmpNode, keyNode, rest.lineNumber, rest.columnNumber)
                astForSpreadOrRestElement(rest, Option(accessAst))
              case identifier: Identifier =>
                convertDestructingArrayElement(identifier, index, localTmpName)
              case assignment: AssignmentPattern =>
                convertDestructingArrayElementWithDefault(assignment, index, localTmpName)
              case other => astForNodeWithFunctionReference(other)
            }
          case (None, _) => Ast()
        }
    }

    val returnTmpNode = identifierNode(pattern, localTmpName)
    scope.popScope()
    localAstParentStack.pop()

    val blockChildren = (assignmentTmpCallAst +: subTreeAsts :+ Ast(returnTmpNode)).toList
    blockAst(blockNode_, blockChildren)
  }

  private def extractDeclarationsFromExportDecl(exported: Option[BabelNode]): Option[(Ast, Seq[String])] =
    exported.map { declaration =>
      val ast         = astForNodeWithFunctionReferenceAndCall(declaration)
      val defaultName = codeForNodes(ast.nodes.toSeq)
      val codes       = codesForNode(declaration)
      val names       = if (codes.isEmpty) defaultName.toSeq else codes
      (ast, names)
    }

  private def handleRequireCallForDependencies(
    declarator: VariableDeclarator,
    lhs: BabelNode,
    rhs: BabelNode,
    call: Option[NewCall]
  ): Unit = {
    val rhsCode = rhs.code
    val groupId = rhsCode.substring(rhsCode.indexOf(s"$RequireKeyword(") + 9, rhsCode.indexOf(")") - 1)
    val names   = lhs match {
      case array: ArrayPattern => array.elements.flatten.map(_.code).toList
      case obj: ObjectPattern  => obj.properties.map(_.code).toList
      case _                   => List(lhs.code)
    }
    names.foreach { name =>
      val _dependencyNode = dependencyNode(name, groupId, RequireKeyword)
      diffGraph.addNode(_dependencyNode)
      val importNode = createImportNodeAndAttachToCall(declarator, groupId, name, call)
      diffGraph.addEdge(importNode, _dependencyNode, EdgeTypes.IMPORTS)
    }
  }

  private def astForVariableDeclarator(
    declarator: VariableDeclarator,
    scopeType: VariableScopeManager.ScopeType,
    kind: String
  ): Ast = {
    val idNode         = declarator.id
    val declaratorCode = s"$kind ${declarator.code}"
    val tpe            = typeFor(declarator)
    val typeFullName   = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any

    val idName = idNode match {
      case identifier: Identifier => identifier.name
      case _                      => idNode.code
    }
    val nLocalNode = localNode(declarator, idName, idName, typeFullName).order(0).possibleTypes(Seq(tpe))
    scope.addVariable(idName, nLocalNode, typeFullName, scopeType)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)

    declarator.init match {
      case None       => Ast()
      case Some(init) =>
        val sourceAst = init match {
          case requireCall if requireCall.code.startsWith(s"$RequireKeyword(") =>
            val call = astForNodeWithFunctionReference(requireCall)
            handleRequireCallForDependencies(declarator, idNode, init, call.root.map(_.asInstanceOf[NewCall]))
            call
          case initExpr =>
            astForNodeWithFunctionReference(initExpr)
        }
        idNode match {
          case pattern: (ObjectPattern | ArrayPattern) =>
            astForDeconstruction(pattern, sourceAst, declaratorCode)
          case _ =>
            val destAst = idNode match {
              case identifier: Identifier => astForIdentifier(identifier, Option(typeFullName))
              case _                      => astForNode(idNode)
            }

            createAssignmentCallAst(
              destAst,
              sourceAst,
              declaratorCode,
              line = line(declarator),
              column = column(declarator)
            )
        }
    }
  }

  private def convertDestructingObjectElement(element: BabelNode, key: BabelNode, localTmpName: String): Ast = {
    val valueAst = astForNode(element)

    val nLocalNode = localNode(element, element.code, element.code, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(element.code, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

    val fieldAccessTmpNode = identifierNode(element, localTmpName)
    val keyName            = stripQuotes(key.code)
    val keyNode            = fieldIdentifierNode(key, keyName, keyName)
    val accessAst = createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      valueAst,
      accessAst,
      s"${codeOf(valueAst.nodes.head)} = ${codeOf(accessAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingArrayElement(element: BabelNode, index: Int, localTmpName: String): Ast = {
    val valueAst = astForNode(element)

    val nLocalNode = localNode(element, element.code, element.code, Defines.Any).order(0)
    diffGraph.addEdge(localAstParentStack.head, nLocalNode, EdgeTypes.AST)
    scope.addVariable(element.code, nLocalNode, Defines.Any, VariableScopeManager.ScopeType.MethodScope)

    val fieldAccessTmpNode = identifierNode(element, localTmpName)
    val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
    val accessAst          = indexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
    createAssignmentCallAst(
      valueAst,
      accessAst,
      s"${codeOf(valueAst.nodes.head)} = ${codeOf(accessAst.nodes.head)}",
      element.lineNumber,
      element.columnNumber
    )
  }

  private def convertDestructingArrayElementWithDefault(
    element: AssignmentPattern,
    index: Int,
    localTmpName: String
  ): Ast = {
    val rhsAst = astForNodeWithFunctionReference(element.right)
    val lhsAst = element.left match {
      case pattern: (ObjectPattern | ArrayPattern) =>
        val sourceAst = astForNodeWithFunctionReference(element.right)
        astForDeconstruction(pattern, sourceAst, element.code)
      case lhs => astForNodeWithFunctionReference(lhs)
    }

    val testAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
      val accessAst          =
        indexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      val voidCallNode_ = voidCallNode(element.lineNumber, element.columnNumber)
      createEqualsCallAst(accessAst, Ast(voidCallNode_), element.lineNumber, element.columnNumber)
    }
    val falseAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = literalNode(element, index.toString, Option(Defines.Number))
      indexAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
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
    element: AssignmentPattern,
    key: BabelNode,
    localTmpName: String
  ): Ast = {
    val rhsAst = astForNodeWithFunctionReference(element.right)
    val lhsAst = element.left match {
      case pattern: (ObjectPattern | ArrayPattern) =>
        val sourceAst = astForNodeWithFunctionReference(element.right)
        astForDeconstruction(pattern, sourceAst, element.code)
      case lhs => astForNodeWithFunctionReference(lhs)
    }

    val keyName = stripQuotes(key.code)
    val testAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = fieldIdentifierNode(key, keyName, keyName)
      val accessAst = createFieldAccessCallAst(fieldAccessTmpNode, keyNode, element.lineNumber, element.columnNumber)
      val voidCallNode_ = voidCallNode(element.lineNumber, element.columnNumber)
      createEqualsCallAst(accessAst, Ast(voidCallNode_), element.lineNumber, element.columnNumber)
    }
    val falseAst = {
      val fieldAccessTmpNode = identifierNode(element, localTmpName)
      val keyNode            = fieldIdentifierNode(key, keyName, keyName)
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

  private def createParamAst(pattern: BabelNode, keyName: String, sourceAst: Ast): Ast = {
    val testAst = {
      val lhsNode = identifierNode(pattern, keyName)
      scope.addVariableReference(keyName, lhsNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val rhsNode =
        callNode(pattern, "void 0", "<operator>.void", DispatchTypes.STATIC_DISPATCH)
      createEqualsCallAst(Ast(lhsNode), Ast(rhsNode), pattern.lineNumber, pattern.columnNumber)
    }

    val falseNode = {
      val initNode = identifierNode(pattern, keyName)
      scope.addVariableReference(keyName, initNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      initNode
    }
    createTernaryCallAst(testAst, sourceAst, Ast(falseNode), pattern.lineNumber, pattern.columnNumber)
  }

}
