package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.CSharpOperators
import io.joern.csharpsrc2cpg.datastructures.{BlockScope, MethodScope, NamespaceScope, TypeScope}
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.FieldDeclaration
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.proto.cpg.Cpg.EvaluationStrategies

import scala.util.Try
import io.joern.csharpsrc2cpg.datastructures.FieldDecl

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForNamespaceDeclaration(namespace: DotNetNodeInfo): Seq[Ast] = {
    val fullName = astFullName(namespace)
    val name     = fullName.split('.').filterNot(_.isBlank).lastOption.getOrElse(fullName)
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .code(code(namespace))
      .lineNumber(line(namespace))
      .columnNumber(columnEnd(namespace))
      .filename(relativeFileName)
      .fullName(fullName)
    scope.pushNewScope(NamespaceScope(fullName))
    val memberAsts = namespace.json(ParserKeys.Members).arr.flatMap(astForNode).toSeq
    scope.popScope()
    Seq(Ast(namespaceBlock).withChildren(memberAsts))
  }

  protected def astForClassDeclaration(classDecl: DotNetNodeInfo): Seq[Ast] = {
    val name     = nameFromNode(classDecl)
    val fullName = astFullName(classDecl)
    val typeDecl = typeDeclNode(classDecl, name, fullName, relativeFileName, code(classDecl))
    scope.pushNewScope(TypeScope(fullName))
    val modifiers = astForModifiers(classDecl)
    val (fields, nonFields) = classDecl
      .json(ParserKeys.Members)
      .arr
      .map(createDotNetNodeInfo)
      .toSeq
      .partition { x =>
        x.node match
          case FieldDeclaration => true
          case _                => false
      }
      
    val members = astForMembers(fields) ++ astForMembers(nonFields)

    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
    Seq(typeDeclAst)
  }

  protected def astForFieldDeclaration(fieldDecl: DotNetNodeInfo): Seq[Ast] = {
    val declarationNode = createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    val declAsts        = astForVariableDeclaration(declarationNode)

    val memberNodes = declAsts
      .flatMap(_.nodes.collectFirst { case x: NewIdentifier => x })
      .map(x => memberNode(declarationNode, x.name, code(declarationNode), x.typeFullName))
    memberNodes.map(Ast(_).withChildren(astForModifiers(fieldDecl)))
  }

  protected def astForLocalDeclarationStatement(localDecl: DotNetNodeInfo): Seq[Ast] = {
    astForVariableDeclaration(createDotNetNodeInfo(localDecl.json(ParserKeys.Declaration)))
  }

  protected def astForVariableDeclaration(varDecl: DotNetNodeInfo): Seq[Ast] = {
    val typeFullName = nodeTypeFullName(varDecl)
    varDecl
      .json(ParserKeys.Variables)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(astForVariableDeclarator(_, typeFullName))
      .toSeq
  }

  protected def astForVariableDeclarator(decl: DotNetNodeInfo, typeFullName: String): Seq[Ast] = {
    val varDecl = decl.node match
      case FieldDeclaration =>
        val name    = nameFromNode(decl)
        val hasInit = decl.json(ParserKeys.Initializer).isNull
        val isStatic = astForModifier(decl.json)
          .flatMap(_.root)
          .collectFirst { case x: NewModifier => x.modifierType }
          .contains(ModifierTypes.STATIC)
        val fieldDecl = createDotNetNodeInfo(decl.json(ParserKeys.Declaration))
        scope.pushField(FieldDecl(name, isStatic, hasInit, fieldDecl))
        fieldDecl
      case _ => decl

    val name          = nameFromNode(varDecl)
    val identifierAst = astForIdentifier(varDecl, typeFullName)
    val _localNode    = localNode(varDecl, name, name, typeFullName)
    val localNodeAst  = Ast(_localNode)
    scope.addToScope(name, _localNode)
    val assignmentNode = callNode(
      varDecl,
      code(varDecl),
      Operators.assignment,
      Operators.assignment,
      DispatchTypes.STATIC_DISPATCH,
      None,
      None
    )
    val initializerJson = varDecl.json(ParserKeys.Initializer)
    if (initializerJson.isNull) {
      // Implicitly assigned to `null`
      Seq(
        callAst(assignmentNode, Seq(identifierAst, Ast(literalNode(varDecl, BuiltinTypes.Null, BuiltinTypes.Null)))),
        localNodeAst
      )
    } else {
      val rhs = astForNode(createDotNetNodeInfo(initializerJson))
      Seq(callAst(assignmentNode, identifierAst +: rhs), localNodeAst)
    }
  }

  protected def astForMethodDeclaration(methodDecl: DotNetNodeInfo): Seq[Ast] = {
    val name = nameFromNode(methodDecl)
    val params = methodDecl
      .json(ParserKeys.ParameterList)
      .obj(ParserKeys.Parameters)
      .arr
      .map(createDotNetNodeInfo)
      .zipWithIndex
      .map(astForParameter)
      .toSeq
    val methodReturn = nodeToMethodReturn(createDotNetNodeInfo(methodDecl.json(ParserKeys.ReturnType)))
    val signature =
      methodSignature(methodReturn, params.flatMap(_.nodes.collectFirst { case x: NewMethodParameterIn => x }))
    val fullName    = s"${astFullName(methodDecl)}:$signature"
    val methodNode_ = methodNode(methodDecl, name, code(methodDecl), fullName, Option(signature), relativeFileName)
    val modifiers   = astForModifiers(methodDecl).flatMap(_.nodes).collect { case x: NewModifier => x }
    scope.pushNewScope(MethodScope(fullName))

    // 1. Is this a constructors/static initializer method?
    val body = if (scope.surroundingTypeDeclFullName.contains(name)) {
      // 2. Do we have fields? Then we need to initialize them explicitly
      val (staticFields, dynamicFields) = scope.getFieldsInScope.partition(_.isStatic)
      // 3. If this has a static modifier, then we prepend the initializers of our static fields
      if (modifiers.exists(_.modifierType == "STATIC") && staticFields.nonEmpty) {
        // TODO: Filter out `decl` if there is already an explicit assignment for that variable
        val decls = staticFields.flatMap { case FieldDecl(name, _, isInitialized, node) =>
          astForVariableDeclarator(node, nodeTypeFullName(node))
        }
        astForBlock(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)), prefixAsts = decls)
      } else if (dynamicFields.nonEmpty) {
        // 4. If this does not have a static modifier, then we prepend the initializers of our dynamic fields
        // TODO: Filter out `decl` if there is already an explicit assignment for that variable
        val decls = dynamicFields.flatMap { case FieldDecl(name, _, isInitialized, node) =>
          astForVariableDeclarator(node, nodeTypeFullName(node))
        }
        astForBlock(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)), prefixAsts = decls)
      } else {
        // If this is not a constructors/static initializer method, then build body as normal
        astForBlock(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)))
      }
    } else {
      // If this is not a constructors/static initializer method, then build body as normal
      astForBlock(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)))
    }

    scope.popScope()

    val thisNode =
      if (!modifiers.exists(_.modifierType == ModifierTypes.STATIC)) astForThisNode(methodDecl)
      else Ast()
    Seq(methodAst(methodNode_, thisNode +: params, body, methodReturn, modifiers))
  }

  private def methodSignature(methodReturn: NewMethodReturn, params: Seq[NewMethodParameterIn]): String = {
    s"${methodReturn.typeFullName}(${params.map(_.typeFullName).mkString(",")})"
  }

  private def astForParameter(paramNode: DotNetNodeInfo, idx: Int): Ast = {
    val name               = nameFromNode(paramNode)
    val isVariadic         = false                                // TODO
    val typeFullName       = nodeTypeFullName(paramNode)
    val evaluationStrategy = EvaluationStrategies.BY_SHARING.name // TODO
    val param =
      parameterInNode(paramNode, name, code(paramNode), idx + 1, isVariadic, evaluationStrategy, Option(typeFullName))
    Ast(param)
  }

  private def astForThisNode(methodDecl: DotNetNodeInfo): Ast = {
    val name         = "this"
    val typeFullName = scope.surroundingTypeDeclFullName.getOrElse(Defines.Any)
    val param = parameterInNode(methodDecl, name, name, 0, false, EvaluationStrategies.BY_SHARING.name, typeFullName)
    Ast(param)
  }

  protected def astForBlock(
    body: DotNetNodeInfo,
    code: Option[String] = None,
    prefixAsts: List[Ast] = List.empty
  ): Ast = {
    val block = blockNode(body)
    code.foreach(block.code(_))
    scope.pushNewScope(BlockScope)
    val statements = body.json(ParserKeys.Statements).arr.flatMap(astForNode).toList
    val _blockAst  = blockAst(block, prefixAsts ++ statements)
    scope.popScope()
    _blockAst
  }

  private def nodeToMethodReturn(methodReturn: DotNetNodeInfo): NewMethodReturn = {
    methodReturnNode(
      methodReturn,
      Try(methodReturn.json(ParserKeys.Value).str)
        .orElse(Try(methodReturn.json(ParserKeys.Keyword).obj(ParserKeys.Value).str))
        .orElse(Try(methodReturn.code))
        .getOrElse(Defines.Any)
    )
  }

  /** Parses the modifier array and handles implicit defaults.
    * @see
    *   https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/access-modifiers
    */
  private def astForModifiers(declaration: DotNetNodeInfo): Seq[Ast] = {
    val explicitModifiers = declaration.json(ParserKeys.Modifiers).arr.flatMap(astForModifier).toList
    val accessModifiers = explicitModifiers
      .flatMap(_.nodes)
      .collect { case x: NewModifier => x.modifierType } intersect List(
      ModifierTypes.PUBLIC,
      ModifierTypes.PRIVATE,
      ModifierTypes.INTERNAL,
      ModifierTypes.PROTECTED
    )
    val implicitAccessModifier = accessModifiers match
      // Internal is default for top-level definitions
      case Nil if scope.isTopLevel => Ast(newModifierNode(ModifierTypes.INTERNAL))
      // Private is default for nested definitions
      case Nil => Ast(newModifierNode(ModifierTypes.PRIVATE))
      case _   => Ast()

    implicitAccessModifier :: explicitModifiers
  }

  private def astForModifier(modifier: ujson.Value): Option[Ast] = {
    Option {
      modifier(ParserKeys.Value).str match
        case "public"   => newModifierNode(ModifierTypes.PUBLIC)
        case "private"  => newModifierNode(ModifierTypes.PRIVATE)
        case "internal" => newModifierNode(ModifierTypes.INTERNAL)
        case "static"   => newModifierNode(ModifierTypes.STATIC)
        case "readonly" => newModifierNode(ModifierTypes.READONLY)
        case "virtual"  => newModifierNode(ModifierTypes.VIRTUAL)
        case x =>
          logger.warn(s"Unhandled modifier name '$x'")
          null
    }.map(Ast(_))
  }

}
