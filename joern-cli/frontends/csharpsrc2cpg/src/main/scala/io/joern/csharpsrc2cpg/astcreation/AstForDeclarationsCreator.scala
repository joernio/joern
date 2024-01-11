package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.datastructures.{BlockScope, MethodScope, NamespaceScope, TypeScope}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.datastructures.Stack.StackWrapper
import io.joern.x2cpg.utils.NodeBuilders.newModifierNode
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.proto.cpg.Cpg.EvaluationStrategies

import scala.util.Try

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
    val members   = astForMembers(classDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
    Seq(typeDeclAst)
  }

  protected def astForFieldDeclaration(fieldDecl: DotNetNodeInfo): Seq[Ast] = {
    val declarationNode = createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    val declAsts        = astForVariableDeclaration(declarationNode)
    // TODO: Create a <clinit>-style method with declAsts body
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
      .flatMap { astForVariableDeclarator(_, typeFullName) }
      .toSeq
  }

  protected def astForVariableDeclarator(varDecl: DotNetNodeInfo, typeFullName: String): List[Ast] = {
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
      List(
        callAst(assignmentNode, Seq(identifierAst, Ast(literalNode(varDecl, BuiltinTypes.Null, BuiltinTypes.Null)))),
        localNodeAst
      )
    } else {
      val rhs = astForNode(createDotNetNodeInfo(initializerJson))
      List(callAst(assignmentNode, identifierAst +: rhs), localNodeAst)
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
    scope.pushNewScope(MethodScope(fullName))
    val body = astForMethodBody(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)))
    scope.popScope()
    val modifiers = astForModifiers(methodDecl).flatMap(_.nodes).collect { case x: NewModifier => x }
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
    val typeFullName = scope.surroundingTypeDeclFullName.getOrElse("ANY")
    val param = parameterInNode(methodDecl, name, name, 0, false, EvaluationStrategies.BY_SHARING.name, typeFullName)
    Ast(param)
  }

  private def astForMethodBody(body: DotNetNodeInfo): Ast = {
    val block = blockNode(body)
    scope.pushNewScope(BlockScope)
    val statements = body.json(ParserKeys.Statements).arr.flatMap(astForNode).toList
    val _blockAst  = blockAst(block, statements)
    scope.popScope()
    _blockAst
  }

  private def nodeToMethodReturn(methodReturn: DotNetNodeInfo): NewMethodReturn = {
    methodReturnNode(
      methodReturn,
      Try(methodReturn.json(ParserKeys.Value).str)
        .orElse(Try(methodReturn.json(ParserKeys.Keyword).obj(ParserKeys.Value).str))
        .orElse(Try(methodReturn.code))
        .getOrElse("ANY")
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
