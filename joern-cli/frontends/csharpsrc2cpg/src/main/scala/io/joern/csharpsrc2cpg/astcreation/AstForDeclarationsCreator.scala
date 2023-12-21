package io.joern.csharpsrc2cpg.astcreation

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
    val nameNode = createDotNetNodeInfo(namespace.json(ParserKeys.Name))
    val fullName = astFullName(nameNode)
    val name     = fullName.split('.').filterNot(_.isBlank).lastOption.getOrElse(fullName)
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .code(code(namespace))
      .lineNumber(line(nameNode))
      .columnNumber(columnEnd(nameNode))
      .filename(relativeFileName)
      .fullName(fullName)
    methodAstParentStack.push(namespaceBlock)
    val memberAsts = namespace.json(ParserKeys.Members).arr.flatMap(astForNode).toSeq
    methodAstParentStack.pop()
    Seq(Ast(namespaceBlock).withChildren(memberAsts))
  }

  protected def astForClassDeclaration(classDecl: DotNetNodeInfo): Seq[Ast] = {
    val name     = nameFromIdentifier(classDecl)
    val fullName = astFullName(classDecl)
    val typeDecl = typeDeclNode(classDecl, name, fullName, relativeFileName, code(classDecl))
    methodAstParentStack.push(typeDecl)
    val modifiers = astForModifiers(classDecl)
    val members   = astForMembers(classDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    methodAstParentStack.pop()
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

  protected def astForVariableDeclaration(varDecl: DotNetNodeInfo): Seq[Ast] = {
    val typeFullName = nodeTypeFullName(varDecl)
    varDecl
      .json(ParserKeys.Variables)
      .arr
      .map(createDotNetNodeInfo)
      .map(astForVariableDeclarator(_, typeFullName))
      .toSeq
  }

  protected def astForVariableDeclarator(varDecl: DotNetNodeInfo, typeFullName: String): Ast = {
    val name          = nameFromNode(varDecl)
    val identifierAst = Ast(identifierNode(varDecl, name, name, typeFullName))
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
      callAst(assignmentNode, Seq(identifierAst, Ast(literalNode(varDecl, BuiltinTypes.Null, BuiltinTypes.Null))))
    } else {
      val rhs = astForNode(createDotNetNodeInfo(initializerJson))
      callAst(assignmentNode, identifierAst +: rhs)
    }
  }

  protected def astForMethodDeclaration(methodDecl: DotNetNodeInfo): Seq[Ast] = {
    val name = nameFromIdentifier(methodDecl)
    val params = methodDecl
      .json(ParserKeys.ParameterList)
      .obj(ParserKeys.Parameters)
      .arr
      .map(createDotNetNodeInfo)
      .zipWithIndex
      .map(astForParameter)
      .toSeq
    val body         = astForMethodBody(createDotNetNodeInfo(methodDecl.json(ParserKeys.Body)))
    val methodReturn = nodeToMethodReturn(createDotNetNodeInfo(methodDecl.json(ParserKeys.ReturnType)))
    val signature =
      methodSignature(methodReturn, params.flatMap(_.nodes.collectFirst { case x: NewMethodParameterIn => x }))
    val fullName    = s"${astFullName(methodDecl)}:$signature"
    val methodNode_ = methodNode(methodDecl, name, code(methodDecl), fullName, Option(signature), relativeFileName)
    val modifiers   = astForModifiers(methodDecl).flatMap(_.nodes).collect { case x: NewModifier => x }
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
    val name = "this"
    val typeFullName =
      methodAstParentStack.headOption.map(_.properties.getOrElse(PropertyNames.FULL_NAME, "ANY").toString)
    val param =
      parameterInNode(methodDecl, name, name, 0, false, EvaluationStrategies.BY_SHARING.name, typeFullName)
    Ast(param)
  }

  private def astForMethodBody(body: DotNetNodeInfo): Ast = {
    val block      = blockNode(body)
    val statements = List.empty // TODO
    blockAst(block, statements)
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
      case Nil
          if methodAstParentStack.isEmpty || !methodAstParentStack
            .take(2)
            .map(_.label())
            .distinct
            .contains(NodeTypes.METHOD) =>
        Ast(newModifierNode(ModifierTypes.INTERNAL))
      // Private is default for nested definitions
      case Nil
          if methodAstParentStack.headOption.exists(x => x.isInstanceOf[NewMethod] || x.isInstanceOf[NewTypeDecl]) =>
        Ast(newModifierNode(ModifierTypes.PRIVATE))
      case _ => Ast()

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
