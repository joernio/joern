package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.datastructures.{BlockScope, MethodScope, NamespaceScope, TypeScope}
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.utils.NodeBuilders.{newMethodReturnNode, newModifierNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.proto.cpg.Cpg.EvaluationStrategies

import scala.util.Try
import io.joern.csharpsrc2cpg.datastructures.FieldDecl
import io.joern.csharpsrc2cpg.utils.Utils.{composeMethodFullName, composeMethodLikeSignature}
import scala.collection.mutable.ArrayBuffer
import io.joern.csharpsrc2cpg.CSharpOperators as CSharpOperators
import io.joern.x2cpg.Defines
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.csharpsrc2cpg.datastructures.EnumScope

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

    // TODO: Check if any explicit constructor / static constructor decls exists,
    //  if it doesn't, need to add in default constructor and static constructor and
    //  pull all field initializations into them.

    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
    Seq(typeDeclAst)
  }

  protected def astForRecordDeclaration(recordDecl: DotNetNodeInfo): Seq[Ast] = {
    val name     = nameFromNode(recordDecl)
    val fullName = astFullName(recordDecl)
    val typeDecl = typeDeclNode(recordDecl, name, fullName, relativeFileName, code(recordDecl))
    scope.pushNewScope(TypeScope(fullName))
    val modifiers = astForModifiers(recordDecl)

    // Covers the case where record type can be declared as `record Person(string Name);`
    // Here, Person should be a TypeDecl and Name should be a member instead of a parameter
    val membersFromParams = Try {
      recordDecl
        .json(ParserKeys.ParameterList)(ParserKeys.Parameters)
        .arr
        .map(createDotNetNodeInfo)
        .toSeq
    }.toOption
      .getOrElse(Seq.empty)
      .map { paramNode =>
        val name         = nameFromNode(paramNode)
        val typeFullName = nodeTypeFullName(paramNode)
        Ast(memberNode(paramNode, name, paramNode.code, typeFullName))
      }

    val members =
      astForMembers(recordDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq) ++ membersFromParams
    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
    Seq(typeDeclAst)
  }

  protected def astForEnumDeclaration(enumDecl: DotNetNodeInfo): Seq[Ast] = {
    val name     = nameFromNode(enumDecl)
    val fullName = astFullName(enumDecl)
    val aliasFor = Try(enumDecl.json(ParserKeys.BaseList)(ParserKeys.Types).arr.map(createDotNetNodeInfo).head).toOption
      .map(nodeTypeFullName)
      .getOrElse(DotNetTypeMap(BuiltinTypes.Int))

    val typeDecl = typeDeclNode(enumDecl, name, fullName, relativeFileName, code(enumDecl))
    scope.pushNewScope(EnumScope(fullName, aliasFor))
    val modifiers = astForModifiers(enumDecl)

    val members = astForMembers(enumDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
    Seq(typeDeclAst)
  }

  /** Creates enum members. These are associated with integer types, and by default, are `int` types.
    * @see
    *   <a href="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/enum">Enumeration
    *   Types</a>
    */
  protected def astForEnumMemberDeclaration(enumMemberDecl: DotNetNodeInfo): Seq[Ast] = {
    val name = nameFromNode(enumMemberDecl)
    val typeFullName = scope
      .peekScope()
      .collectFirst { case EnumScope(fullName, aliasFor) => aliasFor }
      .getOrElse(DotNetTypeMap(BuiltinTypes.Int))
    val member    = memberNode(enumMemberDecl, name, code(enumMemberDecl), typeFullName)
    val modifiers = astForModifiers(enumMemberDecl)

    val memberAst = Ast(member).withChildren(modifiers)
    Seq(memberAst)
  }

  protected def astForFieldDeclaration(fieldDecl: DotNetNodeInfo): Seq[Ast] = {
    val isStatic = fieldDecl
      .json(ParserKeys.Modifiers)
      .arr
      .flatMap(astForModifier)
      .flatMap(_.root)
      .collectFirst { case x: NewModifier => x.modifierType }
      .contains(ModifierTypes.STATIC)

    val declarationNode = createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    val declAsts        = astForVariableDeclaration(declarationNode, isStatic)

    val memberNodes = declAsts
      .flatMap(_.nodes.collectFirst { case x: NewIdentifier => x })
      .map(x => memberNode(declarationNode, x.name, code(declarationNode), x.typeFullName))
    memberNodes.map(Ast(_).withChildren(astForModifiers(fieldDecl)))
  }

  protected def astForLocalDeclarationStatement(localDecl: DotNetNodeInfo): Seq[Ast] = {
    astForVariableDeclaration(createDotNetNodeInfo(localDecl.json(ParserKeys.Declaration)))
  }

  protected def astForVariableDeclaration(varDecl: DotNetNodeInfo, isStatic: Boolean): Seq[Ast] = {
    val typeFullName = nodeTypeFullName(varDecl)
    varDecl
      .json(ParserKeys.Variables)
      .arr
      .map(createDotNetNodeInfo)
      .flatMap(x => {
        val name    = nameFromNode(x)
        val hasInit = !x.json(ParserKeys.Initializer).isNull
        scope.pushField(FieldDecl(name, typeFullName, isStatic, hasInit, x))
        astForVariableDeclarator(x, typeFullName)
      })
      .toSeq
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

  protected def astForVariableDeclarator(varDecl: DotNetNodeInfo, typeFullName: String): Seq[Ast] = {
    // Create RHS AST first to propagate types
    val initializerJson = varDecl.json(ParserKeys.Initializer)
    val rhs             = if (!initializerJson.isNull) astForNode(createDotNetNodeInfo(initializerJson)) else Seq.empty
    val rhsTypeFullName =
      if (typeFullName == Defines.Any || typeFullName == "var") getTypeFullNameFromAstNode(rhs) else typeFullName

    val name          = nameFromNode(varDecl)
    val identifierAst = astForIdentifier(varDecl, rhsTypeFullName)
    val _localNode    = localNode(varDecl, name, name, rhsTypeFullName)
    val localNodeAst  = Ast(_localNode)
    scope.addToScope(name, _localNode)
    if (initializerJson.isNull) {
      val assignmentNode = callNode(
        varDecl,
        code(varDecl),
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        None
      )
      // Implicitly assigned to `null`
      Seq(
        callAst(assignmentNode, Seq(identifierAst, Ast(literalNode(varDecl, BuiltinTypes.Null, BuiltinTypes.Null)))),
        localNodeAst
      )
    } else {
      val assignmentNode = callNode(
        varDecl,
        code(varDecl),
        Operators.assignment,
        Operators.assignment,
        DispatchTypes.STATIC_DISPATCH,
        None,
        Some(rhsTypeFullName)
      )

      Seq(callAst(assignmentNode, identifierAst +: rhs), localNodeAst)
    }
  }

  protected def astForConstructorDeclaration(constructorDecl: DotNetNodeInfo): Seq[Ast] = {
    val params = constructorDecl
      .json(ParserKeys.ParameterList)
      .obj(ParserKeys.Parameters)
      .arr
      .map(createDotNetNodeInfo)
      .zipWithIndex
      .map(astForParameter)
      .toSeq
    // TODO: Decide on proper return type for constructors. No `ReturnType` key in C# JSON for constructors so just
    //  defaulted to void (same as java) for now
    val methodReturn = newMethodReturnNode(BuiltinTypes.Void, None, None, None)
    val signature = composeMethodLikeSignature(
      BuiltinTypes.Void,
      params.flatMap(_.nodes.collectFirst { case x: NewMethodParameterIn => x.typeFullName })
    )
    val typeDeclFullName = scope.surroundingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace);

    val modifiers =
      (astForModifiers(constructorDecl) :+ Ast(newModifierNode(ModifierTypes.CONSTRUCTOR)))
        .flatMap(_.nodes)
        .collect { case x: NewModifier =>
          x
        }
        .filter(_.modifierType != ModifierTypes.INTERNAL)

    val isStaticConstructor = modifiers.exists(_.modifierType == ModifierTypes.STATIC)

    val (name, fullName) = if (isStaticConstructor) {
      (Defines.StaticInitMethodName, composeMethodFullName(typeDeclFullName, Defines.StaticInitMethodName, signature))
    } else {
      (Defines.ConstructorMethodName, composeMethodFullName(typeDeclFullName, Defines.ConstructorMethodName, signature))
    }

    scope.pushNewScope(MethodScope(fullName))

    // 1. Do we have fields? Then we need to initialize them explicitly
    val (staticFields, dynamicFields) = scope.getFieldsInScope.partition(_.isStatic)

    val prefixAsts = if (isStaticConstructor && staticFields.nonEmpty) {
      // 2. If this has a static modifier, then we create a prefixAst list of the static field initializers
      astVariableDeclarationForInitializedFields(staticFields)
    } else if (dynamicFields.nonEmpty) {
      // 3. If this does not have a static modifier, then we create a prefixAst list of the dynamic field initializers
      astVariableDeclarationForInitializedFields(dynamicFields)
    } else {
      Seq.empty
    }

    val body = astForBlock(createDotNetNodeInfo(constructorDecl.json(ParserKeys.Body)), prefixAsts = prefixAsts.toList)

    scope.popScope()

    val methodNode_ =
      methodNode(constructorDecl, name, code(constructorDecl), fullName, Option(signature), relativeFileName)

    val thisNode =
      if (!isStaticConstructor) astForThisNode(constructorDecl)
      else Ast()
    Seq(methodAst(methodNode_, thisNode +: params, body, methodReturn, modifiers))
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

    val methodReturnNode = createDotNetNodeInfo(methodDecl.json(ParserKeys.ReturnType))
    val methodReturnStr  = nodeTypeFullName(methodReturnNode)
    val methodReturn     = nodeToMethodReturn(methodReturnNode)
    val signature =
      methodSignature(methodReturn, params.flatMap(_.nodes.collectFirst { case x: NewMethodParameterIn => x }))
    val fullName    = s"${astFullName(methodDecl)}:$signature"
    val methodNode_ = methodNode(methodDecl, name, code(methodDecl), fullName, Option(signature), relativeFileName)
    scope.pushNewScope(MethodScope(fullName))

    // In the case of interfaces, the method body may not be present
    val jsonBody = methodDecl.json(ParserKeys.Body)
    val body =
      if (!jsonBody.isNull) astForBlock(createDotNetNodeInfo(jsonBody))
      else Ast(blockNode(methodDecl)) // Creates an empty block
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

  protected def astVariableDeclarationForInitializedFields(fieldDecls: Seq[FieldDecl]): Seq[Ast] = {
    fieldDecls.filter(_.isInitialized).flatMap { case FieldDecl(name, typeFullName, _, isInitialized, node) =>
      astForVariableDeclarator(node, nodeTypeFullName(node))
    }
  }
}
