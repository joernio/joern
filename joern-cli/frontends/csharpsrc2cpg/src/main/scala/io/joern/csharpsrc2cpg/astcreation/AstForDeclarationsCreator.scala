package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.{CSharpModifiers, Constants}
import io.joern.csharpsrc2cpg.astcreation.AstParseLevel.FULL_AST
import io.joern.csharpsrc2cpg.astcreation.BuiltinTypes.DotNetTypeMap
import io.joern.csharpsrc2cpg.datastructures.*
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetNodeInfo, ParserKeys}
import io.joern.csharpsrc2cpg.utils.Utils.{
  composeGetterName,
  composeMethodFullName,
  composeMethodLikeSignature,
  composeSetterName
}
import io.joern.x2cpg.utils.NodeBuilders.{newMethodReturnNode, newModifierNode}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.*
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.proto.cpg.Cpg.EvaluationStrategies

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer
import scala.util.Try

trait AstForDeclarationsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForNamespaceDeclaration(namespace: DotNetNodeInfo): Seq[Ast] = {
    @tailrec
    def recurseNamespace(parts: List[String], prefix: List[String] = List.empty): Unit = {
      parts match {
        case head :: tail =>
          val currentFullName = prefix :+ head
          scope.pushNewScope(NamespaceScope(currentFullName.mkString(".")))
          recurseNamespace(tail, currentFullName)
        case Nil => // nothing
      }
    }

    val fullName = astFullName(namespace)

    val namespaceParts = fullName.split("[.]").toList
    recurseNamespace(namespaceParts)

    val name = fullName.split('.').filterNot(_.isBlank).lastOption.getOrElse(fullName)
    val namespaceBlock = NewNamespaceBlock()
      .name(name)
      .code(code(namespace))
      .lineNumber(line(namespace))
      .columnNumber(columnEnd(namespace))
      .filename(relativeFileName)
      .fullName(fullName)
    val memberAsts = namespace.json(ParserKeys.Members).arr.flatMap(astForNode).toSeq
    namespaceParts.foreach(_ => scope.popScope())
    Seq(Ast(namespaceBlock).withChildren(memberAsts))
  }

  protected def astForClassDeclaration(classDecl: DotNetNodeInfo): Seq[Ast] = {
    val name     = nameFromNode(classDecl)
    val fullName = astFullName(classDecl)
    val inheritsFromTypeFullName = Try(classDecl.json(ParserKeys.BaseList)).toOption match {
      case Some(baseList: ujson.Obj) =>
        baseList(ParserKeys.Types).arr.map { t =>
          nodeTypeFullName(createDotNetNodeInfo(t(ParserKeys.Type)))
        }.toSeq
      case _ => Seq.empty
    }

    inheritsFromTypeFullName.foreach(scope.pushTypeToScope)

    val annotationAsts =
      Try(classDecl.json(ParserKeys.AttributeLists))
        .map(_.arr.map(createDotNetNodeInfo).flatMap(astForAttributeLists).toSeq)
        .getOrElse(Seq.empty)

    val typeDecl =
      typeDeclNode(classDecl, name, fullName, relativeFileName, code(classDecl), inherits = inheritsFromTypeFullName)
    scope.pushNewScope(TypeScope(fullName))
    val modifiers = astForModifiers(classDecl)
    val members = astForMembers(classDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
      ++ addConstructorWithFieldInitializationsIfNeeded(fullName)
      ++ addStaticConstructorWithFieldInitializationsIfNeeded(fullName)

    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
      .withChildren(annotationAsts)
    Seq(typeDeclAst)
  }

  private def addConstructorWithFieldInitializationsIfNeeded(typeDeclFullName: String): Seq[Ast] = {
    val dynamicFields = scope.getFieldsInScope.filter(f => !f.isStatic && f.isInitialized)
    val hasExplicitCtor =
      scope.tryResolveTypeReference(typeDeclFullName).exists(_.methods.exists(_.name == Defines.ConstructorMethodName))
    // We should only create the constructor when we are the FULL_AST parseLevel. Otherwise, hasExplicitCtor will
    // not be accurate.
    val shouldBuildCtor = dynamicFields.nonEmpty && !hasExplicitCtor && parseLevel == FULL_AST

    if (shouldBuildCtor) {
      val methodReturn = newMethodReturnNode(DotNetTypeMap(BuiltinTypes.Void), None, None, None)
      val signature    = composeMethodLikeSignature(methodReturn.typeFullName)
      val modifiers    = Seq(newModifierNode(ModifierTypes.CONSTRUCTOR), newModifierNode(ModifierTypes.INTERNAL))
      val name         = Defines.ConstructorMethodName
      val fullName     = composeMethodFullName(typeDeclFullName, name, signature)

      val body = {
        scope.pushNewScope(MethodScope(fullName))
        val fieldInitAssignmentAsts = astVariableDeclarationForInitializedFields(dynamicFields)
        scope.popScope()
        Ast(NewBlock().typeFullName(Defines.Any)).withChildren(fieldInitAssignmentAsts)
      }

      val methodNode_ = NewMethod()
        .name(name)
        .fullName(fullName)
        .signature(signature)
        .filename(relativeFileName)

      val parameterNodes = Seq(
        NewMethodParameterIn()
          .name(Constants.This)
          .code(Constants.This)
          .typeFullName(typeDeclFullName)
          .evaluationStrategy(EvaluationStrategies.BY_SHARING.name)
          .isVariadic(false)
          .index(0)
      )

      methodAst(methodNode_, parameterNodes.map(Ast(_)), body, methodReturn, modifiers) :: Nil
    } else {
      Seq.empty
    }
  }

  private def addStaticConstructorWithFieldInitializationsIfNeeded(typeDeclFullname: String): Seq[Ast] = {
    val staticFields = scope.getFieldsInScope.filter(f => f.isStatic && f.isInitialized)
    val hasExplicitCtor =
      scope.tryResolveTypeReference(typeDeclFullname).exists(_.methods.exists(_.name == Defines.StaticInitMethodName))
    val shouldBuildCtor = staticFields.nonEmpty && !hasExplicitCtor && parseLevel == FULL_AST

    if (shouldBuildCtor) {
      val methodReturn = newMethodReturnNode(DotNetTypeMap(BuiltinTypes.Void), None, None, None)
      val signature    = composeMethodLikeSignature(methodReturn.typeFullName)
      val modifiers = Seq(
        newModifierNode(ModifierTypes.CONSTRUCTOR),
        newModifierNode(ModifierTypes.INTERNAL),
        newModifierNode(ModifierTypes.STATIC)
      )
      val name     = Defines.StaticInitMethodName
      val fullName = composeMethodFullName(typeDeclFullname, name, signature)

      val body = {
        scope.pushNewScope(MethodScope(fullName))
        val fieldInitAssignmentAsts = astVariableDeclarationForInitializedFields(staticFields)
        scope.popScope()
        Ast(NewBlock().typeFullName(Defines.Any)).withChildren(fieldInitAssignmentAsts)
      }

      val methodNode_ = NewMethod()
        .name(name)
        .fullName(fullName)
        .signature(signature)
        .filename(relativeFileName)

      methodAst(methodNode_, Nil, body, methodReturn, modifiers) :: Nil
    } else {
      Nil
    }
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

    val annotationAsts =
      Try(recordDecl.json(ParserKeys.AttributeLists))
        .map(_.arr.map(createDotNetNodeInfo).flatMap(astForAttributeLists).toSeq)
        .getOrElse(Seq.empty)

    val members =
      astForMembers(recordDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq) ++ membersFromParams
    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
      .withChildren(annotationAsts)
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

    val annotationAsts =
      Try(enumDecl.json(ParserKeys.AttributeLists))
        .map(_.arr.map(createDotNetNodeInfo).flatMap(astForAttributeLists).toSeq)
        .getOrElse(Seq.empty)

    val members = astForMembers(enumDecl.json(ParserKeys.Members).arr.map(createDotNetNodeInfo).toSeq)
    scope.popScope()
    val typeDeclAst = Ast(typeDecl)
      .withChildren(modifiers)
      .withChildren(members)
      .withChildren(annotationAsts)
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
      .collectFirst { case EnumScope(_, aliasFor) => aliasFor }
      .getOrElse(DotNetTypeMap(BuiltinTypes.Int))
    val member    = memberNode(enumMemberDecl, name, code(enumMemberDecl), typeFullName)
    val modifiers = astForModifiers(enumMemberDecl)

    val memberAst = Ast(member).withChildren(modifiers)
    Seq(memberAst)
  }

  protected def astForFieldDeclaration(fieldDecl: DotNetNodeInfo): Seq[Ast] = {
    val modifiers    = modifiersForNode(fieldDecl)
    val isStatic     = modifiers.exists(_.modifierType == ModifierTypes.STATIC)
    val modifierAsts = modifiers.map(Ast(_))

    val declarationNode = createDotNetNodeInfo(fieldDecl.json(ParserKeys.Declaration))
    val declAsts        = astForVariableDeclaration(declarationNode, isStatic)

    val annotationAsts =
      Try(fieldDecl.json(ParserKeys.AttributeLists))
        .map(_.arr.map(createDotNetNodeInfo).flatMap(astForAttributeLists).toSeq)
        .getOrElse(Seq.empty)

    val memberNodes = declAsts
      .flatMap(_.nodes.collectFirst { case x: NewIdentifier => x })
      .map(x => memberNode(declarationNode, x.name, code(declarationNode), x.typeFullName))
    memberNodes.map(Ast(_).withChildren(annotationAsts).withChildren(modifierAsts))
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
        astForVariableDeclarator(x, typeFullName, shouldPushVariable = false)
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

  protected def astForVariableDeclarator(
    varDecl: DotNetNodeInfo,
    typeFullName: String,
    shouldPushVariable: Boolean = true
  ): Seq[Ast] = {
    // Create RHS AST first to propagate types
    val initializerJson = varDecl.json(ParserKeys.Initializer)
    val rhs             = if (!initializerJson.isNull) astForNode(createDotNetNodeInfo(initializerJson)) else Seq.empty
    val rhsTypeFullName =
      if (typeFullName == Defines.Any || typeFullName == "var") getTypeFullNameFromAstNode(rhs)
      else scope.tryResolveTypeReference(typeFullName).map(_.name).getOrElse(typeFullName)

    val name          = nameFromNode(varDecl)
    val identifierAst = astForIdentifier(varDecl, rhsTypeFullName)
    val _localNode    = localNode(varDecl, name, name, rhsTypeFullName)
    val localNodeAst  = Ast(_localNode)

    if (shouldPushVariable) {
      scope.addToScope(name, _localNode)
    }

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
      .map(astForParameter(_, _, None))
      .toSeq
    // TODO: Decide on proper return type for constructors. No `ReturnType` key in C# JSON for constructors so just
    //  defaulted to void (same as java) for now
    val methodReturn     = newMethodReturnNode(DotNetTypeMap(BuiltinTypes.Void), None, None, None)
    val signature        = composeMethodLikeSignature(DotNetTypeMap(BuiltinTypes.Void), params)
    val typeDeclFullName = scope.surroundingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace);

    val modifiers = (modifiersForNode(constructorDecl) :+ newModifierNode(ModifierTypes.CONSTRUCTOR))
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
      if (!isStaticConstructor) astForThisParameter(constructorDecl)
      else Ast()
    Seq(methodAst(methodNode_, thisNode +: params, body, methodReturn, modifiers))
  }

  protected def astForMethodDeclaration(
    methodDecl: DotNetNodeInfo,
    extraModifiers: List[NewModifier] = Nil
  ): Seq[Ast] = {
    val name = nameFromNode(methodDecl)
    val params = methodDecl
      .json(ParserKeys.ParameterList)
      .obj(ParserKeys.Parameters)
      .arr
      .map(createDotNetNodeInfo)
      .zipWithIndex
      .map(astForParameter(_, _, None))
      .toSeq

    val annotationAsts =
      Try(methodDecl.json(ParserKeys.AttributeLists))
        .map(_.arr.map(createDotNetNodeInfo).flatMap(astForAttributeLists).toSeq)
        .getOrElse(Seq.empty)

    val methodReturnAstNode = createDotNetNodeInfo(methodDecl.json(ParserKeys.ReturnType))
    val methodReturn        = methodReturnNode(methodReturnAstNode, nodeTypeFullName(methodReturnAstNode))
    val signature           = composeMethodLikeSignature(methodReturn.typeFullName, params)
    val fullName            = s"${astFullName(methodDecl)}:$signature"
    val methodNode_ = methodNode(methodDecl, name, code(methodDecl), fullName, Option(signature), relativeFileName)
    scope.pushNewScope(MethodScope(fullName))

    // In the case of interfaces, the method body may not be present
    val jsonBody = methodDecl.json(ParserKeys.Body)
    val body =
      if (!jsonBody.isNull && parseLevel == AstParseLevel.FULL_AST) astForBlock(createDotNetNodeInfo(jsonBody))
      else Ast(blockNode(methodDecl)) // Creates an empty block
    scope.popScope()
    val modifiers = modifiersForNode(methodDecl) ++ extraModifiers
    val thisNode =
      if (!modifiers.exists(_.modifierType == ModifierTypes.STATIC)) astForThisParameter(methodDecl)
      else Ast()
    Seq(methodAstWithAnnotations(methodNode_, thisNode +: params, body, methodReturn, modifiers, annotationAsts))
  }

  private def astForParameter(paramNode: DotNetNodeInfo, idx: Int, paramTypeHint: Option[String] = None): Ast = {
    val name               = nameFromNode(paramNode)
    val isVariadic         = false                                // TODO
    val typeFullName       = paramTypeHint.getOrElse(nodeTypeFullName(paramNode))
    val evaluationStrategy = EvaluationStrategies.BY_SHARING.name // TODO
    val param =
      parameterInNode(paramNode, name, code(paramNode), idx + 1, isVariadic, evaluationStrategy, Option(typeFullName))
    scope.addToScope(name, param)
    Ast(param)
  }

  private def astForThisParameter(methodDecl: DotNetNodeInfo): Ast = {
    val name         = Constants.This
    val typeFullName = scope.surroundingTypeDeclFullName.getOrElse(Defines.Any)
    val param = parameterInNode(methodDecl, name, name, 0, false, EvaluationStrategies.BY_SHARING.name, typeFullName)
    Ast(param)
  }

  protected def astForThisReceiver(invocationExpr: DotNetNodeInfo, typeFullName: Option[String] = None): Ast = {
    val name = Constants.This
    val param = identifierNode(
      invocationExpr,
      name,
      name,
      typeFullName.orElse(scope.surroundingTypeDeclFullName).getOrElse(Defines.Any)
    )
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
    val statements = Try(body.json(ParserKeys.Statements)).toOption match {
      case Some(value: ujson.Arr) => value.arr.flatMap(astForNode).toList
      case _                      => List.empty
    }
    val _blockAst = blockAst(block, prefixAsts ++ statements)
    scope.popScope()
    _blockAst
  }

  /** Parses the modifier array and handles implicit defaults.
    * @see
    *   https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/access-modifiers
    */
  private def astForModifiers(declaration: DotNetNodeInfo): Seq[Ast] = {
    modifiersForNode(declaration).map(Ast(_))
  }

  private def modifiersForNode(node: DotNetNodeInfo): Seq[NewModifier] = {
    val explicitModifiers = node.json(ParserKeys.Modifiers).arr.flatMap(readModifier).toList
    val accessModifiers = explicitModifiers.map(_.modifierType) intersect List(
      ModifierTypes.PUBLIC,
      ModifierTypes.PRIVATE,
      ModifierTypes.INTERNAL,
      ModifierTypes.PROTECTED,
      CSharpModifiers.CONST
    )
    val implicitAccessModifier = accessModifiers match
      // Internal is default for top-level definitions
      case Nil if scope.isTopLevel => newModifierNode(ModifierTypes.INTERNAL) :: Nil
      // Private is default for nested definitions
      case Nil => newModifierNode(ModifierTypes.PRIVATE) :: Nil
      case _   => Nil

    implicitAccessModifier ++ explicitModifiers
  }

  private def readModifier(modifier: ujson.Value): Option[NewModifier] = {
    Option {
      modifier(ParserKeys.Value).str match
        case "public"    => newModifierNode(ModifierTypes.PUBLIC)
        case "private"   => newModifierNode(ModifierTypes.PRIVATE)
        case "internal"  => newModifierNode(ModifierTypes.INTERNAL)
        case "static"    => newModifierNode(ModifierTypes.STATIC)
        case "readonly"  => newModifierNode(ModifierTypes.READONLY)
        case "virtual"   => newModifierNode(ModifierTypes.VIRTUAL)
        case "const"     => newModifierNode(CSharpModifiers.CONST)
        case "abstract"  => newModifierNode(ModifierTypes.ABSTRACT)
        case "protected" => newModifierNode(ModifierTypes.PROTECTED)
        case x =>
          logger.warn(s"Unhandled modifier name '$x'")
          null
    }
  }

  private def astForModifier(modifier: ujson.Value): Option[Ast] = {
    readModifier(modifier).map(Ast(_))
  }

  protected def astVariableDeclarationForInitializedFields(fieldDecls: Seq[FieldDecl]): Seq[Ast] = {
    fieldDecls.filter(_.isInitialized).flatMap { case FieldDecl(name, typeFullName, _, isInitialized, node) =>
      astForVariableDeclarator(node, nodeTypeFullName(node), shouldPushVariable = false)
    }
  }

  protected def astForPropertyDeclaration(propertyDecl: DotNetNodeInfo): Seq[Ast] = {
    val accessorList = createDotNetNodeInfo(propertyDecl.json(ParserKeys.AccessorList))
    val accessors    = accessorList.json(ParserKeys.Accessors).arr.map(createDotNetNodeInfo)
    accessors.flatMap(astForPropertyAccessor(_, propertyDecl)).toList
  }

  private def astForPropertyAccessor(accessorDecl: DotNetNodeInfo, propertyDecl: DotNetNodeInfo): Seq[Ast] = {
    accessorDecl.node match
      case GetAccessorDeclaration => astForGetAccessorDeclaration(accessorDecl, propertyDecl)
      case SetAccessorDeclaration => astForSetAccessorDeclaration(accessorDecl, propertyDecl)
      case _ =>
        logger.warn(s"Unhandled property accessor '${accessorDecl.node}'")
        Nil
  }

  private def astForSetAccessorDeclaration(accessorDecl: DotNetNodeInfo, propertyDecl: DotNetNodeInfo): Seq[Ast] = {
    val name         = composeSetterName(nameFromNode(propertyDecl))
    val modifiers    = modifiersForNode(propertyDecl)
    val returnType   = BuiltinTypes.Void
    val valueType    = nodeTypeFullName(propertyDecl)
    val baseType     = scope.surroundingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace)
    val isStatic     = modifiers.exists(_.modifierType == ModifierTypes.STATIC)
    val valueParam   = Ast(NewMethodParameterIn().typeFullName(valueType).name("value").index(1))
    val parameters   = Option.unless(isStatic)(astForThisParameter(propertyDecl)).toList :+ valueParam
    val signature    = composeMethodLikeSignature(returnType, parameters)
    val fullName     = composeMethodFullName(baseType, name, signature)
    val body         = Try(astForBlock(createDotNetNodeInfo(accessorDecl.json(ParserKeys.Body)))).getOrElse(Ast())
    val methodReturn = methodReturnNode(accessorDecl, returnType)
    val methodNode_  = methodNode(accessorDecl, name, fullName, signature, relativeFileName)

    methodAst(methodNode_, parameters, body, methodReturn, modifiers) :: Nil
  }

  private def astForGetAccessorDeclaration(accessorDecl: DotNetNodeInfo, propertyDecl: DotNetNodeInfo): Seq[Ast] = {
    val name         = composeGetterName(nameFromNode(propertyDecl))
    val modifiers    = modifiersForNode(propertyDecl)
    val returnType   = nodeTypeFullName(propertyDecl)
    val baseType     = scope.surroundingTypeDeclFullName.getOrElse(Defines.UnresolvedNamespace)
    val isStatic     = modifiers.exists(_.modifierType == ModifierTypes.STATIC)
    val parameters   = if isStatic then Nil else astForThisParameter(propertyDecl) :: Nil
    val signature    = composeMethodLikeSignature(returnType, parameters)
    val fullName     = composeMethodFullName(baseType, name, signature)
    val body         = Ast(blockNode(accessorDecl))
    val methodReturn = methodReturnNode(accessorDecl, returnType)
    val methodNode_  = methodNode(accessorDecl, name, fullName, signature, relativeFileName)

    methodAst(methodNode_, parameters, body, methodReturn, modifiers) :: Nil
  }

  /** Creates an AST for a simple `x => { ... }` style lambda expression
    *
    * @param lambdaExpression
    *   the expression.
    * @param paramTypeHint
    *   a type that could hint at what the parameter type may be.
    */
  protected def astForSimpleLambdaExpression(
    lambdaExpression: DotNetNodeInfo,
    paramTypeHint: Option[String] = None
  ): Seq[Ast] = {
    // Create method declaration
    val name     = nextClosureName()
    val fullName = s"${scope.surroundingScopeFullName.getOrElse(Defines.UnresolvedNamespace)}.$name"
    // Set parameter type if necessary, which may require the type hint
    val paramType = paramTypeHint.flatMap(AstCreatorHelper.elementTypesFromCollectionType).headOption
    val paramAsts = Try(lambdaExpression.json(ParserKeys.Parameter)).toOption match {
      case Some(parameterObj: ujson.Obj) =>
        Seq(astForParameter(createDotNetNodeInfo(parameterObj), 0, paramType))
      case _ =>
        lambdaExpression
          .json(ParserKeys.ParameterList)
          .obj(ParserKeys.Parameters)
          .arr
          .map(createDotNetNodeInfo)
          .zipWithIndex
          .map(astForParameter(_, _, paramType))
          .toSeq
    }

    scope.pushNewScope(MethodScope(fullName))
    // Handle lambda body
    val bodyJson = createDotNetNodeInfo(lambdaExpression.json(ParserKeys.Body))
    val block    = blockNode(bodyJson)
    scope.pushNewScope(BlockScope)
    val body =
      if (this.parseLevel == AstParseLevel.SIGNATURES) Seq.empty else astForNode(bodyJson)
    val blockAst_ = blockAst(block, body.toList)
    scope.popScope()
    scope.popScope()
    val method = methodNode(
      lambdaExpression,
      name,
      code(lambdaExpression),
      fullName,
      None,
      relativeFileName,
      Option(NodeTypes.METHOD),
      scope.surroundingScopeFullName
    )
    val modifiers = astForModifiers(lambdaExpression).flatMap(_.nodes).collect { case x: NewModifier => x }
    val lambdaReturnType = body.lastOption
      .getOrElse(Ast())
      .nodes
      .filter {
        case x: NewCall => !x.name.startsWith("<operator")
        case _          => true
      }
      .map(Ast.apply)
      .map(getTypeFullNameFromAstNode)
      .headOption
      .getOrElse(Defines.Any)
    val methodReturn = methodReturnNode(lambdaExpression, lambdaReturnType)
    Ast.storeInDiffGraph(methodAst(method, paramAsts, blockAst_, methodReturn, modifiers), diffGraph)
    // Create type decl
    val lambdaTypeDecl = typeDeclNode(lambdaExpression, name, fullName, relativeFileName, code(lambdaExpression))
    scope.surroundingScopeFullName.foreach { fn =>
      lambdaTypeDecl.astParentFullName(fn).astParentType(NodeTypes.METHOD)
    }
    Ast.storeInDiffGraph(Ast(lambdaTypeDecl), diffGraph)
    // Create method ref
    val methodRef = methodRefNode(lambdaExpression, code(lambdaExpression), fullName, lambdaReturnType)
    Ast(methodRef) :: Nil
  }

  def astForAnonymousObjectCreationExpression(anonObjExpr: DotNetNodeInfo): Seq[Ast] = {
    val typeDeclName     = nextAnonymousTypeName()
    val typeDeclFullName = s"${scope.surroundingScopeFullName.getOrElse(Defines.Any)}.${typeDeclName}"

    val _typeDeclNode = typeDeclNode(
      anonObjExpr,
      typeDeclName,
      typeDeclFullName,
      relativeFileName,
      code(anonObjExpr),
      astParentType = NodeTypes.METHOD,
      astParentFullName = scope.surroundingScopeFullName.getOrElse(Defines.Any)
    )

    scope.pushNewScope(TypeScope(typeDeclFullName))

    val memberAsts = anonObjExpr
      .json(ParserKeys.Initializers)
      .arr
      .map(createDotNetNodeInfo)
      .map(astForAnonymousObjectMemberDeclarator)
      .toSeq

    scope.popScope()
    Ast.storeInDiffGraph(Ast(_typeDeclNode).withChildren(memberAsts), diffGraph)

    val _typeRefNode = typeRefNode(anonObjExpr, code(anonObjExpr), typeDeclFullName)
    Ast(_typeRefNode) :: Nil
  }

  private def astForAnonymousObjectMemberDeclarator(memberDeclarator: DotNetNodeInfo): Ast = {
    val rhsNode         = createDotNetNodeInfo(memberDeclarator.json(ParserKeys.Expression))
    val rhsAst          = astForNode(rhsNode)
    val rhsTypeFullName = getTypeFullNameFromAstNode(rhsAst)

    val lhsNode = Try(
      createDotNetNodeInfo(memberDeclarator.json(ParserKeys.NameEquals)(ParserKeys.Name))
    ).toOption match {
      case Some(lhs) => Option(lhs)
      case None      => None
    }

    val lhsAst = lhsNode match {
      case Some(node) => astForNode(node)
      case _          => Seq.empty[Ast]
    }

    val name = lhsNode match {
      case Some(node) => nameFromNode(node)
      case _          => nameFromNode(rhsNode)
    }

    val memberType = rhsTypeFullName match {
      case Defines.Any => getTypeFullNameFromAstNode(lhsAst)
      case otherType   => otherType
    }

    val _memberNode = memberNode(memberDeclarator, name, code(memberDeclarator), memberType)

    Ast(_memberNode)
  }

}
