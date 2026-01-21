package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.EcmaBuiltins
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.datastructures.VariableScopeManager
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{
  DispatchTypes,
  EdgeTypes,
  EvaluationStrategies,
  ModifierTypes,
  Operators,
  PropertyDefaults,
  PropertyNames
}
import ujson.Value

import scala.util.Try

trait AstForTypesCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def astForTypeAlias(alias: BabelNodeInfo): Ast = {
    val (aliasName, aliasFullName) = calcTypeNameAndFullName(alias)
    registerType(aliasFullName)

    val nameNodeInfo = if (hasKey(alias.json, "right")) {
      createBabelNodeInfo(alias.json("right"))
    } else {
      createBabelNodeInfo(alias.json)
    }
    val nameTpe = typeFor(nameNodeInfo)
    val name = if (nameTpe.contains("{") || nameTpe.contains("(")) {
      calcTypeNameAndFullName(nameNodeInfo)._1
    } else nameTpe

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val aliasTypeDeclNode =
      typeDeclNode(alias, aliasName, aliasFullName, parserResult.filename, alias.code, astParentType, astParentFullName)
    seenAliasTypes.add(aliasTypeDeclNode)

    if (!Defines.JsTypes.contains(name) && !seenAliasTypes.exists(_.name == name)) {
      val (typeName, typeFullName) = calcTypeNameAndFullName(alias, Option(name))
      registerType(typeFullName)

      val typeDeclNode_ = typeDeclNode(
        alias,
        typeName,
        typeFullName,
        parserResult.filename,
        alias.code,
        astParentType,
        astParentFullName,
        alias = Option(aliasFullName)
      )
      diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
    } else {
      seenAliasTypes.find(t => t.name == name).foreach(_.aliasTypeFullName(aliasFullName))
    }

    // adding all class methods / functions and uninitialized, non-static members
    (alias.node match {
      case TSTypeLiteral => classMembersForTypeAlias(alias)
      case ObjectPattern => Try(alias.json("properties").arr).toOption.toSeq.flatten
      case _             => classMembersForTypeAlias(createBabelNodeInfo(alias.json("typeAnnotation")))
    }).filter(member => isClassMethodOrUninitializedMemberOrObjectProperty(member) && !isStaticMember(member))
      .map(m => astForClassMember(m, aliasTypeDeclNode, ignoreInitCalls = true))
    Ast(aliasTypeDeclNode)
  }

  private def isConstructor(json: Value): Boolean = createBabelNodeInfo(json).node match {
    case TSConstructSignatureDeclaration => true
    case _                               => safeStr(json, "kind").contains("constructor")
  }

  private def classMembers(clazz: BabelNodeInfo, withConstructor: Boolean = true): Seq[Value] = {
    val allMembers                 = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    val constructor                = allMembers.find(isConstructor)
    val constructorBody            = constructor.flatMap(c => Try(c("body")("body").arr).toOption)
    val constructorParameters      = constructor.flatMap(c => Try(c("params").arr).toOption)
    val dynamicallyDeclaredMembers = constructorBody.toSeq.flatten.filter(isInitializedMember)
    val parameterProperties        = constructorParameters.toSeq.flatten.filter(isParameterProperty)
    if (withConstructor) {
      parameterProperties ++ allMembers ++ dynamicallyDeclaredMembers
    } else {
      parameterProperties ++ allMembers.diff(constructor.toSeq) ++ dynamicallyDeclaredMembers
    }
  }

  private def classMembersForTypeAlias(alias: BabelNodeInfo): Seq[Value] =
    Try(alias.json("members").arr).toOption.toSeq.flatten

  private def createFakeConstructor(
    code: String,
    forElem: BabelNodeInfo,
    methodBlockContent: ConstructorContent
  ): MethodAst = {
    val fakeStartEnd =
      s"""
         | "start": ${start(forElem.json).getOrElse(-1)},
         | "end": ${end(forElem.json).getOrElse(-1)}
         |""".stripMargin

    val fakeConstructorCode = s"""{
      | "type": "ClassMethod",
      | $fakeStartEnd,
      | "key": {
      |   "type": "Identifier",
      |   "name": "constructor",
      |   $fakeStartEnd
      | },
      | "kind": "constructor",
      | "id": null,
      | "params": [],
      | "body": {
      |   "type": "BlockStatement",
      |   "body": []
      | }
      |}""".stripMargin
    val result =
      createMethodAstAndNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)), false, false, methodBlockContent)
    result.methodNode.code(code)
    result
  }

  private def findClassConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(isConstructor)

  protected case class ConstructorContent(
    clazz: Option[BabelNodeInfo],
    constructorContent: List[Value],
    typeDecl: Option[NewTypeDecl]
  )
  protected object ConstructorContent {
    def empty: ConstructorContent = ConstructorContent(None, List.empty, None)
  }

  private def createClassConstructor(classExpr: BabelNodeInfo, constructorContent: ConstructorContent): MethodAst =
    findClassConstructor(classExpr) match {
      case Some(classConstructor) if hasKey(classConstructor, "body") =>
        val result =
          createMethodAstAndNode(createBabelNodeInfo(classConstructor), false, false, constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
      case Some(classConstructor) =>
        val methodNode =
          createMethodDefinitionNode(createBabelNodeInfo(classConstructor), constructorContent)
        diffGraph.addEdge(methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        MethodAst(Ast(methodNode), methodNode, Ast(methodNode))
      case _ =>
        val result = createFakeConstructor("constructor() {}", classExpr, constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
    }

  private def interfaceConstructor(typeName: String, tsInterface: BabelNodeInfo): NewMethod =
    findClassConstructor(tsInterface) match {
      case Some(interfaceConstructor) =>
        createMethodDefinitionNode(createBabelNodeInfo(interfaceConstructor), ConstructorContent.empty)
      case _ => createFakeConstructor(s"new: $typeName", tsInterface, ConstructorContent.empty).methodNode
    }

  private def astsForEnumMember(tsEnumMember: BabelNodeInfo): Seq[Ast] = {
    val name          = code(tsEnumMember.json("id"))
    val tpe           = typeFor(tsEnumMember)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    val memberNode_   = memberNode(tsEnumMember, name, tsEnumMember.code, typeFullName).possibleTypes(possibleTypes)
    addModifier(memberNode_, tsEnumMember.json)

    if (hasKey(tsEnumMember.json, "initializer")) {
      val lhsAst = astForNode(tsEnumMember.json("id"))
      val rhsAst = astForNodeWithFunctionReference(tsEnumMember.json("initializer"))
      val callNode_ =
        callNode(tsEnumMember, tsEnumMember.code, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
      val argAsts = List(lhsAst, rhsAst)
      Seq(callAst(callNode_, argAsts), Ast(memberNode_))
    } else {
      Seq(Ast(memberNode_))
    }
  }

  protected def astForClassMember(
    classElement: Value,
    typeDeclNode: NewTypeDecl,
    ignoreInitCalls: Boolean = false
  ): Ast = {
    val nodeInfo      = createBabelNodeInfo(classElement)
    val tpe           = typeFor(nodeInfo)
    val possibleTypes = Seq(tpe)
    val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
    val memberNode_ = nodeInfo.node match {
      case TSDeclareMethod | TSDeclareFunction =>
        val function = createMethodDefinitionNode(nodeInfo, ConstructorContent.empty)
        addModifier(function, nodeInfo.json)
        memberNode(nodeInfo, function.name, nodeInfo.code, typeFullName, Seq(function.fullName))
          .possibleTypes(possibleTypes)
      case ClassMethod | ClassPrivateMethod =>
        val function = createMethodAstAndNode(nodeInfo, false, false, ConstructorContent.empty).methodNode
        addModifier(function, nodeInfo.json)
        memberNode(nodeInfo, function.name, nodeInfo.code, typeFullName, Seq(function.fullName))
          .possibleTypes(possibleTypes)
      case ExpressionStatement if isInitializedMember(classElement) =>
        val memberNodeInfo = createBabelNodeInfo(nodeInfo.json("expression")("left")("property"))
        val name           = memberNodeInfo.code
        memberNode(nodeInfo, name, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
      case TSPropertySignature | ObjectProperty if hasKey(nodeInfo.json("key"), "name") =>
        val memberNodeInfo = createBabelNodeInfo(nodeInfo.json("key"))
        val name           = memberNodeInfo.json("name").str
        val memberNode_    = memberNode(nodeInfo, name, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
        astsForDecorators(nodeInfo).foreach { decoratorAst =>
          Ast.storeInDiffGraph(decoratorAst, diffGraph)
          decoratorAst.root.foreach(diffGraph.addEdge(memberNode_, _, EdgeTypes.AST))
        }
        memberNode_
      case _ =>
        val name = nodeInfo.node match {
          case ClassProperty        => code(nodeInfo.json("key"))
          case ClassPrivateProperty => code(nodeInfo.json("key")("id"))
          case TSParameterProperty =>
            val unpackedParam = createBabelNodeInfo(nodeInfo.json("parameter"))
            unpackedParam.node match {
              case AssignmentPattern => createBabelNodeInfo(unpackedParam.json("left")).code
              case _                 => unpackedParam.json("name").str
            }
          // TODO: name field most likely needs adjustment for other Babel AST types
          case _ => nodeInfo.code
        }
        val memberNode_ = memberNode(nodeInfo, name, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
        astsForDecorators(nodeInfo).foreach { decoratorAst =>
          Ast.storeInDiffGraph(decoratorAst, diffGraph)
          decoratorAst.root.foreach(diffGraph.addEdge(memberNode_, _, EdgeTypes.AST))
        }
        memberNode_
    }

    addModifier(memberNode_, classElement)
    diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)

    if (!ignoreInitCalls && hasKey(nodeInfo.json, "value") && !nodeInfo.json("value").isNull) {
      val lhsAst = astForNode(nodeInfo.json("key"))
      val rhsAst = astForNodeWithFunctionReference(nodeInfo.json("value"))
      val callNode_ =
        callNode(nodeInfo, nodeInfo.code, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
      val argAsts = List(lhsAst, rhsAst)
      return callAst(callNode_, argAsts)
    }

    if (!ignoreInitCalls && nodeInfo.node == TSParameterProperty) {
      val name       = memberNode_.name
      val memberNode = fieldIdentifierNode(nodeInfo, name, name)
      val thisNode   = identifierNode(nodeInfo, "this")
      scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val fieldAccessAst = createFieldAccessCallAst(thisNode, memberNode, nodeInfo.lineNumber, nodeInfo.columnNumber)
      val rhsAst         = Ast(identifierNode(nodeInfo, name, name, tpe).possibleTypes(possibleTypes))
      val callNode_ = callNode(nodeInfo, s"this.$name = $name", Operators.assignment, DispatchTypes.STATIC_DISPATCH)
      val argAsts   = List(fieldAccessAst, rhsAst)
      return callAst(callNode_, argAsts)
    }

    Ast()
  }

  protected def astForEnum(tsEnum: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsEnum)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val typeDeclNode_ = typeDeclNode(
      tsEnum,
      typeName,
      typeFullName,
      parserResult.filename,
      s"enum $typeName",
      astParentType,
      astParentFullName
    )
    seenAliasTypes.add(typeDeclNode_)

    addModifier(typeDeclNode_, tsEnum.json)

    val typeRefNode_ = typeRefNode(tsEnum, s"enum $typeName", typeFullName)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)
    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val enumMembers = tsEnum.json("members").arr.toList
    val (enumMembersWithInitializer, enumMembersPlain) =
      enumMembers.partition(m => hasKey(m, "initializer") && !m("initializer").isNull)

    val memberAsts = enumMembersPlain.flatMap(m => astsForEnumMember(createBabelNodeInfo(m)))

    if (enumMembersWithInitializer.isEmpty) {
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(memberAsts), diffGraph)
    } else {
      val init = staticEnumInitMethodAst(
        tsEnum,
        typeDeclNode_,
        enumMembersWithInitializer,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        Defines.Any
      )
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(memberAsts).withChild(init), diffGraph)
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)
    Ast(typeRefNode_)
  }

  private def isStaticMember(json: Value): Boolean = {
    val nodeInfo = createBabelNodeInfo(json).node
    val isStatic = safeBool(json, "static").contains(true)
    nodeInfo != ClassMethod && nodeInfo != ClassPrivateMethod && isStatic
  }

  private def isInitializedMember(json: Value): Boolean = {
    val hasInitializedValue = hasKey(json, "value") && !json("value").isNull
    val isAssignment = createBabelNodeInfo(json) match {
      case node if node.node == ExpressionStatement =>
        val exprNode = createBabelNodeInfo(node.json("expression"))
        exprNode.node == AssignmentExpression &&
        createBabelNodeInfo(exprNode.json("left")).node == MemberExpression &&
        code(exprNode.json("left")("object")) == "this"
      case _ => false
    }
    hasInitializedValue || isAssignment
  }

  private def isParameterProperty(json: Value): Boolean = {
    createBabelNodeInfo(json).node == TSParameterProperty
  }

  private def isStaticInitBlock(json: Value): Boolean = createBabelNodeInfo(json).node == StaticBlock

  private def isClassMethodOrUninitializedMember(json: Value): Boolean = {
    val nodeInfo = createBabelNodeInfo(json).node
    !isStaticInitBlock(json) &&
    (nodeInfo == ClassMethod || nodeInfo == ClassPrivateMethod || !isInitializedMember(json))
  }

  private def isClassMethodOrUninitializedMemberOrObjectProperty(json: Value): Boolean = {
    val nodeInfo = createBabelNodeInfo(json).node
    !isStaticInitBlock(json) &&
    (nodeInfo == ObjectProperty || nodeInfo == ClassMethod || nodeInfo == ClassPrivateMethod || !isInitializedMember(
      json
    ))
  }

  private def staticClassInitMethodAst(
    node: BabelNodeInfo,
    typeDeclNode: NewTypeDecl,
    staticMemberInits: List[Value],
    staticMemberBlocks: List[Value],
    fullName: String,
    returnType: String
  ): Ast = {
    val modifiers =
      List(NewModifier().modifierType(ModifierTypes.STATIC), NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))
    val blockNode   = NewBlock().typeFullName(Defines.Any)
    val methodNode_ = methodNode(node, io.joern.x2cpg.Defines.StaticInitMethodName, fullName, "", parserResult.filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(
      fullName,
      io.joern.x2cpg.Defines.StaticInitMethodName,
      blockNode,
      typeRefIdStack.headOption
    )
    localAstParentStack.push(blockNode)

    val initAsts = staticMemberInits.map(astForClassMember(_, typeDeclNode))
      ++ staticMemberBlocks.map(astForNodeWithFunctionReference)
    val body = blockAst(NewBlock().typeFullName(Defines.Any), initAsts)

    methodAstParentStack.pop()
    localAstParentStack.pop()
    scope.popScope()

    val methodReturn = methodReturnNode(node, returnType)
    methodAst(methodNode_, Nil, body, methodReturn, modifiers)
  }

  private def staticEnumInitMethodAst(
    node: BabelNodeInfo,
    typeDeclNode: NewTypeDecl,
    enumMembersWithInitializer: List[Value],
    fullName: String,
    returnType: String
  ): Ast = {
    val modifiers =
      List(NewModifier().modifierType(ModifierTypes.STATIC), NewModifier().modifierType(ModifierTypes.CONSTRUCTOR))
    val blockNode   = NewBlock().typeFullName(Defines.Any)
    val methodNode_ = methodNode(node, io.joern.x2cpg.Defines.StaticInitMethodName, fullName, "", parserResult.filename)

    methodAstParentStack.push(methodNode_)
    scope.pushNewMethodScope(
      fullName,
      io.joern.x2cpg.Defines.StaticInitMethodName,
      blockNode,
      typeRefIdStack.headOption
    )
    localAstParentStack.push(blockNode)

    val initAsts         = enumMembersWithInitializer.flatMap(m => astsForEnumMember(createBabelNodeInfo(m)))
    val (calls, members) = initAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))

    members.foreach(m => diffGraph.addEdge(typeDeclNode, m.root.get, EdgeTypes.AST))
    val body = blockAst(NewBlock().typeFullName(Defines.Any), calls)

    methodAstParentStack.pop()
    localAstParentStack.pop()
    scope.popScope()

    val methodReturn = methodReturnNode(node, returnType)
    methodAst(methodNode_, Nil, body, methodReturn, modifiers)
  }

  protected def astForClass(clazz: BabelNodeInfo, shouldCreateAssignmentCall: Boolean = false): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val superClass = Try(createBabelNodeInfo(clazz.json("superClass")).code).toOption.toSeq
    val implements = Try(clazz.json("implements").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten
    val mixins     = Try(clazz.json("mixins").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten

    val typeDeclNode_ = typeDeclNode(
      clazz,
      typeName,
      typeFullName,
      parserResult.filename,
      s"class $typeName",
      astParentType,
      astParentFullName,
      inherits = superClass ++ implements ++ mixins
    )
    seenAliasTypes.add(typeDeclNode_)

    addModifier(typeDeclNode_, clazz.json)
    astsForDecorators(clazz).foreach { decoratorAst =>
      Ast.storeInDiffGraph(decoratorAst, diffGraph)
      decoratorAst.root.foreach(diffGraph.addEdge(typeDeclNode_, _, EdgeTypes.AST))
    }

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode_, EdgeTypes.AST)

    val typeRefNode_ = typeRefNode(clazz, s"class $typeName", typeFullName)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode_)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val allClassMembers = classMembers(clazz, withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInitCalls =
      allClassMembers.filter(m => !isStaticMember(m) && (isInitializedMember(m) || isParameterProperty(m)))

    val constructor =
      createClassConstructor(clazz, ConstructorContent(Some(clazz), memberInitCalls, Some(typeDeclNode_)))
    val constructorNode = constructor.methodNode

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(m => isClassMethodOrUninitializedMember(m) && !isStaticMember(m) && !isParameterProperty(m))
      .map(m => astForClassMember(m, typeDeclNode_))

    // adding all static members and retrieving their initialization calls
    val staticMemberInits = allClassMembers.filter(isStaticMember)

    // retrieving initialization calls from the static initialization block if any
    val staticInitBlock  = allClassMembers.find(isStaticInitBlock)
    val staticInitBlocks = staticInitBlock.map(block => block("body").arr.toList).getOrElse(List.empty)

    if (staticMemberInits.nonEmpty || staticInitBlocks.nonEmpty) {
      val init = staticClassInitMethodAst(
        clazz,
        typeDeclNode_,
        staticMemberInits,
        staticInitBlocks,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        Defines.Any
      )
      Ast.storeInDiffGraph(init, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.nodes.head, EdgeTypes.AST)
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    if (shouldCreateAssignmentCall) {
      diffGraph.addEdge(localAstParentStack.head, typeRefNode_, EdgeTypes.AST)

      // return a synthetic assignment to enable tracing of the implicitly created identifier for
      // the class definition assigned to its constructor
      val classIdNode = identifierNode(clazz, typeName, typeName, Defines.Any, Seq(constructorNode.fullName))
      val constructorRefNode =
        methodRefNode(clazz, constructorNode.code, constructorNode.fullName, constructorNode.fullName)

      val idLocal = localNode(clazz, typeName, typeName, Defines.Any).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(typeName, idLocal, Defines.Any, VariableScopeManager.ScopeType.BlockScope)
      scope.addVariableReference(typeName, classIdNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

      val assignmentAst = createAssignmentCallAst(
        classIdNode,
        constructorRefNode,
        s"$typeName = ${constructorNode.fullName}",
        clazz.lineNumber,
        clazz.columnNumber
      )

      val classDecorationAst     = createClassDecorationAst(clazz, classIdNode)
      val propertyDecorationAsts = createPropertyDecorationAsts(clazz, classIdNode)
      val methodDecorationAsts   = createMethodDecorationAsts(clazz, classIdNode)
      if (classDecorationAst.root.isDefined || propertyDecorationAsts.nonEmpty || methodDecorationAsts.nonEmpty) {
        val blockNode_   = blockNode(clazz)
        val childrenAsts = List(assignmentAst, classDecorationAst) ++ propertyDecorationAsts ++ methodDecorationAsts
        setArgumentIndices(childrenAsts)
        Ast(blockNode_).withChildren(childrenAsts)
      } else assignmentAst
    } else {
      Ast(typeRefNode_)
    }
  }

  private def createClassDecorationAst(classNodeInfo: BabelNodeInfo, classIdNode: NewIdentifier): Ast = {
    val decoratorAsts = decoratorExpressionElements(classNodeInfo).map(e => astForNodeWithFunctionReference(e.json))
    if (decoratorAsts.nonEmpty) {
      val lhsAst = Ast(
        identifierNode(
          classNodeInfo,
          classIdNode.name,
          classIdNode.code,
          Defines.Any,
          classIdNode.dynamicTypeHintFullName
        )
      )

      val receiverNode = identifierNode(classNodeInfo, "__decorate")
      scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
      val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
      scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

      val decorateCallNode =
        callNode(
          classNodeInfo,
          s"__decorate([${decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")}], ${classIdNode.name})",
          "__decorate",
          DispatchTypes.DYNAMIC_DISPATCH
        )

      val classRefNode = Ast(
        identifierNode(
          classNodeInfo,
          classIdNode.name,
          classIdNode.code,
          Defines.Any,
          classIdNode.dynamicTypeHintFullName
        )
      )
      val annotationExprAst = astForDecorateArray(classNodeInfo, decoratorAsts)
      val args              = Seq(annotationExprAst, classRefNode)
      val decorateCallAst =
        callAst(decorateCallNode, args, receiver = Option(Ast(receiverNode)), base = Option(Ast(thisNode)))

      createAssignmentCallAst(
        lhsAst,
        decorateCallAst,
        s"${classIdNode.name} = ${codeOf(decorateCallAst.nodes.head)}",
        classNodeInfo.lineNumber,
        classNodeInfo.columnNumber
      )
    } else Ast()
  }

  private def createMetadataCallTypeAst(tsMethodNodeInfo: BabelNodeInfo): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallRecThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallRecThisNode.name,
      metadataCallRecThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:type'", Defines.String)
    val functionLiteralNode   = literalNode(tsMethodNodeInfo, "Function", Defines.Any)
    val metadataCallType =
      callNode(
        tsMethodNodeInfo,
        s"""__metadata("design:type", Function)""",
        "__metadata",
        DispatchTypes.DYNAMIC_DISPATCH
      )
    val metadataTypeArgAsts = Seq(Ast(designTypeLiteralNode), Ast(functionLiteralNode))
    callAst(
      metadataCallType,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallRecThisNode))
    )
  }

  private def createMetadataCallParamTypesAst(tsMethodNodeInfo: BabelNodeInfo, numParams: Int): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallThisNode.name,
      metadataCallThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designParamTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:paramtypes'", Defines.String)

    val objectLiteralNode = literalNode(tsMethodNodeInfo, "Object", Defines.Object)

    val blockNode_ = blockNode(tsMethodNodeInfo)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(tsMethodNodeInfo, tmpName, tmpName, Defines.Any).order(0)
    val tmpArrayNode = identifierNode(tsMethodNodeInfo, tmpName)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
    scope.addVariableReference(tmpName, tmpArrayNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val arrayCallNode =
      callNode(
        tsMethodNodeInfo,
        s"${EcmaBuiltins.arrayFactory}()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH
      )

    val lineNumber     = tsMethodNodeInfo.lineNumber
    val columnNumber   = tsMethodNodeInfo.columnNumber
    val assignmentCode = s"${localTmpNode.code} = ${arrayCallNode.code}"
    val assignmentTmpArrayCallNode =
      createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

    val elementAsts = (0 until numParams).toList.map { _ =>
      val objectLiteralNode = literalNode(tsMethodNodeInfo, "Object", Defines.Object)
      val pushCallNode      = callNode(tsMethodNodeInfo, s"$tmpName.push(Object)", "", DispatchTypes.DYNAMIC_DISPATCH)

      val baseNode   = identifierNode(tsMethodNodeInfo, tmpName)
      val memberNode = fieldIdentifierNode(tsMethodNodeInfo, "push", "push")
      val receiverNode =
        createFieldAccessCallAst(baseNode, memberNode, tsMethodNodeInfo.lineNumber, tsMethodNodeInfo.columnNumber)
      val thisPushNode = identifierNode(tsMethodNodeInfo, tmpName)
      callAst(
        pushCallNode,
        List(Ast(objectLiteralNode)),
        receiver = Option(receiverNode),
        base = Option(Ast(thisPushNode))
      )
    }

    val tmpArrayReturnNode = identifierNode(tsMethodNodeInfo, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val blockChildrenAsts = assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode)
    setArgumentIndices(blockChildrenAsts)
    val blockAst_ = blockAst(blockNode_, blockChildrenAsts)

    val metadataParamTypeCall =
      callNode(
        tsMethodNodeInfo,
        s"""__metadata("design:paramtypes", [${List.fill(numParams)("Object").mkString(",")}])""",
        "__metadata",
        DispatchTypes.DYNAMIC_DISPATCH
      )
    val metadataTypeArgAsts = Seq(Ast(designParamTypeLiteralNode), blockAst_)
    callAst(
      metadataParamTypeCall,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallThisNode))
    )
  }

  private def createMetadataCallReturnTypeAst(tsMethodNodeInfo: BabelNodeInfo, tpe: String): Ast = {
    val metadataCallRecNode = identifierNode(tsMethodNodeInfo, "__metadata")
    scope.addVariableReference(
      metadataCallRecNode.name,
      metadataCallRecNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )
    val metadataCallTypeRecThisNode =
      identifierNode(tsMethodNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
    scope.addVariableReference(
      metadataCallTypeRecThisNode.name,
      metadataCallTypeRecThisNode,
      Defines.Any,
      EvaluationStrategies.BY_REFERENCE
    )

    val designTypeLiteralNode = literalNode(tsMethodNodeInfo, "'design:returntype'", Defines.String)
    val tpeLiteralNode        = literalNode(tsMethodNodeInfo, tpe, Defines.Any)
    val metadataCallType =
      callNode(tsMethodNodeInfo, s"""__metadata("design:type", $tpe)""", "__metadata", DispatchTypes.DYNAMIC_DISPATCH)
    val metadataTypeArgAsts = Seq(Ast(designTypeLiteralNode), Ast(tpeLiteralNode))
    callAst(
      metadataCallType,
      metadataTypeArgAsts,
      receiver = Option(Ast(metadataCallRecNode)),
      base = Option(Ast(metadataCallTypeRecThisNode))
    )
  }

  private def createPropertyDecorationAsts(classNodeInfo: BabelNodeInfo, classIdNode: NewIdentifier): Seq[Ast] = {
    val tsProperties = classMembers(classNodeInfo).flatMap { member =>
      val memberNodeInfo = createBabelNodeInfo(member)
      if (
        memberNodeInfo.node == ClassProperty ||
        memberNodeInfo.node == ClassPrivateProperty
      ) {
        Some(memberNodeInfo)
      } else {
        None
      }
    }

    tsProperties.map { tsPropertyNodeInfo =>
      val decoratorAsts =
        decoratorExpressionElements(tsPropertyNodeInfo).map(e => astForNodeWithFunctionReference(e.json))
      if (decoratorAsts.nonEmpty) {
        val name = tsPropertyNodeInfo.node match {
          case ClassProperty        => code(tsPropertyNodeInfo.json("key"))
          case ClassPrivateProperty => code(tsPropertyNodeInfo.json("key")("id"))
          case _                    => tsPropertyNodeInfo.code
        }
        val propertyTpe = typeFor(tsPropertyNodeInfo)

        val receiverNode = identifierNode(classNodeInfo, "__decorate")
        scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
        scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

        val classPrototypeAccessAst = createFieldAccessCallAst(
          identifierNode(classNodeInfo, classIdNode.name),
          fieldIdentifierNode(classNodeInfo, "prototype", "prototype"),
          classNodeInfo.lineNumber,
          classNodeInfo.columnNumber
        )
        val propertyNameNode = literalNode(tsPropertyNodeInfo, s"'$name'", Defines.String)
        val voidCallNode_    = voidCallNode(line(tsPropertyNodeInfo), column(tsPropertyNodeInfo))

        val arg1Code = decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val decorateCallCode =
          s"__decorate([$arg1Code], ${codeOf(classPrototypeAccessAst.nodes.head)}, ${propertyNameNode.code}, ${voidCallNode_.code})"

        val decorateCallNode = callNode(classNodeInfo, decorateCallCode, "__decorate", DispatchTypes.DYNAMIC_DISPATCH)
        val subAst           = astForDecorateArray(classNodeInfo, decoratorAsts)
        callAst(
          decorateCallNode,
          subAst +: Seq(classPrototypeAccessAst, Ast(propertyNameNode), Ast(voidCallNode_)),
          receiver = Option(Ast(receiverNode)),
          base = Option(Ast(thisNode))
        )
      } else {
        Ast()
      }
    }
  }

  private def createMethodDecorationAsts(classNodeInfo: BabelNodeInfo, classIdNode: NewIdentifier): Seq[Ast] = {
    val tsMethods = classMembers(classNodeInfo).flatMap { member =>
      val memberNodeInfo = createBabelNodeInfo(member)
      if (memberNodeInfo.node == ClassMethod) {
        Some(memberNodeInfo)
      } else {
        None
      }
    }

    tsMethods.map { tsMethodNodeInfo =>
      val decoratorAsts =
        decoratorExpressionElements(tsMethodNodeInfo).map(e => astForNodeWithFunctionReference(e.json))
      val (name, fullName) = calcMethodNameAndFullName(tsMethodNodeInfo)
      val methodTpe        = typeFor(tsMethodNodeInfo).stripPrefix("__ecma.")
      val paramNodeInfos = if (hasKey(tsMethodNodeInfo.json, "parameters")) {
        tsMethodNodeInfo.json("parameters").arr.toSeq
      } else {
        tsMethodNodeInfo.json("params").arr.toSeq
      }
      val paramDecoratorAsts = paramNodeInfos.zipWithIndex.flatMap { case (value, idx) =>
        val paramAsts =
          decoratorExpressionElements(createBabelNodeInfo(value)).map(e => astForNodeWithFunctionReference(e.json))

        if (paramAsts.isEmpty) {
          Seq.empty
        } else {
          val paramNodeInfo = createBabelNodeInfo(value)
          val receiverNode  = identifierNode(paramNodeInfo, "__param")
          scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
          val thisNode =
            identifierNode(paramNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
          scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

          paramAsts.map { p =>
            val callNode_ = callNode(
              paramNodeInfo,
              s"__param($idx, ${codeOf(p.nodes.head)})",
              "__param",
              DispatchTypes.DYNAMIC_DISPATCH
            )
            val idxNode = literalNode(paramNodeInfo, s"$idx", Defines.Number)
            val argAsts = Seq(Ast(idxNode), p)
            val paramDecorateCallAst =
              callAst(callNode_, argAsts, receiver = Option(Ast(receiverNode)), base = Option(Ast(thisNode)))
            paramDecorateCallAst
          }
        }
      }

      if (decoratorAsts.nonEmpty || paramDecoratorAsts.nonEmpty) {
        val receiverNode = identifierNode(classNodeInfo, "__decorate")
        scope.addVariableReference(receiverNode.name, receiverNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)
        val thisNode = identifierNode(classNodeInfo, "this").dynamicTypeHintFullName(rootTypeDecl.map(_.fullName).toSeq)
        scope.addVariableReference(thisNode.name, thisNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

        val classPrototypeAccessAst = createFieldAccessCallAst(
          identifierNode(classNodeInfo, classIdNode.name),
          fieldIdentifierNode(classNodeInfo, "prototype", "prototype"),
          classNodeInfo.lineNumber,
          classNodeInfo.columnNumber
        )
        val functionNameNode          = literalNode(tsMethodNodeInfo, s"'$name'", Defines.String)
        val nullNode                  = literalNode(tsMethodNodeInfo, "null", Defines.Null)
        val metadataCallTypeAst       = createMetadataCallTypeAst(tsMethodNodeInfo)
        val metadataCallParamTypesAst = createMetadataCallParamTypesAst(tsMethodNodeInfo, paramNodeInfos.size)
        val metadataCallReturnTypeAst = createMetadataCallReturnTypeAst(tsMethodNodeInfo, methodTpe)

        val arg1Code = decoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val arg2Code = paramDecoratorAsts.flatMap(_.root.map(codeOf)).mkString(",")
        val arg3Code = codeOf(metadataCallTypeAst.root.get)
        val arg4Code = codeOf(metadataCallParamTypesAst.root.get)
        val arg5Code = codeOf(metadataCallReturnTypeAst.root.get)
        val decorateCallCode =
          s"__decorate([$arg1Code, $arg2Code, $arg3Code, $arg4Code, $arg5Code], ${codeOf(classPrototypeAccessAst.nodes.head)}, ${functionNameNode.code}, null)"

        val decorateCallNode = callNode(classNodeInfo, decorateCallCode, "__decorate", DispatchTypes.DYNAMIC_DISPATCH)
        val subAst = astForDecorateArray(
          classNodeInfo,
          decoratorAsts ++ paramDecoratorAsts ++ Seq(
            metadataCallTypeAst,
            metadataCallParamTypesAst,
            metadataCallReturnTypeAst
          )
        )
        callAst(
          decorateCallNode,
          subAst +: Seq(classPrototypeAccessAst, Ast(functionNameNode), Ast(nullNode)),
          receiver = Option(Ast(receiverNode)),
          base = Option(Ast(thisNode))
        )
      } else {
        Ast()
      }
    }
  }

  private def astForDecorateArray(classNodeInfo: BabelNodeInfo, decoratorAsts: List[Ast]): Ast = {
    val blockNode_ = blockNode(classNodeInfo)
    scope.pushNewBlockScope(blockNode_)
    localAstParentStack.push(blockNode_)

    val tmpName      = generateUnusedVariableName(usedVariableNames, "_tmp")
    val localTmpNode = localNode(classNodeInfo, tmpName, tmpName, Defines.Any).order(0)
    val tmpArrayNode = identifierNode(classNodeInfo, tmpName)
    diffGraph.addEdge(localAstParentStack.head, localTmpNode, EdgeTypes.AST)
    scope.addVariableReference(tmpName, tmpArrayNode, Defines.Any, EvaluationStrategies.BY_REFERENCE)

    val arrayCallNode =
      callNode(
        classNodeInfo,
        s"${EcmaBuiltins.arrayFactory}()",
        EcmaBuiltins.arrayFactory,
        DispatchTypes.STATIC_DISPATCH
      )

    val lineNumber     = classNodeInfo.lineNumber
    val columnNumber   = classNodeInfo.columnNumber
    val assignmentCode = s"${localTmpNode.code} = ${arrayCallNode.code}"
    val assignmentTmpArrayCallNode =
      createAssignmentCallAst(tmpArrayNode, arrayCallNode, assignmentCode, lineNumber, columnNumber)

    val elementAsts = decoratorAsts.map { ast =>
      val elementCode  = ast.root.map(codeOf).getOrElse(PropertyDefaults.Code)
      val pushCallNode = callNode(classNodeInfo, s"$tmpName.push($elementCode)", "", DispatchTypes.DYNAMIC_DISPATCH)

      val baseNode   = identifierNode(classNodeInfo, tmpName)
      val memberNode = fieldIdentifierNode(classNodeInfo, "push", "push")
      val receiverNode =
        createFieldAccessCallAst(baseNode, memberNode, classNodeInfo.lineNumber, classNodeInfo.columnNumber)
      val thisPushNode = identifierNode(classNodeInfo, tmpName)
      callAst(pushCallNode, List(ast), receiver = Option(receiverNode), base = Option(Ast(thisPushNode)))
    }

    val tmpArrayReturnNode = identifierNode(classNodeInfo, tmpName)

    scope.popScope()
    localAstParentStack.pop()

    val blockChildrenAsts = assignmentTmpArrayCallNode +: elementAsts :+ Ast(tmpArrayReturnNode)
    setArgumentIndices(blockChildrenAsts)
    blockAst(blockNode_, blockChildrenAsts)
  }

  protected def addModifier(node: NewNode, json: Value): Unit = createBabelNodeInfo(json).node match {
    case ClassPrivateProperty =>
      diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PRIVATE), EdgeTypes.AST)
    case _ =>
      if (safeBool(json, "abstract").contains(true))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.ABSTRACT), EdgeTypes.AST)
      if (safeBool(json, "static").contains(true))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.STATIC), EdgeTypes.AST)
      if (safeBool(json, "readonly").contains(true))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.READONLY), EdgeTypes.AST)
      if (safeStr(json, "accessibility").contains("public"))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PUBLIC), EdgeTypes.AST)
      if (safeStr(json, "accessibility").contains("private"))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PRIVATE), EdgeTypes.AST)
      if (safeStr(json, "accessibility").contains("protected"))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PROTECTED), EdgeTypes.AST)
  }

  protected def astForModule(tsModuleDecl: BabelNodeInfo): Ast = {
    val (name, fullName) = calcTypeNameAndFullName(tsModuleDecl)
    registerType(fullName)

    val namespaceNode = NewNamespaceBlock()
      .code(tsModuleDecl.code)
      .lineNumber(tsModuleDecl.lineNumber)
      .columnNumber(tsModuleDecl.columnNumber)
      .filename(parserResult.filename)
      .name(name)
      .fullName(fullName)

    methodAstParentStack.push(namespaceNode)
    dynamicInstanceTypeStack.push(fullName)

    scope.pushNewMethodScope(fullName, name, namespaceNode, None)

    val blockAst = if (hasKey(tsModuleDecl.json, "body")) {
      val nodeInfo = createBabelNodeInfo(tsModuleDecl.json("body"))
      nodeInfo.node match {
        case TSModuleDeclaration => astForModule(nodeInfo)
        case _                   => astForBlockStatement(nodeInfo)
      }
    } else {
      Ast()
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()

    Ast(namespaceNode).withChild(blockAst)
  }

  protected def astForInterface(tsInterface: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsInterface)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FullName).toString

    val extendz = Try(tsInterface.json("extends").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten

    val typeDeclNode_ = typeDeclNode(
      tsInterface,
      typeName,
      typeFullName,
      parserResult.filename,
      s"interface $typeName",
      astParentType,
      astParentFullName,
      inherits = extendz
    )
    seenAliasTypes.add(typeDeclNode_)

    addModifier(typeDeclNode_, tsInterface.json)

    methodAstParentStack.push(typeDeclNode_)
    dynamicInstanceTypeStack.push(typeFullName)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode_, None)

    val constructorNode = interfaceConstructor(typeName, tsInterface)
    diffGraph.addEdge(constructorNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)

    val memberNode_ =
      memberNode(tsInterface, constructorNode.name, constructorNode.code, typeFullName, Seq(constructorNode.fullName))
    diffGraph.addEdge(typeDeclNode_, memberNode_, EdgeTypes.AST)

    val interfaceBodyElements = classMembers(tsInterface, withConstructor = false)

    interfaceBodyElements.foreach { classElement =>
      val nodeInfo      = createBabelNodeInfo(classElement)
      val tpe           = typeFor(nodeInfo)
      val possibleTypes = Seq(tpe)
      val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
      val memberNodes = nodeInfo.node match {
        case TSCallSignatureDeclaration | TSMethodSignature =>
          val functionNode = createMethodDefinitionNode(nodeInfo, ConstructorContent.empty)
          addModifier(functionNode, nodeInfo.json)
          Seq(
            memberNode(nodeInfo, functionNode.name, nodeInfo.code, typeFullName, Seq(functionNode.fullName))
              .possibleTypes(possibleTypes)
          )
        case _ =>
          val names = nodeInfo.node match {
            case TSPropertySignature | TSMethodSignature =>
              if (hasKey(nodeInfo.json("key"), "value")) {
                Seq(safeStr(nodeInfo.json("key"), "value").getOrElse(code(nodeInfo.json("key")("value"))))
              } else Seq(code(nodeInfo.json("key")))
            case TSIndexSignature =>
              nodeInfo.json("parameters").arr.toSeq.map(_("name").str)
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => Seq(nodeInfo.code)
          }
          names.map { n =>
            val node = memberNode(nodeInfo, n, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
            addModifier(node, nodeInfo.json)
            node
          }
      }
      memberNodes.foreach(diffGraph.addEdge(typeDeclNode_, _, EdgeTypes.AST))
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()

    Ast(typeDeclNode_)
  }

}
