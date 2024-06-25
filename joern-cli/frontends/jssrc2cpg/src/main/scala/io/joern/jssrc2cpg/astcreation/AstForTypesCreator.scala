package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.datastructures.BlockScope
import io.joern.jssrc2cpg.parser.BabelAst.*
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.{Ast, ValidationMode}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines
import io.joern.x2cpg.utils.NodeBuilders.newBindingNode
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, EdgeTypes, ModifierTypes, Operators}
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
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

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
    val allMembers = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    val dynamicallyDeclaredMembers =
      allMembers
        .find(isConstructor)
        .flatMap(c => Try(c("body")("body").arr).toOption)
        .toSeq
        .flatten
        .filter(isInitializedMember)
    if (withConstructor) {
      allMembers ++ dynamicallyDeclaredMembers
    } else {
      allMembers.filterNot(isConstructor) ++ dynamicallyDeclaredMembers
    }
  }

  private def classMembersForTypeAlias(alias: BabelNodeInfo): Seq[Value] =
    Try(alias.json("members").arr).toOption.toSeq.flatten

  private def createFakeConstructor(
    code: String,
    forElem: BabelNodeInfo,
    methodBlockContent: List[Ast] = List.empty
  ): MethodAst = {
    val fakeConstructorCode = s"""{
      | "type": "ClassMethod",
      | "key": {
      |   "type": "Identifier",
      |   "name": "constructor",
      |   "loc": {
      |     "start": {
      |       "line": ${forElem.lineNumber.getOrElse(-1)},
      |       "column": ${forElem.columnNumber.getOrElse(-1)}
      |     }
      |   }
      | },
      | "kind": "constructor",
      | "id": null,
      | "params": [],
      | "body": {
      |   "type": "BlockStatement",
      |   "body": []
      | }
      |}""".stripMargin
    val result = createMethodAstAndNode(
      createBabelNodeInfo(ujson.read(fakeConstructorCode)),
      methodBlockContent = methodBlockContent
    )
    result.methodNode.code(code)
    result
  }

  private def findClassConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(isConstructor)

  private def createClassConstructor(classExpr: BabelNodeInfo, constructorContent: List[Ast]): MethodAst =
    findClassConstructor(classExpr) match {
      case Some(classConstructor) if hasKey(classConstructor, "body") =>
        val result =
          createMethodAstAndNode(createBabelNodeInfo(classConstructor), methodBlockContent = constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
      case Some(classConstructor) =>
        val methodNode =
          createMethodDefinitionNode(createBabelNodeInfo(classConstructor), methodBlockContent = constructorContent)
        diffGraph.addEdge(methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        MethodAst(Ast(methodNode), methodNode, Ast(methodNode))
      case _ =>
        val result = createFakeConstructor("constructor() {}", classExpr, methodBlockContent = constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result
    }

  private def interfaceConstructor(typeName: String, tsInterface: BabelNodeInfo): NewMethod =
    findClassConstructor(tsInterface) match {
      case Some(interfaceConstructor) => createMethodDefinitionNode(createBabelNodeInfo(interfaceConstructor))
      case _                          => createFakeConstructor(s"new: $typeName", tsInterface).methodNode
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

  private def astForClassMember(
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
        val function    = createMethodDefinitionNode(nodeInfo)
        val bindingNode = newBindingNode("", "", "")
        diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
        diffGraph.addEdge(bindingNode, function, EdgeTypes.REF)
        addModifier(function, nodeInfo.json)
        memberNode(nodeInfo, function.name, nodeInfo.code, typeFullName, Seq(function.fullName))
          .possibleTypes(possibleTypes)
      case ClassMethod | ClassPrivateMethod =>
        val function    = createMethodAstAndNode(nodeInfo).methodNode
        val bindingNode = newBindingNode("", "", "")
        diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
        diffGraph.addEdge(bindingNode, function, EdgeTypes.REF)
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
        memberNode(nodeInfo, name, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
      case _ =>
        val name = nodeInfo.node match {
          case ClassProperty        => code(nodeInfo.json("key"))
          case ClassPrivateProperty => code(nodeInfo.json("key")("id"))
          // TODO: name field most likely needs adjustment for other Babel AST types
          case _ => nodeInfo.code
        }
        memberNode(nodeInfo, name, nodeInfo.code, typeFullName).possibleTypes(possibleTypes)
    }

    addModifier(memberNode_, classElement)
    diffGraph.addEdge(typeDeclNode, memberNode_, EdgeTypes.AST)
    astsForDecorators(nodeInfo).foreach { decoratorAst =>
      Ast.storeInDiffGraph(decoratorAst, diffGraph)
      decoratorAst.root.foreach(diffGraph.addEdge(memberNode_, _, EdgeTypes.AST))
    }

    if (!ignoreInitCalls && hasKey(nodeInfo.json, "value") && !nodeInfo.json("value").isNull) {
      val lhsAst = astForNode(nodeInfo.json("key"))
      val rhsAst = astForNodeWithFunctionReference(nodeInfo.json("value"))
      val callNode_ =
        callNode(nodeInfo, nodeInfo.code, Operators.assignment, DispatchTypes.STATIC_DISPATCH)
      val argAsts = List(lhsAst, rhsAst)
      callAst(callNode_, argAsts)
    } else {
      Ast()
    }
  }

  protected def astForEnum(tsEnum: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsEnum)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

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

    val memberAsts = tsEnum.json("members").arr.toList.flatMap(m => astsForEnumMember(createBabelNodeInfo(m)))

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(member), diffGraph)
    } else {
      val init =
        staticInitMethodAst(calls, s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}", None, Defines.Any)
      Ast.storeInDiffGraph(Ast(typeDeclNode_).withChildren(member).withChild(init), diffGraph)
    }

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

  protected def astForClass(clazz: BabelNodeInfo, shouldCreateAssignmentCall: Boolean = false): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    registerType(typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

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
    val memberInitCalls = allClassMembers
      .filter(m => !isStaticMember(m) && isInitializedMember(m))
      .map(m => astForClassMember(m, typeDeclNode_))

    val constructor     = createClassConstructor(clazz, memberInitCalls)
    val constructorNode = constructor.methodNode

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
      .map(m => astForClassMember(m, typeDeclNode_))

    // adding all static members and retrieving their initialization calls
    val staticMemberInitCalls =
      allClassMembers.filter(isStaticMember).map(m => astForClassMember(m, typeDeclNode_))

    // retrieving initialization calls from the static initialization block if any
    val staticInitBlock = allClassMembers.find(isStaticInitBlock)
    val staticInitBlockAsts =
      staticInitBlock.map(block => block("body").arr.toList.map(astForNodeWithFunctionReference)).getOrElse(List.empty)

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    if (staticMemberInitCalls.nonEmpty || staticInitBlockAsts.nonEmpty) {
      val init = staticInitMethodAst(
        staticMemberInitCalls ++ staticInitBlockAsts,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.Any
      )
      Ast.storeInDiffGraph(init, diffGraph)
      diffGraph.addEdge(typeDeclNode_, init.nodes.head, EdgeTypes.AST)
    }

    if (shouldCreateAssignmentCall) {
      diffGraph.addEdge(localAstParentStack.head, typeRefNode_, EdgeTypes.AST)

      // return a synthetic assignment to enable tracing of the implicitly created identifier for
      // the class definition assigned to its constructor
      val classIdNode = identifierNode(clazz, typeName, Seq(constructorNode.fullName))
      val constructorRefNode =
        methodRefNode(clazz, constructorNode.code, constructorNode.fullName, constructorNode.fullName)

      val idLocal = localNode(clazz, typeName, typeName, Defines.Any).order(0)
      diffGraph.addEdge(localAstParentStack.head, idLocal, EdgeTypes.AST)
      scope.addVariable(typeName, idLocal, BlockScope)
      scope.addVariableReference(typeName, classIdNode)

      createAssignmentCallAst(
        classIdNode,
        constructorRefNode,
        s"$typeName = ${constructorNode.fullName}",
        clazz.lineNumber,
        clazz.columnNumber
      )
    } else {
      Ast(typeRefNode_)
    }
  }

  protected def addModifier(node: NewNode, json: Value): Unit = createBabelNodeInfo(json).node match {
    case ClassPrivateProperty =>
      diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PRIVATE), EdgeTypes.AST)
    case _ =>
      if (safeBool(json, "abstract").contains(true))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.ABSTRACT), EdgeTypes.AST)
      if (safeBool(json, "static").contains(true))
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.STATIC), EdgeTypes.AST)
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
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

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

    val constructorBindingNode = newBindingNode("", "", "")
    diffGraph.addEdge(typeDeclNode_, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val interfaceBodyElements = classMembers(tsInterface, withConstructor = false)

    interfaceBodyElements.foreach { classElement =>
      val nodeInfo      = createBabelNodeInfo(classElement)
      val tpe           = typeFor(nodeInfo)
      val possibleTypes = Seq(tpe)
      val typeFullName  = if (Defines.isBuiltinType(tpe)) tpe else Defines.Any
      val memberNodes = nodeInfo.node match {
        case TSCallSignatureDeclaration | TSMethodSignature =>
          val functionNode = createMethodDefinitionNode(nodeInfo)
          val bindingNode  = newBindingNode("", "", "")
          diffGraph.addEdge(typeDeclNode_, bindingNode, EdgeTypes.BINDS)
          diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
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
