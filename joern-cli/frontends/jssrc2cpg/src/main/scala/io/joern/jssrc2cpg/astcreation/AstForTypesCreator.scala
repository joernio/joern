package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.passes.Defines
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.nodes.NewCall
import io.shiftleft.codepropertygraph.generated.DispatchTypes
import io.shiftleft.codepropertygraph.generated.Operators
import io.shiftleft.codepropertygraph.generated.nodes.NewTypeDecl
import ujson.Value

import scala.util.Try

trait AstForTypesCreator { this: AstCreator =>

  protected def astForTypeAlias(alias: BabelNodeInfo): Ast = {
    val (aliasName, aliasFullName) = calcTypeNameAndFullName(alias)
    val name = if (hasKey(alias.json, "right")) {
      typeFor(createBabelNodeInfo(alias.json("right")))
    } else {
      typeFor(createBabelNodeInfo(alias.json))
    }
    registerType(aliasName, aliasFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

    val aliasTypeDeclNode =
      createTypeDeclNode(aliasName, aliasFullName, parserResult.filename, alias.code, astParentType, astParentFullName)
    seenAliasTypes.add(aliasTypeDeclNode)

    val typeDeclNodeAst =
      if (!Defines.JSTYPES.contains(name) && !seenAliasTypes.exists(_.name == name)) {
        val (typeName, typeFullName) = calcTypeNameAndFullName(alias, Some(name))
        val typeDeclNode = createTypeDeclNode(
          typeName,
          typeFullName,
          parserResult.filename,
          alias.code,
          astParentType,
          astParentFullName,
          alias = Some(aliasFullName)
        )
        registerType(typeName, typeFullName)
        Ast(typeDeclNode)
      } else {
        seenAliasTypes
          .collectFirst { case typeDecl if typeDecl.name == name => Ast(typeDecl.aliasTypeFullName(aliasFullName)) }
          .getOrElse(Ast())
      }

    typeDeclNodeAst.root.foreach(diffGraph.addEdge(methodAstParentStack.head, _, EdgeTypes.AST))
    Ast(aliasTypeDeclNode)
  }

  private def isConstructor(json: Value): Boolean = createBabelNodeInfo(json).node match {
    case TSConstructSignatureDeclaration => true
    case _                               => safeStr(json, "kind").contains("constructor")
  }

  private def classMembers(clazz: BabelNodeInfo, withConstructor: Boolean = true): Seq[Value] = {
    val allMembers = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    if (withConstructor) {
      allMembers
    } else {
      allMembers.filterNot(isConstructor)
    }
  }

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

  private def createClassConstructor(classExpr: BabelNodeInfo, constructorContent: List[Ast]): Ast =
    findClassConstructor(classExpr) match {
      case Some(classConstructor) if hasKey(classConstructor, "body") =>
        val result =
          createMethodAstAndNode(createBabelNodeInfo(classConstructor), methodBlockContent = constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result.methodAst
      case Some(classConstructor) =>
        val methodNode =
          createMethodDefinitionNode(createBabelNodeInfo(classConstructor), methodBlockContent = constructorContent)
        diffGraph.addEdge(methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        Ast(methodNode)
      case _ =>
        val result = createFakeConstructor("constructor() {}", classExpr, methodBlockContent = constructorContent)
        diffGraph.addEdge(result.methodNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)
        result.methodAst
    }

  private def interfaceConstructor(typeName: String, tsInterface: BabelNodeInfo): NewMethod =
    findClassConstructor(tsInterface) match {
      case Some(interfaceConstructor) => createMethodDefinitionNode(createBabelNodeInfo(interfaceConstructor))
      case _                          => createFakeConstructor(s"new: $typeName", tsInterface).methodNode
    }

  private def astsForEnumMember(tsEnumMember: BabelNodeInfo): Seq[Ast] = {
    val name       = code(tsEnumMember.json("id"))
    val memberNode = createMemberNode(name, tsEnumMember.code, dynamicTypeOption = None)
    addModifier(memberNode, tsEnumMember.json)

    if (hasKey(tsEnumMember.json, "initializer")) {
      val lhsAst = astForNode(tsEnumMember.json("id"))
      val rhsAst = astForNodeWithFunctionReference(tsEnumMember.json("initializer"))
      val callNode =
        createCallNode(
          tsEnumMember.code,
          Operators.assignment,
          DispatchTypes.STATIC_DISPATCH,
          tsEnumMember.lineNumber,
          tsEnumMember.columnNumber
        )
      val argAsts = List(lhsAst, rhsAst)
      Seq(createCallAst(callNode, argAsts), Ast(memberNode))
    } else {
      Seq(Ast(memberNode))
    }
  }

  private def astForClassMember(classElement: Value, typeDeclNode: NewTypeDecl): Ast = {
    val nodeInfo = createBabelNodeInfo(classElement)
    val memberNode = nodeInfo.node match {
      case ClassMethod | ClassPrivateMethod =>
        val function = createMethodAstAndNode(nodeInfo).methodNode
        addModifier(function, nodeInfo.json)
        val dynamicTypeHintFullName = Some(function.fullName)
        createMemberNode(function.name, nodeInfo.code, dynamicTypeHintFullName)
      case _ =>
        val name = nodeInfo.node match {
          case ClassProperty        => code(nodeInfo.json("key"))
          case ClassPrivateProperty => code(nodeInfo.json("key")("id"))
          // TODO: name field most likely needs adjustment for other Babel AST types
          case _ => nodeInfo.code
        }
        createMemberNode(name, nodeInfo.code, dynamicTypeOption = None)
    }
    addModifier(memberNode, classElement)
    diffGraph.addEdge(typeDeclNode, memberNode, EdgeTypes.AST)

    if (hasKey(nodeInfo.json, "value") && !nodeInfo.json("value").isNull) {
      val lhsAst = astForNode(nodeInfo.json("key"))
      val rhsAst = astForNodeWithFunctionReference(nodeInfo.json("value"))
      val callNode =
        createCallNode(
          nodeInfo.code,
          Operators.assignment,
          DispatchTypes.STATIC_DISPATCH,
          nodeInfo.lineNumber,
          nodeInfo.columnNumber
        )
      val argAsts = List(lhsAst, rhsAst)
      createCallAst(callNode, argAsts)
    } else {
      Ast()
    }
  }

  protected def astForEnum(tsEnum: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsEnum)
    registerType(typeName, typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

    val typeDeclNode = createTypeDeclNode(
      typeName,
      typeFullName,
      parserResult.filename,
      s"enum $typeName",
      astParentType,
      astParentFullName
    )
    seenAliasTypes.add(typeDeclNode)

    addModifier(typeDeclNode, tsEnum.json)

    methodAstParentStack.push(typeDeclNode)
    dynamicInstanceTypeStack.push(typeFullName)

    val memberAsts = tsEnum.json("members").arr.toList.flatMap(m => astsForEnumMember(createBabelNodeInfo(m)))

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDeclNode).withChildren(member)
    } else {
      val init =
        staticInitMethodAst(calls, s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}", None, Defines.ANY)
      Ast(typeDeclNode).withChildren(member).withChild(init)
    }
  }

  private def isStaticMember(json: Value): Boolean = {
    val nodeInfo = createBabelNodeInfo(json).node
    val isStatic = safeBool(json, "static").contains(true)
    nodeInfo != ClassMethod && nodeInfo != ClassPrivateMethod && isStatic
  }

  private def isInitializedMember(json: Value): Boolean = hasKey(json, "value") && !json("value").isNull

  private def isStaticInitBlock(json: Value): Boolean = createBabelNodeInfo(json).node == StaticBlock

  private def isClassMethodOrUninitializedMember(json: Value): Boolean = {
    val nodeInfo = createBabelNodeInfo(json).node
    !isStaticInitBlock(json) &&
    (nodeInfo == ClassMethod || nodeInfo == ClassPrivateMethod || !isInitializedMember(json))
  }

  protected def astForClass(clazz: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    registerType(typeName, typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

    val superClass = Try(createBabelNodeInfo(clazz.json("superClass")).code).toOption.toSeq
    val implements = Try(clazz.json("implements").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten
    val mixins     = Try(clazz.json("mixins").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten

    val typeDeclNode = createTypeDeclNode(
      typeName,
      typeFullName,
      parserResult.filename,
      s"class $typeName",
      astParentType,
      astParentFullName,
      inherits = superClass ++ implements ++ mixins
    )
    seenAliasTypes.add(typeDeclNode)

    addModifier(typeDeclNode, clazz.json)

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode, EdgeTypes.AST)

    val typeRefNode = createTypeRefNode(s"class $typeName", typeFullName, clazz)

    methodAstParentStack.push(typeDeclNode)
    dynamicInstanceTypeStack.push(typeFullName)
    typeRefIdStack.push(typeRefNode)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode, None)

    val allClassMembers = classMembers(clazz, withConstructor = false).toList

    // adding all other members and retrieving their initialization calls
    val memberInitCalls = allClassMembers
      .filter(m => !isStaticMember(m) && isInitializedMember(m))
      .map(m => astForClassMember(m, typeDeclNode))

    val constructorAst = createClassConstructor(clazz, memberInitCalls)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(typeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorAst.nodes.head, EdgeTypes.REF)

    // adding all class methods / functions and uninitialized, non-static members
    allClassMembers
      .filter(member => isClassMethodOrUninitializedMember(member) && !isStaticMember(member))
      .map(m => astForClassMember(m, typeDeclNode))

    // adding all static members and retrieving their initialization calls
    val staticMemberInitCalls =
      allClassMembers.filter(isStaticMember).map(m => astForClassMember(m, typeDeclNode))

    // retrieving initialization calls from the static initialization block if any
    val staticInitBlock = allClassMembers.find(isStaticInitBlock)
    val staticInitBlockAsts =
      staticInitBlock.map(block => block("body").arr.toList.map(astForNode)).getOrElse(List.empty)

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    typeRefIdStack.pop()
    scope.popScope()

    if (staticMemberInitCalls.nonEmpty || staticInitBlockAsts.nonEmpty) {
      val init = staticInitMethodAst(
        staticMemberInitCalls ++ staticInitBlockAsts,
        s"$typeFullName:${io.joern.x2cpg.Defines.StaticInitMethodName}",
        None,
        Defines.ANY
      )
      Ast.storeInDiffGraph(init, diffGraph)
      diffGraph.addEdge(typeDeclNode, init.nodes.head, EdgeTypes.AST)
    }
    Ast(typeRefNode)
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
    registerType(typeName, typeFullName)

    val astParentType     = methodAstParentStack.head.label
    val astParentFullName = methodAstParentStack.head.properties("FULL_NAME").toString

    val extendz = Try(tsInterface.json("extends").arr.map(createBabelNodeInfo(_).code)).toOption.toSeq.flatten

    val typeDeclNode = createTypeDeclNode(
      typeName,
      typeFullName,
      parserResult.filename,
      s"interface $typeName",
      astParentType,
      astParentFullName,
      inherits = extendz
    )
    seenAliasTypes.add(typeDeclNode)

    addModifier(typeDeclNode, tsInterface.json)

    methodAstParentStack.push(typeDeclNode)
    dynamicInstanceTypeStack.push(typeFullName)

    scope.pushNewMethodScope(typeFullName, typeName, typeDeclNode, None)

    val constructorNode = interfaceConstructor(typeName, tsInterface)
    diffGraph.addEdge(constructorNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(typeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val interfaceBodyElements = classMembers(tsInterface, withConstructor = false)

    interfaceBodyElements.foreach { classElement =>
      val nodeInfo = createBabelNodeInfo(classElement)
      val memberNodes = nodeInfo.node match {
        case TSCallSignatureDeclaration =>
          val functionNode            = createMethodDefinitionNode(nodeInfo)
          val dynamicTypeHintFullName = Some(functionNode.fullName)
          val bindingNode             = createBindingNode()
          diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
          diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
          addModifier(functionNode, nodeInfo.json)
          Seq(createMemberNode(functionNode.name, nodeInfo.code, dynamicTypeHintFullName))
        case _ =>
          val names = nodeInfo.node match {
            case TSPropertySignature =>
              if (hasKey(nodeInfo.json("key"), "value")) {
                Seq(nodeInfo.json("key")("value").str)
              } else Seq(code(nodeInfo.json("key")))
            case TSIndexSignature =>
              nodeInfo.json("parameters").arr.toSeq.map(_("name").str)
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => Seq(nodeInfo.code)
          }
          names.map { n =>
            val node = createMemberNode(n, nodeInfo.code, dynamicTypeOption = None)
            addModifier(node, nodeInfo.json)
            node
          }
      }
      memberNodes.foreach(diffGraph.addEdge(typeDeclNode, _, EdgeTypes.AST))
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()

    Ast(typeDeclNode)
  }

}
