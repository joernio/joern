package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
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
import ujson.Value

import scala.util.Try

trait AstForTypesCreator {

  this: AstCreator =>

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
      if (
        !Defines.values
          .exists { case typeName: Defines.Tpe => typeName.label == name } && !seenAliasTypes.exists(_.name == name)
      ) {
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

    Ast(aliasTypeDeclNode).merge(typeDeclNodeAst)
  }

  private def isConstructor(json: Value): Boolean = {
    createBabelNodeInfo(json).node match {
      case BabelAst.TSConstructSignatureDeclaration => true
      case _                                        => safeStr(json, "kind").contains("constructor")
    }

  }

  private def classMembers(clazz: BabelNodeInfo, withConstructor: Boolean = true): Seq[Value] = {
    val allMembers = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    if (withConstructor) {
      allMembers
    } else {
      allMembers.filterNot(isConstructor)
    }
  }

  private def createFakeConstructor(code: String): NewMethod = {
    val fakeConstructorCode = """{
      | "type": "ClassMethod",
      | "key": {
      |   "type": "Identifier",
      |   "name": "constructor"
      | },
      | "kind": "constructor",
      | "id": null,
      | "params": [],
      | "body": {
      |   "type": "BlockStatement",
      |   "body": []
      | }
      |}""".stripMargin
    val (_, methodNode) = createMethodAstAndNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)))
    methodNode.code(code)
  }

  private def fixMethodNames(methodNode: NewMethod, withTypeName: String): NewMethod = {
    val name     = methodNode.name.replace("<", s"$withTypeName<")
    val fullName = methodNode.fullName.replace("<", s"$withTypeName<")
    methodNode.name(name).fullName(fullName)
  }

  private def classConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(isConstructor)

  private def classConstructor(typeName: String, classExpr: BabelNodeInfo): NewMethod = {
    val methodNode = classConstructor(classExpr) match {
      case Some(classConstructor) if hasKey(classConstructor, "body") =>
        val (_, methodNode) = createMethodAstAndNode(createBabelNodeInfo(classConstructor))
        methodNode
      case Some(classConstructor) =>
        createMethodDefinitionNode(createBabelNodeInfo(classConstructor))
      case _ =>
        createFakeConstructor("constructor() {}")
    }
    fixMethodNames(methodNode, typeName)
  }

  private def interfaceConstructor(typeName: String, tsInterface: BabelNodeInfo): NewMethod = {
    val methodNode = classConstructor(tsInterface) match {
      case Some(interfaceConstructor) => createMethodDefinitionNode(createBabelNodeInfo(interfaceConstructor))
      case _                          => createFakeConstructor(s"new: $typeName")
    }
    fixMethodNames(methodNode, typeName)
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

    val memberAsts = tsEnum.json("members").arr.toSeq.flatMap(m => astsForEnumMember(createBabelNodeInfo(m)))

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()

    val (calls, member) = memberAsts.partition(_.nodes.headOption.exists(_.isInstanceOf[NewCall]))
    if (calls.isEmpty) {
      Ast(typeDeclNode).withChildren(member)
    } else {
      val init =
        createAstForFakeStaticInitMethod(typeFullName, parserResult.filename, tsEnum.lineNumber, calls)
      Ast(typeDeclNode).withChildren(member).withChild(init)
    }
  }

  protected def fixMethodFullName(method: NewMethod, withTypeName: String): NewMethod =
    method.fullName(method.fullName.replace(s":${method.name}", s":$withTypeName:${method.name}"))

  protected def astForClass(clazz: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    val metaTypeName             = s"$typeName<meta>"
    val metaTypeFullName         = s"$typeFullName<meta>"

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

    registerType(metaTypeName, metaTypeFullName)

    val metaTypeDeclNode = createTypeDeclNode(
      metaTypeName,
      metaTypeFullName,
      parserResult.filename,
      s"class $metaTypeName",
      astParentType,
      astParentFullName
    )

    diffGraph.addEdge(methodAstParentStack.head, typeDeclNode, EdgeTypes.AST)
    diffGraph.addEdge(methodAstParentStack.head, metaTypeDeclNode, EdgeTypes.AST)

    val metaTypeRefNode = createTypeRefNode(s"class $typeName", metaTypeFullName, clazz)

    methodAstParentStack.push(typeDeclNode)
    dynamicInstanceTypeStack.push(typeFullName)
    metaTypeRefIdStack.push(metaTypeRefNode)

    val constructorNode = classConstructor(typeName, clazz)
    diffGraph.addEdge(constructorNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(metaTypeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val classBodyElements = classMembers(clazz, withConstructor = false)

    classBodyElements.foreach { classElement =>
      val nodeInfo = createBabelNodeInfo(classElement)
      val memberNode = nodeInfo.node match {
        case BabelAst.ClassMethod | BabelAst.ClassPrivateMethod =>
          val (_, function) = createMethodAstAndNode(nodeInfo)
          addModifier(function, nodeInfo.json)
          val classMethod             = fixMethodFullName(function, typeName)
          val dynamicTypeHintFullName = Some(classMethod.fullName)
          createMemberNode(classMethod.name, nodeInfo.code, dynamicTypeHintFullName)
        case _ =>
          val name = nodeInfo.node match {
            case BabelAst.ClassProperty =>
              code(nodeInfo.json("key"))
            case BabelAst.ClassPrivateProperty =>
              code(nodeInfo.json("key")("id"))
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => nodeInfo.code
          }
          createMemberNode(name, nodeInfo.code, dynamicTypeOption = None)
      }
      addModifier(memberNode, classElement)
      if (classElement("static").bool) {
        diffGraph.addEdge(metaTypeDeclNode, memberNode, EdgeTypes.AST)
      } else {
        diffGraph.addEdge(typeDeclNode, memberNode, EdgeTypes.AST)
      }
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    metaTypeRefIdStack.pop()

    Ast(metaTypeRefNode)
  }

  protected def addModifier(node: NewNode, json: Value): Unit = {
    createBabelNodeInfo(json).node match {
      case BabelAst.ClassPrivateProperty =>
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

    scope.pushNewMethodScope(fullName, name, namespaceNode, None)
    scope.pushNewBlockScope(namespaceNode)
    methodAstParentStack.push(namespaceNode)
    dynamicInstanceTypeStack.push(fullName)

    val blockAst = if (hasKey(tsModuleDecl.json, "body")) {
      val nodeInfo = createBabelNodeInfo(tsModuleDecl.json("body"))
      nodeInfo.node match {
        case BabelAst.TSModuleDeclaration => astForModule(nodeInfo)
        case _                            => astForBlockStatement(nodeInfo)
      }
    } else {
      Ast()
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()
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

    val constructorNode = interfaceConstructor(typeName, tsInterface)
    diffGraph.addEdge(constructorNode, NewModifier().modifierType(ModifierTypes.CONSTRUCTOR), EdgeTypes.AST)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(typeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val interfaceBodyElements = classMembers(tsInterface, withConstructor = false)

    interfaceBodyElements.foreach { classElement =>
      val nodeInfo = createBabelNodeInfo(classElement)
      val memberNodes = nodeInfo.node match {
        case BabelAst.TSCallSignatureDeclaration =>
          val functionNode            = createMethodDefinitionNode(nodeInfo)
          val classMethod             = fixMethodFullName(functionNode, typeName)
          val dynamicTypeHintFullName = Some(classMethod.fullName)
          val bindingNode             = createBindingNode()
          diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
          diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
          addModifier(functionNode, nodeInfo.json)
          Seq(createMemberNode(classMethod.name, nodeInfo.code, dynamicTypeHintFullName))
        case _ =>
          val names = nodeInfo.node match {
            case BabelAst.TSPropertySignature =>
              if (hasKey(nodeInfo.json("key"), "value")) {
                Seq(nodeInfo.json("key")("value").str)
              } else Seq(code(nodeInfo.json("key")))
            case BabelAst.TSIndexSignature =>
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

    Ast(typeDeclNode)
  }

}
