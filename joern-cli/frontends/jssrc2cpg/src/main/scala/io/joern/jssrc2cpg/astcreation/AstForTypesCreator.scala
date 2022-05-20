package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import io.shiftleft.codepropertygraph.generated.nodes.NewModifier
import io.shiftleft.codepropertygraph.generated.nodes.NewNode
import io.shiftleft.codepropertygraph.generated.ModifierTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import ujson.Value

import scala.util.Try

trait AstForTypesCreator {

  this: AstCreator =>

  private def isConstructor(json: Value): Boolean = {
    createBabelNodeInfo(json) match {
      case BabelNodeInfo(BabelAst.TSConstructSignatureDeclaration) => true
      case _ => hasKey(json, "kind") && json("kind").str == "constructor"
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

  private def classConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(isConstructor)

  private def classConstructor(typeName: String, classExpr: BabelNodeInfo): NewMethod = {
    val maybeClassConstructor = classConstructor(classExpr)
    val methodNode = maybeClassConstructor match {
      case Some(classConstructor) => createMethodAstAndNode(createBabelNodeInfo(classConstructor))._2
      case _ =>
        val code = "constructor() {}"
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
        val methodNode = createMethodAstAndNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)))._2
        methodNode.code(code)
    }
    val name     = methodNode.name.replace("<", s"$typeName<")
    val fullName = methodNode.fullName.replace("<", s"$typeName<")
    methodNode.name(name).fullName(fullName)
  }

  private def interfaceConstructor(typeName: String, tsInterface: BabelNodeInfo): NewMethod = {
    val maybeInterfaceConstructor = classConstructor(tsInterface)
    val methodNode = maybeInterfaceConstructor match {
      case Some(interfaceConstructor) => createMethodDefinitionNode(createBabelNodeInfo(interfaceConstructor))
      case _ =>
        val code = s"new: $typeName"
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
        val methodNode = createMethodAstAndNode(createBabelNodeInfo(ujson.read(fakeConstructorCode)))._2
        methodNode.code(code)
    }
    val name     = methodNode.name.replace("<", s"$typeName<")
    val fullName = methodNode.fullName.replace("<", s"$typeName<")
    methodNode.name(name).fullName(fullName)
  }

  protected def astForClass(clazz: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    val metaTypeName             = s"$typeName<meta>"
    val metaTypeFullName         = s"$typeFullName<meta>"

    val classTypeNode = createTypeNode(typeName, typeFullName)
    Ast.storeInDiffGraph(Ast(classTypeNode), diffGraph)

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

    addModifier(typeDeclNode, clazz.json)

    val classMetaTypeNode = createTypeNode(metaTypeName, metaTypeFullName)
    Ast.storeInDiffGraph(Ast(classMetaTypeNode), diffGraph)

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
      val memberNode = createBabelNodeInfo(classElement) match {
        case m @ (BabelNodeInfo(BabelAst.ClassMethod) | BabelNodeInfo(BabelAst.ClassPrivateMethod)) =>
          val function = createMethodAstAndNode(m)._2
          addModifier(function, m.json)
          val fullName                = function.fullName.replace(s":${function.name}", s":$typeName:${function.name}")
          val classMethod             = function.fullName(fullName)
          val functionFullName        = classMethod.fullName
          val dynamicTypeHintFullName = Some(functionFullName)
          createMemberNode(classMethod.name, m.code, dynamicTypeHintFullName)
        case other =>
          val name = other match {
            case classProperty @ BabelNodeInfo(BabelAst.ClassProperty) =>
              code(classProperty.json("key"))
            case privateName @ BabelNodeInfo(BabelAst.ClassPrivateProperty) =>
              code(privateName.json("key")("id"))
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => other.code
          }
          createMemberNode(name, other.code, dynamicTypeOption = None)
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
    createBabelNodeInfo(json) match {
      case BabelNodeInfo(BabelAst.ClassPrivateProperty) =>
        diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PRIVATE), EdgeTypes.AST)
      case _ =>
        if (hasKey(json, "abstract") && json("abstract").bool)
          diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.ABSTRACT), EdgeTypes.AST)
        if (hasKey(json, "static") && json("static").bool)
          diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.STATIC), EdgeTypes.AST)
        if (hasKey(json, "accessibility") && json("accessibility").str == "public")
          diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PUBLIC), EdgeTypes.AST)
        if (hasKey(json, "accessibility") && json("accessibility").str == "private")
          diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PRIVATE), EdgeTypes.AST)
        if (hasKey(json, "accessibility") && json("accessibility").str == "protected") {
          diffGraph.addEdge(node, NewModifier().modifierType(ModifierTypes.PROTECTED), EdgeTypes.AST)
        }
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

    val blockAst = createBabelNodeInfo(tsModuleDecl.json("body")) match {
      case module @ BabelNodeInfo(BabelAst.TSModuleDeclaration) => astForModule(module)
      case other                                                => astForBlockStatement(other)
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    scope.popScope()
    scope.popScope()

    Ast(namespaceNode).withChild(blockAst)
  }

  protected def astForInterface(tsInterface: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(tsInterface)

    val classTypeNode = createTypeNode(typeName, typeFullName)
    Ast.storeInDiffGraph(Ast(classTypeNode), diffGraph)

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
      val memberNodes = createBabelNodeInfo(classElement) match {
        case m @ BabelNodeInfo(BabelAst.TSCallSignatureDeclaration) =>
          val functionNode = createMethodDefinitionNode(m)
          val fullName     = functionNode.fullName.replace(s":${functionNode.name}", s":$typeName:${functionNode.name}")
          val classMethod  = functionNode.fullName(fullName)
          val functionFullName        = classMethod.fullName
          val dynamicTypeHintFullName = Some(functionFullName)

          val bindingNode = createBindingNode()
          diffGraph.addEdge(typeDeclNode, bindingNode, EdgeTypes.BINDS)
          diffGraph.addEdge(bindingNode, functionNode, EdgeTypes.REF)
          addModifier(functionNode, m.json)
          Seq(createMemberNode(classMethod.name, m.code, dynamicTypeHintFullName))
        case other =>
          val names = other match {
            case propSignature @ BabelNodeInfo(BabelAst.TSPropertySignature) =>
              if (hasKey(propSignature.json("key"), "value")) {
                Seq(propSignature.json("key")("value").str)
              } else Seq(code(propSignature.json("key")))
            case indexSignature @ BabelNodeInfo(BabelAst.TSIndexSignature) =>
              indexSignature.json("parameters").arr.toSeq.map(_("name").str)
            // TODO: name field most likely needs adjustment for other Babel AST types
            case _ => Seq(other.code)
          }
          names.map { n =>
            val node = createMemberNode(n, other.code, dynamicTypeOption = None)
            addModifier(node, other.json)
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
