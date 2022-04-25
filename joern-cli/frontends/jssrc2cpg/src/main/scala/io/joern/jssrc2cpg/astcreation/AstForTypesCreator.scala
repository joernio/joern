package io.joern.jssrc2cpg.astcreation

import io.joern.x2cpg.datastructures.Stack._
import io.joern.jssrc2cpg.parser.BabelAst
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.x2cpg.Ast
import io.shiftleft.codepropertygraph.generated.EdgeTypes
import io.shiftleft.codepropertygraph.generated.nodes.NewMethod
import ujson.Value

import scala.util.Try

trait AstForTypesCreator {

  this: AstCreator =>

  private def classMembers(clazz: BabelNodeInfo, withConstructor: Boolean = true): Seq[Value] = {
    val allMembers = Try(clazz.json("body")("body").arr).toOption.toSeq.flatten
    if (withConstructor) {
      allMembers
    } else {
      allMembers.filterNot(_("kind").str == "constructor")
    }
  }

  private def classConstructor(clazz: BabelNodeInfo): Option[Value] =
    classMembers(clazz).find(_("kind").str == "constructor")

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

  protected def astForClass(clazz: BabelNodeInfo): Ast = {
    val (typeName, typeFullName) = calcTypeNameAndFullName(clazz)
    val metaTypeName             = s"$typeName<meta>"
    val metaTypeFullName         = s"$typeFullName<meta>"

    val classTypeNode = createTypeNode(typeName, typeFullName)
    Ast.storeInDiffGraph(Ast(classTypeNode), diffGraph)

    // We do not need to look at classNode.getClassHeritage because
    // the CPG only allows us to encode inheriting from fully known
    // types. Since in JS we "inherit" from a variable which would
    // need to be resolved first, we for now dont handle the class
    // hierarchy.
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

    // In case there is no user-written constructor the JS parser creates
    // an empty one automatically. Hence, the following is safe:
    val constructorNode = classConstructor(typeName, clazz)

    val constructorBindingNode = createBindingNode()
    diffGraph.addEdge(metaTypeDeclNode, constructorBindingNode, EdgeTypes.BINDS)
    diffGraph.addEdge(constructorBindingNode, constructorNode, EdgeTypes.REF)

    val classBodyElements = classMembers(clazz, withConstructor = false)

    classBodyElements.foreach { classElement =>
      val memberId = createBabelNodeInfo(classElement) match {
        case m @ BabelNodeInfo(BabelAst.ClassMethod) =>
          val function                = createMethodAstAndNode(m)._2
          val fullName                = function.fullName.replace(s":${function.name}", s":$typeName:${function.name}")
          val classMethod             = function.fullName(fullName)
          val functionFullName        = classMethod.fullName
          val dynamicTypeHintFullName = Some(functionFullName)
          createMemberNode(classMethod.name, m.code, dynamicTypeHintFullName)
        case other =>
          // TODO: name field most likely needs adjustment
          createMemberNode(other.code, other.code, dynamicTypeOption = None)
      }

      if (classElement("static").bool) {
        diffGraph.addEdge(metaTypeDeclNode, memberId, EdgeTypes.AST)
      } else {
        diffGraph.addEdge(typeDeclNode, memberId, EdgeTypes.AST)
      }
    }

    methodAstParentStack.pop()
    dynamicInstanceTypeStack.pop()
    metaTypeRefIdStack.pop()

    Ast(metaTypeRefNode)
  }

}
