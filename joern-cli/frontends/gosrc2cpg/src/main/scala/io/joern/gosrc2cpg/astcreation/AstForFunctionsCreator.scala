package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.{BlockStmt, Ellipsis, Ident, SelectorExpr}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.datastructures.Stack.*
import io.joern.x2cpg.utils.StringUtils
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.PropertyNames
import io.shiftleft.codepropertygraph.generated.nodes.*
import ujson.Value

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.util.{Failure, Success, Try}

trait AstForFunctionsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>
  private def createFunctionTypeAndTypeDecl(
    node: ParserNodeInfo,
    method: NewMethod,
    methodName: String,
    methodFullName: String,
    signature: String
  ): Ast = {

    val parentNode: NewTypeDecl = methodAstParentStack.collectFirst { case t: NewTypeDecl => t }.getOrElse {
      // TODO: Need to add respective Unit test to test this possibility, as looks to me as dead code.
      //  Replicated it from 'c2cpg' by referring AstForFunctionsCreator.
      val astParentType     = methodAstParentStack.head.label
      val astParentFullName = methodAstParentStack.head.properties(PropertyNames.FULL_NAME).toString
      val typeDeclNode_ =
        typeDeclNode(node, methodName, methodFullName, method.filename, methodName, astParentType, astParentFullName)
      Ast.storeInDiffGraph(Ast(typeDeclNode_), diffGraph)
      typeDeclNode_
    }

    method.astParentFullName = parentNode.fullName
    method.astParentType = parentNode.label
    val functionBinding = NewBinding().name(methodName).methodFullName(methodFullName).signature(signature)
    Ast(functionBinding).withBindsEdge(parentNode, functionBinding).withRefEdge(functionBinding, method)
  }
  def astForFuncDecl(funcDecl: ParserNodeInfo): Seq[Ast] = {

    val filename = relPathFileName
    val name     = funcDecl.json(ParserKeys.Name).obj(ParserKeys.Name).str
    val fullname = s"$fullyQualifiedPackage.$name"
    // TODO: handle multiple return type or tuple (int, int)
    val (returnTypeStr, methodReturn) =
      getReturnType(funcDecl.json(ParserKeys.Type)).headOption
        .getOrElse(("", methodReturnNode(funcDecl, Defines.voidTypeName)))
    val templateParams       = ""
    val params               = funcDecl.json(ParserKeys.Type)(ParserKeys.Params)(ParserKeys.List)
    val genericTypeMethodMap = processTypeParams(funcDecl.json(ParserKeys.Type))
    val signature =
      s"$fullname$templateParams(${parameterSignature(params, genericTypeMethodMap)})$returnTypeStr"
    GoGlobal.recordFullNameToReturnType(fullname, returnTypeStr, Some(signature))
    val methodNode_ = methodNode(funcDecl, name, funcDecl.code, fullname, Some(signature), filename)
    methodAstParentStack.push(methodNode_)
    scope.pushNewScope(methodNode_)
    val astForMethod =
      methodAst(
        methodNode_,
        astForMethodParameter(params, genericTypeMethodMap),
        astForMethodBody(funcDecl.json(ParserKeys.Body)),
        methodReturn
      )
    scope.popScope()
    methodAstParentStack.pop()
    val typeDeclAst = createFunctionTypeAndTypeDecl(funcDecl, methodNode_, name, fullname, signature)
    Seq(astForMethod.merge(typeDeclAst))
  }

  private def astForMethodParameter(params: Value, genericTypeMethodMap: Map[String, List[String]]): Seq[Ast] = {
    var index = 1
    params.arrOpt
      .getOrElse(List())
      .flatMap(x =>
        val typeInfo = createParserNodeInfo(x(ParserKeys.Type))
        val (typeFullName, typeFullNameForcode, isVariadic, evaluationStrategy) =
          processTypeInfo(typeInfo, genericTypeMethodMap)
        x(ParserKeys.Names).arrOpt
          .getOrElse(List())
          .map(y => {
            // We are returning same type from x object for each name in the names array.
            val parameterInfo = createParserNodeInfo(y)
            val paramName     = parameterInfo.json(ParserKeys.Name).str
            val parameterNode = parameterInNode(
              parameterInfo,
              paramName,
              s"${paramName} ${typeFullNameForcode}",
              index,
              isVariadic,
              evaluationStrategy,
              typeFullName
            )
            index += 1
            scope.addToScope(paramName, (parameterNode, typeFullName))
            Ast(parameterNode)
          })
      )
      .toSeq
  }

  private def parameterSignature(params: Value, genericTypeMethodMap: Map[String, List[String]]): String = {
    //    func foo(argc, something int, argv string) int {
    // We get params -> list -> names [argc, something], type (int)
    params.arrOpt
      .getOrElse(ArrayBuffer())
      .map(x =>
        val typeInfo                                           = createParserNodeInfo(x(ParserKeys.Type))
        val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(typeInfo, genericTypeMethodMap)
        x(ParserKeys.Names).arrOpt
          .getOrElse(List())
          .map(_ => {
            // We are returning same type from x object for each name in the names array.
            typeFullName
          })
          .mkString(", ")
      )
      .mkString(", ")
  }

  private def getReturnType(methodTypes: Value): Seq[(String, NewMethodReturn)] = {
    Try(methodTypes(ParserKeys.Results)) match
      case Success(returnType) =>
        returnType(ParserKeys.List).arrOpt
          .getOrElse(List())
          .map(x =>
            val typeInfo                                           = createParserNodeInfo(x(ParserKeys.Type))
            val (typeFullName, typeFullNameForcode, isVariadic, _) = processTypeInfo(typeInfo)
            (typeFullName, methodReturnNode(typeInfo, typeFullName))
          )
          .toSeq
      case Failure(exception) => Seq.empty
  }

  def astForMethodBody(body: Value): Ast = {

    val nodeInfo = createParserNodeInfo(body)
    nodeInfo.node match {
      case BlockStmt => astForBlockStatement(nodeInfo)
      case _         => Ast()
    }
  }

  private def processTypeParams(funDecl: Value): Map[String, List[String]] = {
    val genericTypeMethodMap = new mutable.HashMap[String, List[String]]()
    Try(funDecl(ParserKeys.TypeParams)) match {
      case Success(typeParams) =>
        if (
          typeParams.obj.contains(ParserKeys.List) && typeParams(ParserKeys.List).arr.headOption.get.obj
            .contains(ParserKeys.Type)
        ) {
          val genericTypeName = typeParams(ParserKeys.List).arr.headOption.get
            .obj(ParserKeys.Names)
            .arr
            .headOption
            .get
            .obj(ParserKeys.Name)
            .str
          typeParams(ParserKeys.List).arr.foreach(typeDecl => {
            val genericMethodList = ListBuffer[String]()
            processGenericType(typeDecl(ParserKeys.Type), genericMethodList)
            genericTypeMethodMap.put(genericTypeName, genericMethodList.toList)
          })
        }
      case _ =>
        Map()
    }
    genericTypeMethodMap.toMap
  }

  private def processGenericType(typeDecl: Value, genericTypeList: ListBuffer[String]): Unit = {
    if (typeDecl.obj.contains(ParserKeys.Name)) {
      genericTypeList.addOne(typeDecl(ParserKeys.Name).str)
    } else if (typeDecl.obj.contains(ParserKeys.Op)) {
      processGenericType(typeDecl(ParserKeys.X), genericTypeList)
      processGenericType(typeDecl(ParserKeys.Y), genericTypeList)
    }
  }
}
