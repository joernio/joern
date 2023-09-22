package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import ujson.{Arr, Obj, Value}
import io.joern.x2cpg.Ast

import scala.util.Try

trait CacheBuilder { this: AstCreator =>

  def buildCache(): Unit = {
    try {
      findAndProcess(parserResult.json)
    } catch
      case ex: Exception =>
        logger.warn(s"Error: While processing - ${parserResult.fullPath}", ex)
  }

  private def findAndProcess(json: Value): Unit = {
    json match {
      case obj: Obj =>
        if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.ImportSpec" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          processImports(obj)
        } else if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.TypeSpec" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          createParserNodeInfo(obj)
        } else if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.FuncDecl" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          processFuncDecl(obj)
          createParserNodeInfo(obj)
        } else if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.ValueSpec" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          createParserNodeInfo(obj)
        }
        obj.value.values.foreach(subJson => findAndProcess(subJson))
      case arr: Arr =>
        arr.value.foreach(subJson => findAndProcess(subJson))
      case _ =>
    }
  }

  protected def processTypeSepc(typeSepc: Value): (String, String, Seq[Ast]) = {
    val name     = typeSepc(ParserKeys.Name)(ParserKeys.Name).str
    val fullName = fullyQualifiedPackage + Defines.dot + name
    val typeNode = createParserNodeInfo(typeSepc(ParserKeys.Type))
    // astForStructType() function will record the member types
    (name, fullName, astForStructType(typeNode, fullName))
  }

  protected def processImports(importDecl: Value): (String, String) = {
    val importedEntity = importDecl(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
    val importedAs =
      Try(importDecl(ParserKeys.Name).obj(ParserKeys.Name).str).toOption
        .getOrElse(importedEntity.split("/").last)

    aliasToNameSpaceMapping.put(importedAs, importedEntity)
    (importedEntity, importedAs)
  }

  protected def processFuncDecl(
    funcDeclVal: Value
  ): (String, String, String, Value, Option[(String, String, String, ParserNodeInfo)], Map[String, List[String]]) = {
    val name         = funcDeclVal(ParserKeys.Name).obj(ParserKeys.Name).str
    val receiverInfo = getReceiverInfo(Try(funcDeclVal(ParserKeys.Recv)))
    val methodFullname = receiverInfo match
      case Some(_, typeFullName, _, _) =>
        s"$typeFullName.$name"
      case _ =>
        s"$fullyQualifiedPackage.$name"
    // TODO: handle multiple return type or tuple (int, int)
    val genericTypeMethodMap = processTypeParams(funcDeclVal(ParserKeys.Type))
    val (returnTypeStr, _) =
      getReturnType(funcDeclVal(ParserKeys.Type), genericTypeMethodMap).headOption
        .getOrElse(("", null))
    val params = funcDeclVal(ParserKeys.Type)(ParserKeys.Params)(ParserKeys.List)
    val signature =
      s"$methodFullname(${parameterSignature(params, genericTypeMethodMap)})$returnTypeStr"
    GoGlobal.recordFullNameToReturnType(methodFullname, returnTypeStr, signature)
    (name, methodFullname, signature, params, receiverInfo, genericTypeMethodMap)
  }
}
