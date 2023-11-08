package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.parser.ParserAst.{GenDecl, ValueSpec}
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.UtilityConstants.fileSeparateorPattern
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import ujson.{Arr, Obj, Value}

import java.io.File
import scala.util.Try

trait CacheBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def buildCache(cpgOpt: Option[Cpg]): DiffGraphBuilder = {
    val diffGraph = new DiffGraphBuilder
    try {

      cpgOpt.map { _ =>
        // We don't want to process this part when third party dependencies are being processed.
        val result = GoGlobal.recordAliasToNamespaceMapping(declaredPackageName, fullyQualifiedPackage)
        if (result == null) {
          // if result is null that means item got added first time otherwise it has been already added to global map
          val rootNode = createParserNodeInfo(parserResult.json)
          val ast      = astForPackage(rootNode)
          Ast.storeInDiffGraph(ast, diffGraph)
        }
      }
      findAndProcess(parserResult.json)
      processPackageLevelGolbalVaraiblesAndConstants(parserResult.json)
    } catch {
      case ex: Exception =>
        logger.warn(s"Error: While processing - ${parserResult.fullPath}", ex)
    }
    diffGraph
  }

  private def astForPackage(rootNode: ParserNodeInfo): Ast = {
    val pathTokens = relPathFileName.split(fileSeparateorPattern)
    val packageFolderPath = if (pathTokens.nonEmpty && pathTokens.size > 1) {
      s"${File.separator}${pathTokens.dropRight(1).mkString(File.separator)}"
    } else {
      s"${File.separator}"
    }

    val namespaceBlock = NewNamespaceBlock()
      .name(fullyQualifiedPackage)
      .fullName(fullyQualifiedPackage)
      .filename(packageFolderPath)
    val fakePackageTypeDecl =
      typeDeclNode(rootNode, fullyQualifiedPackage, fullyQualifiedPackage, packageFolderPath, fullyQualifiedPackage)
    Ast(namespaceBlock).withChild(Ast(fakePackageTypeDecl))
  }

  private def processPackageLevelGolbalVaraiblesAndConstants(json: Value): Unit = {
    json(ParserKeys.Decls).arrOpt
      .getOrElse(List())
      .map(createParserNodeInfo)
      .foreach(decl => {
        decl.node match
          case GenDecl =>
            decl
              .json(ParserKeys.Specs)
              .arrOpt
              .getOrElse(List())
              .map(createParserNodeInfo)
              .foreach(spec => {
                spec.node match
                  case ValueSpec => astForValueSpec(spec, true)
                  case _         =>
                  // Only process ValueSpec
              })
          case _ =>
          // Only process GenDecl
      })
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
          processTypeSepc(obj)
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
