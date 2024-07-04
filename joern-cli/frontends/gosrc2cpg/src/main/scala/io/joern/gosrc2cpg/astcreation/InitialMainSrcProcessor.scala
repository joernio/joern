package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.UtilityConstants.fileSeparateorPattern
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import ujson.{Arr, Obj, Value}

import java.io.File

trait InitialMainSrcProcessor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def buildCacheFromMainSrc(): DiffGraphBuilder = {
    try {
      if (goGlobal.recordForThisNamespace(fullyQualifiedPackage)) {
        // java.util.Set.Add method will return true when set already doesn't contain the same value.
        val rootNode = createParserNodeInfo(parserResult.json)
        val ast      = astForPackage(rootNode)
        Ast.storeInDiffGraph(ast, diffGraph)
      }
      identifyAndRecordPackagesWithDifferentName()
      findAndProcess(parserResult.json)
      // NOTE: For dependencies we are just caching the global variables Types.
      processPackageLevelGlobalVariablesAndConstants(parserResult.json)
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

  private def identifyUserPackagesFromImports(spec: Value): Unit = {
    val namespace = spec(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
    goMod
      .recordUsedDependencies(namespace)
    goGlobal.recordForThisNamespaceThroughImports(namespace)
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
          identifyUserPackagesFromImports(obj)
          processImports(obj)
        } else if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.TypeSpec" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          processTypeSepc(createParserNodeInfo(obj))
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
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.FuncLit" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          )
        ) {
          processFuncLiteral(obj)
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
}
