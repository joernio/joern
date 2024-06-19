package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.parser.ParserKeys
import io.joern.x2cpg.ValidationMode
import ujson.{Arr, Obj, Value}

trait DepdencySrcProcessor(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def buildCacheFromDepSrc(): Unit = {
    try {
      identifyAndRecordPackagesWithDifferentName()
      findAndProcess(parserResult.json)
      // NOTE: For dependencies we are just caching the global variables Types.
      processPackageLevelGolbalVaraiblesAndConstants(parserResult.json)
    } catch {
      case ex: Exception =>
        logger.warn(s"Error: While processing dependency source- ${parserResult.fullPath}", ex)
    }
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
            .contains(ParserKeys.NodeType) && (obj(ParserKeys.NodeType).str == "ast.ValueSpec" || obj(
            ParserKeys.NodeType
          ).str == "ast.FuncLit") && !json.obj.contains(ParserKeys.NodeReferenceId)
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
