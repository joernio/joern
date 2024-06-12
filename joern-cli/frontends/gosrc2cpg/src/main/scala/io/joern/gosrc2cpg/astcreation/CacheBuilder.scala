package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.MethodCacheMetaData
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.gosrc2cpg.utils.UtilityConstants.fileSeparateorPattern
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import io.shiftleft.codepropertygraph.generated.nodes.NewNamespaceBlock
import ujson.{Arr, Obj, Value}

import java.io.File
import scala.util.Try

trait CacheBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  def buildCache(cpgOpt: Option[Cpg]): DiffGraphBuilder = {
    val diffGraph    = new DiffGraphBuilder
    val parserResult = init()
    try {
      if (checkIfGivenDependencyPackageCanBeProcessed()) {
        cpgOpt.map { _ =>
          // We don't want to process this part when third party dependencies are being processed.
          if (goGlobal.recordForThisNamespace(fullyQualifiedPackage)) {
            // java.util.Set.Add method will return true when set already doesn't contain the same value.
            val rootNode = createParserNodeInfo(parserResult.json)
            val ast      = astForPackage(rootNode)
            Ast.storeInDiffGraph(ast, diffGraph)
          }
        }
        identifyAndRecordPackagesWithDifferentName()
        findAndProcess(parserResult.json)
        // NOTE: For dependencies we are just caching the global variables Types.
        processPackageLevelGolbalVaraiblesAndConstants(parserResult.json)
      }
    } catch {
      case ex: Exception =>
        logger.warn(s"Error: While processing - ${parserResult.fullPath}", ex)
    }
    diffGraph
  }

  private def checkIfGivenDependencyPackageCanBeProcessed(): Boolean =
    !goGlobal.processingDependencies || goGlobal.processingDependencies && goGlobal.aliasToNameSpaceMapping
      .containsValue(fullyQualifiedPackage)

  private def identifyAndRecordPackagesWithDifferentName(): Unit = {
    // record the package to full namespace mapping only when declared package name is not matching with containing folder name
    if (declaredPackageName != fullyQualifiedPackage.split("/").last)
      goGlobal.recordAliasToNamespaceMapping(declaredPackageName, fullyQualifiedPackage)
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
          ) && !goGlobal.processingDependencies
        ) {
          // NOTE: Dependency code is not being processed here.
          processImports(obj, true)
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
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.ValueSpec" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          ) && !goGlobal.processingDependencies
        ) {
          // NOTE: Dependency code is not being processed here.
          createParserNodeInfo(obj)
        } else if (
          json.obj
            .contains(ParserKeys.NodeType) && obj(ParserKeys.NodeType).str == "ast.FuncLit" && !json.obj.contains(
            ParserKeys.NodeReferenceId
          ) && !goGlobal.processingDependencies
        ) {
          // NOTE: Dependency code is not being processed here.
          processFuncLiteral(obj)
        }

        obj.value.values.foreach(subJson => findAndProcess(subJson))
      case arr: Arr =>
        arr.value.foreach(subJson => findAndProcess(subJson))
      case _ =>
    }
  }

  private def processFuncLiteral(funcLit: Value): Unit = {
    val LambdaFunctionMetaData(signature, _, _, _, _) = generateLambdaSignature(
      createParserNodeInfo(funcLit(ParserKeys.Type))
    )
    goGlobal.recordForThisLamdbdaSignature(signature)
  }

  protected def processTypeSepc(typeSepc: ParserNodeInfo): (String, String, Seq[Ast]) = {
    val name = typeSepc.json(ParserKeys.Name)(ParserKeys.Name).str
    if (goGlobal.checkForDependencyFlags(name)) {
      // Ignoring recording the Type details when we are processing dependencies code with Type name starting with lower case letter
      // As the Types starting with lower case letters will only be accessible within that package. Which means
      // these Types are not going to get referred from main source code.
      val fullName = fullyQualifiedPackage + Defines.dot + name
      val typeNode = createParserNodeInfo(typeSepc.json(ParserKeys.Type))
      val ast = typeNode.node match {
        // As of don't see any use case where InterfaceType needs to be handled.
        case InterfaceType => Seq.empty
        // astForStructType() function will record the member types
        case StructType => astForStructType(typeNode, fullName)
        // Process lambda function types to record lambda function signature mapped to TypeFullName
        case FuncType => processFuncType(typeNode, fullName)
        case _        => Seq.empty
      }
      (name, fullName, ast)
    } else
      ("", "", Seq.empty)
  }

  protected def processImports(importDecl: Value, recordFindings: Boolean = false): (String, String) = {
    val importedEntity = importDecl(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
    if (recordFindings) {
      goMod.recordUsedDependencies(importedEntity)
    }
    val importedAsOption =
      Try(importDecl(ParserKeys.Name).obj(ParserKeys.Name).str).toOption
    importedAsOption match {
      case Some(importedAs) =>
        // As these alias could be different for each file. Hence we maintain the cache at file level.
        if (recordFindings)
          aliasToNameSpaceMapping.put(importedAs, importedEntity)
        (importedEntity, importedAs)
      case _ =>
        val derivedImportedAs = importedEntity.split("/").last
        if (recordFindings)
          goGlobal.recordAliasToNamespaceMapping(derivedImportedAs, importedEntity)
        (importedEntity, derivedImportedAs)
    }
  }

  protected def processFuncDecl(funcDeclVal: Value): MethodMetadata = {
    val name = funcDeclVal(ParserKeys.Name).obj(ParserKeys.Name).str
    if (goGlobal.checkForDependencyFlags(name)) {
      // Ignoring recording the method details when we are processing dependencies code with functions name starting with lower case letter
      // As the functions starting with lower case letters will only be accessible within that package. Which means
      // these methods / functions are not going to get referred from main source code.
      val receiverInfo = getReceiverInfo(Try(funcDeclVal(ParserKeys.Recv)))
      val (methodFullname, recordNamespace) = receiverInfo match
        case Some(_, typeFullName, _, _) =>
          (s"$typeFullName.$name", typeFullName)
        case _ =>
          (s"$fullyQualifiedPackage.$name", fullyQualifiedPackage)
      // TODO: handle multiple return type or tuple (int, int)
      val genericTypeMethodMap = processTypeParams(funcDeclVal(ParserKeys.Type))
      val (returnTypeStr, _) =
        getReturnType(funcDeclVal(ParserKeys.Type), genericTypeMethodMap).headOption
          .getOrElse((Defines.voidTypeName, null))
      val params = funcDeclVal(ParserKeys.Type)(ParserKeys.Params)(ParserKeys.List)
      val signature =
        s"$methodFullname(${parameterSignature(params, genericTypeMethodMap)})${
            if returnTypeStr == Defines.voidTypeName then "" else returnTypeStr
          }"
      goGlobal.recordMethodMetadata(recordNamespace, name, MethodCacheMetaData(returnTypeStr, signature))
      MethodMetadata(name, methodFullname, signature, params, receiverInfo, genericTypeMethodMap)
    } else
      MethodMetadata()
  }
}

case class MethodMetadata(
  name: String = "",
  methodFullname: String = "",
  signature: String = "",
  params: Value = Value("{}"),
  receiverInfo: Option[(String, String, String, ParserNodeInfo)] = None,
  genericTypeMethodMap: Map[String, List[String]] = Map()
)
