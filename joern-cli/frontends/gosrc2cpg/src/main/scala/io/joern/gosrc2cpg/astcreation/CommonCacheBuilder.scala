package io.joern.gosrc2cpg.astcreation

import io.joern.gosrc2cpg.datastructures.MethodCacheMetaData
import io.joern.gosrc2cpg.parser.ParserAst.*
import io.joern.gosrc2cpg.parser.{ParserKeys, ParserNodeInfo}
import io.joern.x2cpg.{Ast, ValidationMode}
import ujson.Value

import scala.util.{Success, Try}

trait CommonCacheBuilder(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def identifyAndRecordPackagesWithDifferentName(): Unit = {
    // record the package to full namespace mapping only when declared package name is not matching with containing folder name
    if (declaredPackageName != fullyQualifiedPackage.split("/").last)
      goGlobal.recordAliasToNamespaceMapping(declaredPackageName, fullyQualifiedPackage)
  }

  protected def processPackageLevelGlobalVariablesAndConstants(json: Value): Unit = {
    json(ParserKeys.Decls).arrOpt
      .getOrElse(List())
      .map(createParserNodeInfo)
      .foreach { decl =>
        decl.node match {
          case GenDecl =>
            decl
              .json(ParserKeys.Specs)
              .arrOpt
              .getOrElse(List())
              .map(createParserNodeInfo)
              .foreach { spec =>
                spec.node match {
                  case ValueSpec => astForValueSpec(spec, true)
                  case _         =>
                  // Only process ValueSpec
                }
              }
          case _ =>
          // Only process GenDecl
        }
      }
  }

  protected def processFuncLiteral(funcLit: Value): Unit = {
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
        case InterfaceType => processInterfaceType(typeNode, fullName)
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

  protected def processFuncDecl(funcDeclVal: Value): MethodMetadata = {
    val name = funcDeclVal(ParserKeys.Name).obj(ParserKeys.Name).str
    if (goGlobal.checkForDependencyFlags(name)) {
      // Ignoring recording the method details when we are processing dependencies code with functions name starting with lower case letter
      // As the functions starting with lower case letters will only be accessible within that package. Which means
      // these methods / functions are not going to get referred from main source code.
      val receiverInfo = getReceiverInfo(Try(funcDeclVal(ParserKeys.Recv)))
      val (methodFullname, recordNamespace) = receiverInfo match {
        case Some(_, typeFullName, _, _) =>
          (s"$typeFullName.$name", typeFullName)
        case _ =>
          (s"$fullyQualifiedPackage.$name", fullyQualifiedPackage)
      }
      // TODO: handle multiple return type or tuple (int, int)
      val genericTypeMethodMap = processTypeParams(funcDeclVal(ParserKeys.Type))
      val (returnTypeStr, _) =
        getReturnType(funcDeclVal(ParserKeys.Type), genericTypeMethodMap).headOption
          .getOrElse((Defines.voidTypeName, null))
      val params = funcDeclVal(ParserKeys.Type)(ParserKeys.Params)(ParserKeys.List)
      val signature =
        s"$methodFullname(${parameterSignature(params, genericTypeMethodMap)})${
            if (returnTypeStr == Defines.voidTypeName) "" else returnTypeStr
          }"
      goGlobal.recordMethodMetadata(recordNamespace, name, MethodCacheMetaData(returnTypeStr, signature))
      MethodMetadata(name, methodFullname, signature, params, receiverInfo, genericTypeMethodMap)
    } else
      MethodMetadata()
  }

  private def processInterfaceType(typeNode: ParserNodeInfo, fullName: String): Seq[Ast] = {
    val methodFields = Try(typeNode.json(ParserKeys.Methods)(ParserKeys.List))
      .orElse(Try(typeNode.json(ParserKeys.Fields)(ParserKeys.List)))
    methodFields.toOption.foreach { fields =>
      fields.arr.foreach { field =>
        Try(field(ParserKeys.Names).arr.head(ParserKeys.Name).str).toOption.foreach { name =>
          val methodTypeNode = createParserNodeInfo(field(ParserKeys.Type))
          val returnTypes = getReturnType(methodTypeNode.json, Map.empty)
          val returnTypeStr = returnTypes match {
            case Seq()    => Defines.voidTypeName
            case Seq(one) => one._1
            case multiple => s"(${multiple.map(_._1).mkString(", ")})"
          }
          val params = Try(methodTypeNode.json(ParserKeys.Params)(ParserKeys.List)).getOrElse(ujson.Arr())
          val sig = parameterSignature(params, Map.empty)
          val methodFullName = s"$fullName.$name"
          val signature = s"$methodFullName($sig)${if (returnTypeStr == Defines.voidTypeName) "" else returnTypeStr}"
          goGlobal.recordMethodMetadata(fullName, name, MethodCacheMetaData(returnTypeStr, signature))
          goGlobal.recordInterfaceMethods(fullName, name)
        }
      }
    }
    Seq.empty
  }

  protected def processImports(importDecl: Value): (String, String) = {
    val importedEntity = importDecl(ParserKeys.Path).obj(ParserKeys.Value).str.replaceAll("\"", "")
    val importedAsOption =
      Try(importDecl(ParserKeys.Name).obj(ParserKeys.Name).str).toOption
    importedAsOption match {
      case Some(importedAs) =>
        // As these alias could be different for each file. Hence we maintain the cache at file level.
        aliasToNameSpaceMapping.put(importedAs, importedEntity)
        (importedEntity, importedAs)
      case _ =>
        val derivedImportedAs = importedEntity.split("/").last
        aliasToNameSpaceMapping.put(derivedImportedAs, importedEntity)
        (importedEntity, derivedImportedAs)
    }
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
