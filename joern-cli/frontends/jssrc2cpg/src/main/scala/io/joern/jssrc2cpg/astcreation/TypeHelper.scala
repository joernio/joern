package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines

import java.util.regex.Pattern

trait TypeHelper { this: AstCreator =>

  private val TypeAnnotationKey = "typeAnnotation"
  private val ReturnTypeKey     = "returnType"
  private val ImportMatcher     = Pattern.compile("(typeof )?import\\([\"'](.*)[\"']\\)")

  private val TypeReplacements = Map(
    " any"       -> s" ${Defines.Any}",
    " unknown"   -> s" ${Defines.Unknown}",
    " void"      -> s" ${Defines.Void}",
    " never"     -> s" ${Defines.Never}",
    " undefined" -> s" ${Defines.Undefined}",
    " number"    -> s" ${Defines.Number}",
    " null"      -> s" ${Defines.Null}",
    " string"    -> s" ${Defines.String}",
    " boolean"   -> s" ${Defines.Boolean}",
    " bigint"    -> s" ${Defines.BigInt}",
    "{}"         -> Defines.Object,
    "typeof "    -> ""
  )

  protected def isPlainTypeAlias(alias: BabelNodeInfo): Boolean = if (hasKey(alias.json, "right")) {
    createBabelNodeInfo(alias.json("right")).node.toString == TSTypeReference.toString
  } else {
    createBabelNodeInfo(alias.json("typeAnnotation")).node.toString == TSTypeReference.toString
  }

  private def typeForFlowType(flowType: BabelNodeInfo): String = flowType.node match {
    case BooleanTypeAnnotation        => Defines.Boolean
    case NumberTypeAnnotation         => Defines.Number
    case ObjectTypeAnnotation         => Defines.Object
    case StringTypeAnnotation         => Defines.String
    case SymbolTypeAnnotation         => Defines.Symbol
    case NumberLiteralTypeAnnotation  => code(flowType.json)
    case ArrayTypeAnnotation          => Defines.Array
    case BooleanLiteralTypeAnnotation => code(flowType.json)
    case NullLiteralTypeAnnotation    => code(flowType.json)
    case StringLiteralTypeAnnotation  => code(flowType.json)
    case GenericTypeAnnotation =>
      val idCode = code(flowType.json("id"))
      if (isNumberType(idCode)) Defines.Number
      else if (isStringType(idCode)) Defines.String
      else idCode
    case ThisTypeAnnotation     => typeHintForThisExpression(Option(flowType)).headOption.getOrElse(Defines.Any);
    case NullableTypeAnnotation => typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TypeAnnotationKey)))
    case _                      => Defines.Any
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = tsType.node match {
    case TSBooleanKeyword   => Defines.Boolean
    case TSBigIntKeyword    => Defines.Number
    case TSNullKeyword      => Defines.Null
    case TSNumberKeyword    => Defines.Number
    case TSObjectKeyword    => Defines.Object
    case TSStringKeyword    => Defines.String
    case TSSymbolKeyword    => Defines.Symbol
    case TSUnknownKeyword   => Defines.Unknown
    case TSVoidKeyword      => Defines.Void
    case TSUndefinedKeyword => Defines.Undefined
    case TSNeverKeyword     => Defines.Never
    case TSIntrinsicKeyword => code(tsType.json)
    case TSTypeReference =>
      val refCode = code(tsType.json)
      if (isNumberType(refCode)) Defines.Number
      else if (isStringType(refCode)) Defines.String
      else refCode
    case TSArrayType         => Defines.Array
    case TSThisType          => typeHintForThisExpression(Option(tsType)).headOption.getOrElse(Defines.Any)
    case TSOptionalType      => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TypeAnnotationKey)))
    case TSRestType          => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TypeAnnotationKey)))
    case TSParenthesizedType => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TypeAnnotationKey)))
    case _                   => Defines.Any
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNodeInfo): String = typeAnnotation.node match {
    case TypeAnnotation   => typeForFlowType(createBabelNodeInfo(typeAnnotation.json(TypeAnnotationKey)))
    case TSTypeAnnotation => typeForTsType(createBabelNodeInfo(typeAnnotation.json(TypeAnnotationKey)))
    case _: FlowType      => typeForFlowType(createBabelNodeInfo(typeAnnotation.json))
    case _: TSType        => typeForTsType(createBabelNodeInfo(typeAnnotation.json))
    case _                => Defines.Any
  }

  private def isStringType(tpe: String): Boolean =
    tpe.isEmpty || tpe == "string" || tpe.startsWith("\"") && tpe.endsWith("\"")

  private def isNumberType(tpe: String): Boolean =
    tpe == "number" || tpe == "int" || tpe.toDoubleOption.isDefined

  private def typeFromTypeMap(node: BabelNodeInfo): String =
    pos(node.json).flatMap(parserResult.typeMap.get) match {
      case Some(value) if isStringType(value)                    => Defines.String
      case Some(value) if isNumberType(value)                    => Defines.Number
      case Some(value) if value == "null"                        => Defines.Null
      case Some(value) if value == "boolean"                     => Defines.Boolean
      case Some(value) if value == "any"                         => Defines.Any
      case Some(value) if ImportMatcher.matcher(value).matches() => importToModule(value)
      case Some(value)                                           => value
      case None                                                  => Defines.Any
    }

  private def importToModule(value: String): String = {
    val matcher = ImportMatcher.matcher(value)
    this.rootTypeDecl.headOption match {
      case Some(typeDecl)            => typeDecl.fullName
      case None if matcher.matches() => matcher.group(2).stripSuffix(".js").concat(s".js:${Defines.Program}")
      case None                      => value
    }
  }

  protected def typeFor(node: BabelNodeInfo): String = {
    val tpe = Seq(TypeAnnotationKey, ReturnTypeKey).find(hasKey(node.json, _)) match {
      case Some(key) => typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
      case None      => typeFromTypeMap(node)
    }
    val tpeWithTypeReplacements = TypeReplacements.foldLeft(tpe) { case (typeStr, (m, r)) => typeStr.replace(m, r) }
    val tpeWithArrayReplacements = if (tpeWithTypeReplacements.endsWith("[]")) {
      Defines.Array
    } else {
      tpeWithTypeReplacements.replaceAll(":[^,.]+\\[]", s": ${Defines.Array}")
    }
    if (!tpeWithArrayReplacements.contains("{") && !tpe.contains("(")) {
      registerType(tpeWithArrayReplacements)
    }
    tpeWithArrayReplacements
  }

  protected def typeHintForThisExpression(node: Option[BabelNodeInfo] = None): Seq[String] = {
    dynamicInstanceTypeStack.headOption match {
      case Some(tpe) => Seq(tpe)
      case None if node.isDefined =>
        typeFor(node.get) match {
          case tpe if tpe != Defines.Any && tpe != "this" => if (Defines.isBuiltinType(tpe)) Seq(tpe) else Seq.empty
          case _                                          => rootTypeDecl.map(_.fullName).toSeq
        }
      case None => rootTypeDecl.map(_.fullName).toSeq
    }
  }

}
