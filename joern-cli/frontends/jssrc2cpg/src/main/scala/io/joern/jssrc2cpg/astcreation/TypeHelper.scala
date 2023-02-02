package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines

trait TypeHelper { this: AstCreator =>

  private val TypeAnnotationKey = "typeAnnotation"
  private val ReturnTypeKey     = "returnType"

  private val TypeReplacements = Map(
    " any"     -> s" ${Defines.Any}",
    " number"  -> s" ${Defines.Number}",
    " null"    -> s" ${Defines.Null}",
    " string"  -> s" ${Defines.String}",
    " boolean" -> s" ${Defines.Boolean}",
    "{}"       -> Defines.Object,
    "typeof "  -> ""
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
    case ArrayTypeAnnotation          => code(flowType.json)
    case BooleanLiteralTypeAnnotation => code(flowType.json)
    case NullLiteralTypeAnnotation    => code(flowType.json)
    case StringLiteralTypeAnnotation  => code(flowType.json)
    case GenericTypeAnnotation        => code(flowType.json("id"))
    case ThisTypeAnnotation           => dynamicInstanceTypeStack.headOption.getOrElse(Defines.Any)
    case NullableTypeAnnotation       => typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TypeAnnotationKey)))
    case _                            => Defines.Any
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = tsType.node match {
    case TSBooleanKeyword    => Defines.Boolean
    case TSBigIntKeyword     => Defines.Number
    case TSNullKeyword       => Defines.Null
    case TSNumberKeyword     => Defines.Number
    case TSObjectKeyword     => Defines.Object
    case TSStringKeyword     => Defines.String
    case TSSymbolKeyword     => Defines.Symbol
    case TSIntrinsicKeyword  => code(tsType.json)
    case TSTypeReference     => code(tsType.json)
    case TSArrayType         => code(tsType.json)
    case TSThisType          => dynamicInstanceTypeStack.headOption.getOrElse(Defines.Any)
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
    tpe.startsWith("\"") && tpe.endsWith("\"")

  private def isNumberType(tpe: String): Boolean =
    tpe.toDoubleOption.isDefined

  private def typeFromTypeMap(node: BabelNodeInfo): String =
    pos(node.json).flatMap(parserResult.typeMap.get) match {
      case Some(value) if value.isEmpty       => Defines.String
      case Some(value) if value == "string"   => Defines.String
      case Some(value) if isStringType(value) => Defines.String
      case Some(value) if value == "number"   => Defines.Number
      case Some(value) if isNumberType(value) => Defines.Number
      case Some(value) if value == "null"     => Defines.Null
      case Some(value) if value == "boolean"  => Defines.Boolean
      case Some(value) if value == "any"      => Defines.Any
      case Some(other) =>
        TypeReplacements.foldLeft(other) { case (typeStr, (m, r)) =>
          typeStr.replace(m, r)
        }
      case None => Defines.Any
    }

  protected def typeFor(node: BabelNodeInfo): String = {
    val tpe = Seq(TypeAnnotationKey, ReturnTypeKey).find(hasKey(node.json, _)) match {
      case Some(key) => typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
      case None      => typeFromTypeMap(node)
    }
    registerType(tpe, tpe)
    tpe
  }

}
