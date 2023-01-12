package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines

trait TypeHelper { this: AstCreator =>

  private val TYPE_ANNOTATION_KEY = "typeAnnotation"
  private val RETURN_TYPE_KEY     = "returnType"

  private val TYPE_REPLACEMENTS = Map(
    " any"     -> s" ${Defines.ANY}",
    " number"  -> s" ${Defines.NUMBER}",
    " null"    -> s" ${Defines.NULL}",
    " string"  -> s" ${Defines.STRING}",
    " boolean" -> s" ${Defines.BOOLEAN}",
    "typeof "  -> ""
  )

  protected def isPlainTypeAlias(alias: BabelNodeInfo): Boolean = if (hasKey(alias.json, "right")) {
    createBabelNodeInfo(alias.json("right")).node.toString == TSTypeReference.toString
  } else {
    createBabelNodeInfo(alias.json("typeAnnotation")).node.toString == TSTypeReference.toString
  }

  private def typeForFlowType(flowType: BabelNodeInfo): String = flowType.node match {
    case BooleanTypeAnnotation        => Defines.BOOLEAN
    case NumberTypeAnnotation         => Defines.NUMBER
    case ObjectTypeAnnotation         => Defines.OBJECT
    case StringTypeAnnotation         => Defines.STRING
    case SymbolTypeAnnotation         => Defines.SYMBOL
    case NumberLiteralTypeAnnotation  => code(flowType.json)
    case ArrayTypeAnnotation          => code(flowType.json)
    case BooleanLiteralTypeAnnotation => code(flowType.json)
    case NullLiteralTypeAnnotation    => code(flowType.json)
    case StringLiteralTypeAnnotation  => code(flowType.json)
    case GenericTypeAnnotation        => code(flowType.json("id"))
    case ThisTypeAnnotation           => dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY)
    case NullableTypeAnnotation       => typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TYPE_ANNOTATION_KEY)))
    case _                            => Defines.ANY
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = tsType.node match {
    case TSBooleanKeyword    => Defines.BOOLEAN
    case TSBigIntKeyword     => Defines.NUMBER
    case TSNullKeyword       => Defines.NULL
    case TSNumberKeyword     => Defines.NUMBER
    case TSObjectKeyword     => Defines.OBJECT
    case TSStringKeyword     => Defines.STRING
    case TSSymbolKeyword     => Defines.SYMBOL
    case TSIntrinsicKeyword  => code(tsType.json)
    case TSTypeReference     => code(tsType.json)
    case TSArrayType         => code(tsType.json)
    case TSThisType          => dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY)
    case TSOptionalType      => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSRestType          => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSParenthesizedType => typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case _                   => Defines.ANY
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNodeInfo): String = typeAnnotation.node match {
    case TypeAnnotation   => typeForFlowType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case TSTypeAnnotation => typeForTsType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case _: FlowType      => typeForFlowType(createBabelNodeInfo(typeAnnotation.json))
    case _: TSType        => typeForTsType(createBabelNodeInfo(typeAnnotation.json))
    case _                => Defines.ANY
  }

  private def isStringType(tpe: String): Boolean =
    tpe.startsWith("\"") && tpe.endsWith("\"")

  private def isNumberType(tpe: String): Boolean =
    tpe.toDoubleOption.isDefined

  private def typeFromTypeMap(node: BabelNodeInfo): String =
    pos(node.json).flatMap(parserResult.typeMap.get) match {
      case Some(value) if value.isEmpty       => Defines.STRING
      case Some(value) if value == "string"   => Defines.STRING
      case Some(value) if isStringType(value) => Defines.STRING
      case Some(value) if value == "number"   => Defines.NUMBER
      case Some(value) if isNumberType(value) => Defines.NUMBER
      case Some(value) if value == "null"     => Defines.NULL
      case Some(value) if value == "boolean"  => Defines.BOOLEAN
      case Some(value) if value == "any"      => Defines.ANY
      case Some(other) =>
        TYPE_REPLACEMENTS.foldLeft(other) { case (typeStr, (m, r)) =>
          typeStr.replace(m, r)
        }
      case None => Defines.ANY
    }

  protected def typeFor(node: BabelNodeInfo): String = {
    val tpe = Seq(TYPE_ANNOTATION_KEY, RETURN_TYPE_KEY).find(hasKey(node.json, _)) match {
      case Some(key) => typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
      case None      => typeFromTypeMap(node)
    }
    registerType(tpe, tpe)
    tpe
  }

}
