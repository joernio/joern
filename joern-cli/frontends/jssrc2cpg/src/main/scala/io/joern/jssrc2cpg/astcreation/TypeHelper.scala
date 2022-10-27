package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelAst._
import io.joern.jssrc2cpg.parser.BabelNodeInfo
import io.joern.jssrc2cpg.passes.Defines

trait TypeHelper { this: AstCreator =>

  private val TYPE_ANNOTATION_KEY   = "typeAnnotation"
  private val RETURN_TYPE_KEY       = "returnType"
  private val TYPE_NOT_HANDLED_TEXT = "Type calculation"

  protected def isPlainTypeAlias(alias: BabelNodeInfo): Boolean = if (hasKey(alias.json, "right")) {
    createBabelNodeInfo(alias.json("right")).node.toString == TSTypeReference.toString
  } else {
    createBabelNodeInfo(alias.json("typeAnnotation")).node.toString == TSTypeReference.toString
  }

  private def typeForFlowType(flowType: BabelNodeInfo): String = flowType.node match {
    case AnyTypeAnnotation            => Defines.ANY
    case ArrayTypeAnnotation          => code(flowType.json)
    case BooleanTypeAnnotation        => Defines.BOOLEAN
    case BooleanLiteralTypeAnnotation => code(flowType.json)
    case NullLiteralTypeAnnotation    => code(flowType.json)
    case ExistsTypeAnnotation         => Defines.ANY
    case FunctionTypeAnnotation       => Defines.ANY
    case GenericTypeAnnotation        => code(flowType.json("id"))
    case InterfaceTypeAnnotation      => Defines.ANY
    case IntersectionTypeAnnotation   => Defines.ANY
    case MixedTypeAnnotation          => Defines.ANY
    case EmptyTypeAnnotation          => Defines.ANY
    case NullableTypeAnnotation =>
      typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TYPE_ANNOTATION_KEY)))
    case NumberLiteralTypeAnnotation => code(flowType.json)
    case NumberTypeAnnotation        => Defines.NUMBER
    case ObjectTypeAnnotation        => Defines.OBJECT
    case StringLiteralTypeAnnotation => code(flowType.json)
    case StringTypeAnnotation        => Defines.STRING
    case SymbolTypeAnnotation        => Defines.SYMBOL
    case ThisTypeAnnotation =>
      dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY)
    case TupleTypeAnnotation       => Defines.ANY
    case TypeofTypeAnnotation      => Defines.ANY
    case UnionTypeAnnotation       => Defines.ANY
    case VoidTypeAnnotation        => Defines.ANY
    case IndexedAccessType         => Defines.ANY
    case OptionalIndexedAccessType => Defines.ANY
    case _ =>
      notHandledYet(flowType, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = tsType.node match {
    case TSAnyKeyword       => Defines.ANY
    case TSBooleanKeyword   => Defines.BOOLEAN
    case TSBigIntKeyword    => Defines.NUMBER
    case TSIntrinsicKeyword => code(tsType.json)
    case TSNeverKeyword     => Defines.ANY
    case TSNullKeyword      => Defines.NULL
    case TSNumberKeyword    => Defines.NUMBER
    case TSObjectKeyword    => Defines.OBJECT
    case TSStringKeyword    => Defines.STRING
    case TSSymbolKeyword    => Defines.SYMBOL
    case TSUndefinedKeyword => Defines.ANY
    case TSUnknownKeyword   => Defines.ANY
    case TSVoidKeyword      => Defines.ANY
    case TSThisType =>
      dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY)
    case TSFunctionType    => Defines.ANY
    case TSConstructorType => Defines.ANY
    case TSTypeReference   => code(tsType.json)
    case TSTypePredicate   => Defines.ANY
    case TSTypeQuery       => Defines.ANY
    case TSTypeLiteral     => Defines.ANY
    case TSArrayType       => code(tsType.json)
    case TSTupleType       => Defines.ANY
    case TSOptionalType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSRestType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSUnionType        => Defines.ANY
    case TSIntersectionType => Defines.ANY
    case TSConditionalType  => Defines.ANY
    case TSInferType        => Defines.ANY
    case TSParenthesizedType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSTypeOperator                => Defines.ANY
    case TSIndexedAccessType           => Defines.ANY
    case TSMappedType                  => Defines.ANY
    case TSLiteralType                 => Defines.ANY
    case TSExpressionWithTypeArguments => Defines.ANY
    case TSImportType                  => Defines.ANY
    case _ =>
      notHandledYet(tsType, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNodeInfo): String = typeAnnotation.node match {
    case TypeAnnotation   => typeForFlowType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case TSTypeAnnotation => typeForTsType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case _: FlowType      => typeForFlowType(createBabelNodeInfo(typeAnnotation.json))
    case _: TSType        => typeForTsType(createBabelNodeInfo(typeAnnotation.json))
    case _ =>
      notHandledYet(typeAnnotation, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY
  }

  protected def typeFor(node: BabelNodeInfo): String =
    Seq(TYPE_ANNOTATION_KEY, RETURN_TYPE_KEY).find(hasKey(node.json, _)) match {
      case Some(key) =>
        val tpe = typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
        registerType(tpe, tpe)
        tpe
      case None =>
        Defines.ANY
    }

}
