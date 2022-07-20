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
    case AnyTypeAnnotation            => Defines.ANY.label
    case ArrayTypeAnnotation          => code(flowType.json)
    case BooleanTypeAnnotation        => Defines.BOOLEAN.label
    case BooleanLiteralTypeAnnotation => code(flowType.json)
    case NullLiteralTypeAnnotation    => code(flowType.json)
    case ExistsTypeAnnotation         => Defines.ANY.label
    case FunctionTypeAnnotation       => Defines.ANY.label
    case GenericTypeAnnotation        => code(flowType.json("id"))
    case InterfaceTypeAnnotation      => Defines.ANY.label
    case IntersectionTypeAnnotation   => Defines.ANY.label
    case MixedTypeAnnotation          => Defines.ANY.label
    case EmptyTypeAnnotation          => Defines.ANY.label
    case NullableTypeAnnotation =>
      typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TYPE_ANNOTATION_KEY)))
    case NumberLiteralTypeAnnotation => code(flowType.json)
    case NumberTypeAnnotation        => Defines.NUMBER.label
    case ObjectTypeAnnotation        => Defines.OBJECT.label
    case StringLiteralTypeAnnotation => code(flowType.json)
    case StringTypeAnnotation        => Defines.STRING.label
    case SymbolTypeAnnotation        => Defines.SYMBOL.label
    case ThisTypeAnnotation =>
      dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY.label)
    case TupleTypeAnnotation       => Defines.ANY.label
    case TypeofTypeAnnotation      => Defines.ANY.label
    case UnionTypeAnnotation       => Defines.ANY.label
    case VoidTypeAnnotation        => Defines.ANY.label
    case IndexedAccessType         => Defines.ANY.label
    case OptionalIndexedAccessType => Defines.ANY.label
    case _ =>
      notHandledYet(flowType, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY.label
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = tsType.node match {
    case TSAnyKeyword       => Defines.ANY.label
    case TSBooleanKeyword   => Defines.BOOLEAN.label
    case TSBigIntKeyword    => Defines.NUMBER.label
    case TSIntrinsicKeyword => code(tsType.json)
    case TSNeverKeyword     => Defines.ANY.label
    case TSNullKeyword      => Defines.NULL.label
    case TSNumberKeyword    => Defines.NUMBER.label
    case TSObjectKeyword    => Defines.OBJECT.label
    case TSStringKeyword    => Defines.STRING.label
    case TSSymbolKeyword    => Defines.SYMBOL.label
    case TSUndefinedKeyword => Defines.ANY.label
    case TSUnknownKeyword   => Defines.ANY.label
    case TSVoidKeyword      => Defines.ANY.label
    case TSThisType =>
      dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY.label)
    case TSFunctionType    => Defines.ANY.label
    case TSConstructorType => Defines.ANY.label
    case TSTypeReference   => code(tsType.json)
    case TSTypePredicate   => Defines.ANY.label
    case TSTypeQuery       => Defines.ANY.label
    case TSTypeLiteral     => Defines.ANY.label
    case TSArrayType       => code(tsType.json)
    case TSTupleType       => Defines.ANY.label
    case TSOptionalType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSRestType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSUnionType        => Defines.ANY.label
    case TSIntersectionType => Defines.ANY.label
    case TSConditionalType  => Defines.ANY.label
    case TSInferType        => Defines.ANY.label
    case TSParenthesizedType =>
      typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
    case TSTypeOperator                => Defines.ANY.label
    case TSIndexedAccessType           => Defines.ANY.label
    case TSMappedType                  => Defines.ANY.label
    case TSLiteralType                 => Defines.ANY.label
    case TSExpressionWithTypeArguments => Defines.ANY.label
    case TSImportType                  => Defines.ANY.label
    case _ =>
      notHandledYet(tsType, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY.label
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNodeInfo): String = typeAnnotation.node match {
    case TypeAnnotation   => typeForFlowType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case TSTypeAnnotation => typeForTsType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
    case _: FlowType      => typeForFlowType(createBabelNodeInfo(typeAnnotation.json))
    case _: TSType        => typeForTsType(createBabelNodeInfo(typeAnnotation.json))
    case _ =>
      notHandledYet(typeAnnotation, TYPE_NOT_HANDLED_TEXT)
      Defines.ANY.label
  }

  protected def typeFor(node: BabelNodeInfo): String =
    Seq(TYPE_ANNOTATION_KEY, RETURN_TYPE_KEY).find(hasKey(node.json, _)) match {
      case Some(key) =>
        val tpe = typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
        registerType(tpe, tpe)
        tpe
      case None =>
        Defines.ANY.label
    }

}
