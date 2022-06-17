package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.{BabelAst, BabelNodeInfo}
import io.joern.jssrc2cpg.passes.Defines

trait TypeHelper {
  this: AstCreator =>

  private val TYPE_ANNOTATION_KEY   = "typeAnnotation"
  private val RETURN_TYPE_KEY       = "returnType"
  private val TYPE_NOT_HANDLED_TEXT = "Type calculation"

  protected def isPlainTypeAlias(alias: BabelNodeInfo): Boolean = {
    if (hasKey(alias.json, "right")) {
      createBabelNodeInfo(alias.json("right")).node.toString == BabelAst.TSTypeReference.toString
    } else {
      createBabelNodeInfo(alias.json("typeAnnotation")).node.toString == BabelAst.TSTypeReference.toString
    }
  }

  private def typeForFlowType(flowType: BabelNodeInfo): String = {
    flowType.node match {
      case BabelAst.AnyTypeAnnotation            => Defines.ANY.label
      case BabelAst.ArrayTypeAnnotation          => code(flowType.json)
      case BabelAst.BooleanTypeAnnotation        => Defines.BOOLEAN.label
      case BabelAst.BooleanLiteralTypeAnnotation => code(flowType.json)
      case BabelAst.NullLiteralTypeAnnotation    => code(flowType.json)
      case BabelAst.ExistsTypeAnnotation         => Defines.ANY.label
      case BabelAst.FunctionTypeAnnotation       => Defines.ANY.label
      case BabelAst.GenericTypeAnnotation        => code(flowType.json("id"))
      case BabelAst.InterfaceTypeAnnotation      => Defines.ANY.label
      case BabelAst.IntersectionTypeAnnotation   => Defines.ANY.label
      case BabelAst.MixedTypeAnnotation          => Defines.ANY.label
      case BabelAst.EmptyTypeAnnotation          => Defines.ANY.label
      case BabelAst.NullableTypeAnnotation =>
        typeForTypeAnnotation(createBabelNodeInfo(flowType.json(TYPE_ANNOTATION_KEY)))
      case BabelAst.NumberLiteralTypeAnnotation => code(flowType.json)
      case BabelAst.NumberTypeAnnotation        => Defines.NUMBER.label
      case BabelAst.ObjectTypeAnnotation        => Defines.OBJECT.label
      case BabelAst.StringLiteralTypeAnnotation => code(flowType.json)
      case BabelAst.StringTypeAnnotation        => Defines.STRING.label
      case BabelAst.SymbolTypeAnnotation        => Defines.SYMBOL.label
      case BabelAst.ThisTypeAnnotation =>
        dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY.label)
      case BabelAst.TupleTypeAnnotation       => Defines.ANY.label
      case BabelAst.TypeofTypeAnnotation      => Defines.ANY.label
      case BabelAst.UnionTypeAnnotation       => Defines.ANY.label
      case BabelAst.VoidTypeAnnotation        => Defines.ANY.label
      case BabelAst.IndexedAccessType         => Defines.ANY.label
      case BabelAst.OptionalIndexedAccessType => Defines.ANY.label
      case _ =>
        notHandledYet(flowType, TYPE_NOT_HANDLED_TEXT)
        Defines.ANY.label
    }
  }

  private def typeForTsType(tsType: BabelNodeInfo): String = {
    tsType.node match {
      case BabelAst.TSAnyKeyword       => Defines.ANY.label
      case BabelAst.TSBooleanKeyword   => Defines.BOOLEAN.label
      case BabelAst.TSBigIntKeyword    => Defines.NUMBER.label
      case BabelAst.TSIntrinsicKeyword => code(tsType.json)
      case BabelAst.TSNeverKeyword     => Defines.ANY.label
      case BabelAst.TSNullKeyword      => Defines.NULL.label
      case BabelAst.TSNumberKeyword    => Defines.NUMBER.label
      case BabelAst.TSObjectKeyword    => Defines.OBJECT.label
      case BabelAst.TSStringKeyword    => Defines.STRING.label
      case BabelAst.TSSymbolKeyword    => Defines.SYMBOL.label
      case BabelAst.TSUndefinedKeyword => Defines.ANY.label
      case BabelAst.TSUnknownKeyword   => Defines.ANY.label
      case BabelAst.TSVoidKeyword      => Defines.ANY.label
      case BabelAst.TSThisType =>
        dynamicInstanceTypeStack.headOption.getOrElse(Defines.ANY.label)
      case BabelAst.TSFunctionType    => Defines.ANY.label
      case BabelAst.TSConstructorType => Defines.ANY.label
      case BabelAst.TSTypeReference   => code(tsType.json)
      case BabelAst.TSTypePredicate   => Defines.ANY.label
      case BabelAst.TSTypeQuery       => Defines.ANY.label
      case BabelAst.TSTypeLiteral     => Defines.ANY.label
      case BabelAst.TSArrayType       => code(tsType.json)
      case BabelAst.TSTupleType       => Defines.ANY.label
      case BabelAst.TSOptionalType =>
        typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
      case BabelAst.TSRestType =>
        typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
      case BabelAst.TSUnionType        => Defines.ANY.label
      case BabelAst.TSIntersectionType => Defines.ANY.label
      case BabelAst.TSConditionalType  => Defines.ANY.label
      case BabelAst.TSInferType        => Defines.ANY.label
      case BabelAst.TSParenthesizedType =>
        typeForTypeAnnotation(createBabelNodeInfo(tsType.json(TYPE_ANNOTATION_KEY)))
      case BabelAst.TSTypeOperator                => Defines.ANY.label
      case BabelAst.TSIndexedAccessType           => Defines.ANY.label
      case BabelAst.TSMappedType                  => Defines.ANY.label
      case BabelAst.TSLiteralType                 => Defines.ANY.label
      case BabelAst.TSExpressionWithTypeArguments => Defines.ANY.label
      case BabelAst.TSImportType                  => Defines.ANY.label
      case _ =>
        notHandledYet(tsType, TYPE_NOT_HANDLED_TEXT)
        Defines.ANY.label
    }
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNodeInfo): String = {
    typeAnnotation.node match {
      case BabelAst.TypeAnnotation   => typeForFlowType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
      case BabelAst.TSTypeAnnotation => typeForTsType(createBabelNodeInfo(typeAnnotation.json(TYPE_ANNOTATION_KEY)))
      case _: BabelAst.FlowType      => typeForFlowType(createBabelNodeInfo(typeAnnotation.json))
      case _: BabelAst.TSType        => typeForTsType(createBabelNodeInfo(typeAnnotation.json))
      case _ =>
        notHandledYet(typeAnnotation, TYPE_NOT_HANDLED_TEXT)
        Defines.ANY.label
    }
  }

  protected def typeFor(node: BabelNodeInfo): String = {
    Seq(TYPE_ANNOTATION_KEY, RETURN_TYPE_KEY).find(hasKey(node.json, _)) match {
      case Some(key) =>
        val tpe = typeForTypeAnnotation(createBabelNodeInfo(node.json(key)))
        registerType(tpe, tpe)
        tpe
      case None =>
        Defines.ANY.label
    }
  }

}
