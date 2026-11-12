package io.joern.jssrc2cpg.astcreation

import io.joern.jssrc2cpg.parser.BabelNodeSyntax.*
import io.joern.x2cpg.frontendspecific.jssrc2cpg.Defines

import java.util.regex.Pattern

trait TypeHelper { this: AstCreator =>

  private val ImportMatcher       = Pattern.compile("(typeof )?import\\([\"'](.*)[\"']\\)")
  private val ArrayReplacePattern = ":[^,.]+\\[]".r

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

  protected def isPlainTypeAlias(alias: DeclareTypeAlias | TypeAlias | TSTypeAliasDeclaration): Boolean = alias match {
    case tsAlias: TSTypeAliasDeclaration => tsAlias.typeAnnotation.isInstanceOf[TSTypeReference]
    case _                               => false
  }

  private def typeForFlowType(flowType: BabelNode): String = flowType match {
    case _: BooleanTypeAnnotation        => Defines.Boolean
    case _: NumberTypeAnnotation         => Defines.Number
    case _: ObjectTypeAnnotation         => Defines.Object
    case _: StringTypeAnnotation         => Defines.String
    case _: SymbolTypeAnnotation         => Defines.Symbol
    case _: NumberLiteralTypeAnnotation  => flowType.code
    case _: ArrayTypeAnnotation          => Defines.Array
    case _: BooleanLiteralTypeAnnotation => flowType.code
    case _: NullLiteralTypeAnnotation    => flowType.code
    case _: StringLiteralTypeAnnotation  => flowType.code
    case generic: GenericTypeAnnotation  =>
      generic.id.code match {
        case tpe if isNumberType(tpe) => Defines.Number
        case tpe if isStringType(tpe) => Defines.String
        case tpe                      => tpe
      }
    case _: ThisTypeAnnotation => typeHintForThisExpression(Option(flowType)).headOption.getOrElse(Defines.Any)
    case nullable: NullableTypeAnnotation => typeForTypeAnnotation(nullable.typeAnnotation)
    case _                                => Defines.Any
  }

  private def typeForTsType(tsType: BabelNode): String = tsType match {
    case _: TSBooleanKeyword   => Defines.Boolean
    case _: TSBigIntKeyword    => Defines.Number
    case _: TSNullKeyword      => Defines.Null
    case _: TSNumberKeyword    => Defines.Number
    case _: TSObjectKeyword    => Defines.Object
    case _: TSStringKeyword    => Defines.String
    case _: TSSymbolKeyword    => Defines.Symbol
    case _: TSUnknownKeyword   => Defines.Unknown
    case _: TSVoidKeyword      => Defines.Void
    case _: TSUndefinedKeyword => Defines.Undefined
    case _: TSNeverKeyword     => Defines.Never
    case _: TSIntrinsicKeyword => tsType.code
    case _: TSTypeReference    =>
      tsType.code match {
        case tpe if isNumberType(tpe) => Defines.Number
        case tpe if isStringType(tpe) => Defines.String
        case tpe                      => tpe
      }
    case _: TSArrayType           => Defines.Array
    case _: TSThisType            => typeHintForThisExpression(Option(tsType)).headOption.getOrElse(Defines.Any)
    case optional: TSOptionalType => typeForTypeAnnotation(optional.typeAnnotation)
    case rest: TSRestType         => typeForTypeAnnotation(rest.typeAnnotation)
    case parenthesized: TSParenthesizedType => typeForTypeAnnotation(parenthesized.typeAnnotation)
    case _                                  => Defines.Any
  }

  private def typeForTypeAnnotation(typeAnnotation: BabelNode): String = typeAnnotation match {
    case flow: TypeAnnotation => typeForFlowType(flow.typeAnnotation)
    case ts: TSTypeAnnotation => typeForTsType(ts.typeAnnotation)
    case _: FlowType          => typeForFlowType(typeAnnotation)
    case _: TSType            => typeForTsType(typeAnnotation)
    case _                    => Defines.Any
  }

  /** The explicitly annotated type of a node: the `typeAnnotation` of e.g. identifiers and properties or the
    * `returnType` of functions, if there is any.
    */
  private def annotatedType(node: BabelNode): Option[BabelNode] = node match {
    case n: Identifier                      => n.typeAnnotation
    case n: ArrayPattern                    => n.typeAnnotation
    case n: ObjectPattern                   => n.typeAnnotation
    case n: AssignmentPattern               => n.typeAnnotation
    case n: RestElement                     => n.typeAnnotation
    case n: ClassProperty                   => n.typeAnnotation
    case n: ClassPrivateProperty            => n.typeAnnotation
    case n: ClassAccessorProperty           => n.typeAnnotation
    case n: Placeholder                     => n.typeAnnotation
    case n: DeclareModuleExports            => Some(n.typeAnnotation)
    case n: FunctionTypeParam               => Some(n.typeAnnotation)
    case n: NullableTypeAnnotation          => Some(n.typeAnnotation)
    case n: TypeAnnotation                  => Some(n.typeAnnotation)
    case n: TypeCastExpression              => Some(n.typeAnnotation)
    case n: TSAsExpression                  => Some(n.typeAnnotation)
    case n: TSTypeAssertion                 => Some(n.typeAnnotation)
    case n: TSSatisfiesExpression           => Some(n.typeAnnotation)
    case n: TSTypeAliasDeclaration          => Some(n.typeAnnotation)
    case n: TSTypeAnnotation                => Some(n.typeAnnotation)
    case n: TSOptionalType                  => Some(n.typeAnnotation)
    case n: TSParenthesizedType             => Some(n.typeAnnotation)
    case n: TSRestType                      => Some(n.typeAnnotation)
    case n: TSTypeOperator                  => Some(n.typeAnnotation)
    case n: TSMappedType                    => n.typeAnnotation
    case n: TSCallSignatureDeclaration      => n.returnType
    case n: TSConstructSignatureDeclaration => n.returnType
    case n: TSConstructorType               => n.returnType
    case n: TSFunctionType                  => n.returnType
    case n: TSIndexSignature                => n.typeAnnotation
    case n: TSMethodSignature               => n.returnType
    case n: TSPropertySignature             => n.typeAnnotation
    case n: TSTypePredicate                 => n.typeAnnotation
    case n: ArrowFunctionExpression         => n.returnType
    case n: ClassMethod                     => n.returnType
    case n: ClassPrivateMethod              => n.returnType
    case n: FunctionDeclaration             => n.returnType
    case n: FunctionExpression              => n.returnType
    case n: ObjectMethod                    => n.returnType
    case n: FunctionTypeAnnotation          => Some(n.returnType)
    case n: TSDeclareFunction               => n.returnType
    case n: TSDeclareMethod                 => n.returnType
    case _                                  => None
  }

  private def isStringType(tpe: String): Boolean = tpe == "string"

  private def isNumberType(tpe: String): Boolean = tpe == "number" || tpe == "int" || tpe.toDoubleOption.isDefined

  private def typeFromTypeMap(node: BabelNode): String =
    range(node).flatMap(parserResult.typeMap.get) match {
      case Some(value) if value == "any"      => Defines.Any
      case Some(value) if value == "boolean"  => Defines.Boolean
      case Some(value) if value == "null"     => Defines.Null
      case Some(value) if isStringType(value) => Defines.String
      case Some(value) if isNumberType(value) => Defines.Number
      case Some(value)                        =>
        val matcher = ImportMatcher.matcher(value)
        if (matcher.matches()) importToModule(value, matcher) else value
      case None => Defines.Any
    }

  private def importToModule(value: String, matcher: java.util.regex.Matcher): String =
    this.rootTypeDecl.headOption match {
      case Some(typeDecl)            => typeDecl.fullName
      case None if matcher.matches() => matcher.group(2).stripSuffix(".js").concat(s".js:${Defines.Program}")
      case None                      => value
    }

  protected def typeFor(node: BabelNode): String = {
    val tpe = annotatedType(node) match {
      case Some(annotation) => typeForTypeAnnotation(annotation)
      case None             => typeFromTypeMap(node)
    }
    // Every replacement key contains a space or a '{'; if the type has neither, none of them can match, so we can skip
    // the twelve String.contains scans entirely.
    val tpeWithTypeReplacements = if (tpe.indexOf(' ') >= 0 || tpe.indexOf('{') >= 0) {
      TypeReplacements.foldLeft(tpe) { case (typeStr, (searchStr, replacement)) =>
        if (typeStr.contains(searchStr)) typeStr.replace(searchStr, replacement) else typeStr
      }
    } else tpe
    val tpeWithArrayReplacements = if (tpeWithTypeReplacements.endsWith("[]")) {
      Defines.Array
    } else {
      ArrayReplacePattern.replaceAllIn(tpeWithTypeReplacements, s": ${Defines.Array}")
    }
    if (!tpeWithArrayReplacements.contains("{") && !tpe.contains("(")) {
      registerType(tpeWithArrayReplacements)
    }
    tpeWithArrayReplacements
  }

  protected def typeHintForThisExpression(node: Option[BabelNode] = None): Seq[String] = {
    dynamicInstanceTypeStack.headOption match {
      case Some(tpe)              => Seq(tpe)
      case None if node.isDefined =>
        typeFor(node.get) match {
          case tpe if tpe != Defines.Any && tpe != "this" => if (Defines.isBuiltinType(tpe)) Seq(tpe) else Seq.empty
          case _                                          => rootTypeDecl.map(_.fullName).toSeq
        }
      case None => rootTypeDecl.map(_.fullName).toSeq
    }
  }

}
