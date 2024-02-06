package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetNodeInfo, ParserKeys}
import io.joern.csharpsrc2cpg.{Constants, astcreation}
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{DispatchTypes, PropertyNames}
import ujson.Value

import scala.util.{Failure, Success, Try}
trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def createDotNetNodeInfo(json: Value): DotNetNodeInfo =
    AstCreatorHelper.createDotNetNodeInfo(json, Option(this.relativeFileName))

  protected def nullSafeCreateParserNodeInfo(json: Option[Value]): DotNetNodeInfo = {
    json match
      case Some(value) if !value.isNull => createDotNetNodeInfo(value)
      case _ => {
        logger.warn("Key not found in json. Defaulting to a null node.")
        DotNetNodeInfo(DotNetJsonAst.Unknown, ujson.Null, "", None, None, None, None)
      }
  }

  def createCallNodeForOperator(
    node: DotNetNodeInfo,
    operatorMethod: String,
    signature: Option[String] = None,
    typeFullName: Option[String] = None
  ): NewCall = {
    callNode(node, node.code, operatorMethod, operatorMethod, DispatchTypes.STATIC_DISPATCH, signature, typeFullName)
  }

  protected def notHandledYet(node: DotNetNodeInfo): Seq[Ast] = {
    val text =
      s"""Node type '${node.node}' not handled yet!
         |  Code: '${node.code}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${node.lineNumber.getOrElse(-1)}
         |  Column: ${node.columnNumber.getOrElse(-1)}
         |  """.stripMargin
    logger.warn(text)
    Seq(Ast(unknownNode(node, node.code)))
  }

  protected def astFullName(node: DotNetNodeInfo): String = {
    scope.surroundingScopeFullName match
      case Some(fullName) => s"$fullName.${nameFromNode(node)}"
      case _              => nameFromNode(node)
  }

  protected def getTypeFullNameFromAstNode(ast: Seq[Ast]): String = {
    ast.headOption
      .flatMap(_.root)
      .map(_.properties.getOrElse(PropertyNames.TYPE_FULL_NAME, Defines.Any).toString)
      .getOrElse(Defines.Any)
  }

  protected def thisNode: NewIdentifier = {
    NewIdentifier()
      .code(Constants.This)
      .name(Constants.This)
      .typeFullName(scope.surroundingTypeDeclFullName.getOrElse(Defines.Any))
  }

  protected def nameFromNode(identifierNode: DotNetNodeInfo): String = AstCreatorHelper.nameFromNode(identifierNode)

  protected def identifierFromDecl(decl: DeclarationNew, dotNetNode: Option[DotNetNodeInfo] = None): NewIdentifier = {
    decl match
      case x: NewLocal =>
        identifierNode(dotNetNode.orNull, x.name, x.code, x.typeFullName, x.dynamicTypeHintFullName)
      case x: NewMethodParameterIn =>
        identifierNode(dotNetNode.orNull, x.name, x.code, x.typeFullName, x.dynamicTypeHintFullName)
      case x =>
        logger.warn(s"Unhandled declaration type '${x.label()}' for ${x.name}")
        identifierNode(dotNetNode.orNull, x.name, x.name, Defines.Any)
  }

  // TODO: Use type map to try resolve full name
  protected def nodeTypeFullName(node: DotNetNodeInfo): String = {

    def typeFromTypeString(typeString: String): String = {
      val isArrayType = typeString.endsWith("[]")
      val rawType     = typeString.stripSuffix("[]")
      val resolvedType = scope
        .tryResolveTypeReference(rawType)
        .orElse(BuiltinTypes.DotNetTypeMap.get(rawType))
        .getOrElse(rawType)

      if (isArrayType) s"$resolvedType[]"
      else resolvedType
    }

    node.node match
      case NumericLiteralExpression if node.code.matches("^\\d+$") => // e.g. 200
        BuiltinTypes.Int
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[d|D]?$") => // e.g 2.1 or 2d
        BuiltinTypes.Double
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[f|F]?$") => // e.g. 2f or 2.1F
        BuiltinTypes.Float
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[m|M]?$") => // e.g. 2m or 2.1M
        BuiltinTypes.Decimal
      case StringLiteralExpression                        => BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)
      case TrueLiteralExpression | FalseLiteralExpression => BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool)
      case IdentifierName                                 => typeFromTypeString(nameFromNode(node))
      case PredefinedType | SimpleBaseType =>
        BuiltinTypes.DotNetTypeMap.getOrElse(node.code, Defines.Any)
      case _ =>
        Try(createDotNetNodeInfo(node.json(ParserKeys.Type))) match
          case Success(typeNode) =>
            typeFromTypeString(typeNode.code)
          case Failure(e) =>
            logger.debug(e.getMessage)
            Defines.Any
  }

}

object AstCreatorHelper {

  /** Creates a info node for the given JSON node.
    * @param json
    *   the json node to convert.
    * @param relativeFileName
    *   optional file name for debugging purposes.
    * @return
    *   the node info.
    */
  def createDotNetNodeInfo(json: Value, relativeFileName: Option[String] = None): DotNetNodeInfo = {
    val metaData = json(ParserKeys.MetaData)
    val ln       = metaData(ParserKeys.LineStart).numOpt.map(_.toInt.asInstanceOf[Integer])
    val cn       = metaData(ParserKeys.ColumnStart).numOpt.map(_.toInt.asInstanceOf[Integer])
    val lnEnd    = metaData(ParserKeys.LineEnd).numOpt.map(_.toInt.asInstanceOf[Integer])
    val cnEnd    = metaData(ParserKeys.ColumnEnd).numOpt.map(_.toInt.asInstanceOf[Integer])
    val c =
      metaData(ParserKeys.Code).strOpt.map(x => x.takeWhile(x => x != '\n' && x != '{')).getOrElse("<empty>").strip()
    val node = nodeType(metaData, relativeFileName)
    DotNetNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def nodeType(node: Value, relativeFileName: Option[String] = None): DotNetParserNode =
    DotNetJsonAst.fromString(node(ParserKeys.Kind).str, relativeFileName)

  def nameFromNode(node: DotNetNodeInfo): String = {
    node.node match
      case NamespaceDeclaration | UsingDirective           => nameFromNamespaceDeclaration(node)
      case IdentifierName | Parameter | _: DeclarationExpr => nameFromIdentifier(node)
      case QualifiedName                                   => nameFromQualifiedName(node)
      case SimpleMemberAccessExpression => nameFromIdentifier(createDotNetNodeInfo(node.json(ParserKeys.Name)))
      case _                            => "<empty>"
  }

  private def nameFromNamespaceDeclaration(namespace: DotNetNodeInfo): String = {
    val nameNode = createDotNetNodeInfo(namespace.json(ParserKeys.Name))
    nameFromNode(nameNode)
  }

  private def nameFromIdentifier(identifier: DotNetNodeInfo): String = {
    identifier.json(ParserKeys.Identifier).obj(ParserKeys.Value).str
  }

  private def nameFromQualifiedName(qualifiedName: DotNetNodeInfo): String = {
    val rhs = nameFromNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Right)))
    val lhs = nameFromNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Left)))
    s"$lhs.$rhs"
  }
}

/** Contains all the C# builtin types, as well as `null` and `void`.
  *
  * @see
  *   <a href="https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/built-in-types">Built-in
  *   types (C# reference)</a>
  */
object BuiltinTypes {

  // Primitives
  val Bool    = "bool"
  val Byte    = "byte"
  val SByte   = "sbyte"
  val Char    = "char"
  val Decimal = "decimal"
  val Double  = "double"
  val Float   = "float"
  val Int     = "int"
  val UInt    = "uint"
  val NInt    = "nint"
  val NUInt   = "nuint"
  val Long    = "long"
  val ULong   = "ulong"
  val Short   = "short"
  val UShort  = "ushort"

  // Reference types
  val Object  = "object"
  val String  = "string"
  val Dynamic = "dynamic"

  // Other
  val Null = "null"
  val Void = "void"

  val DotNetTypeMap = Map(
    Bool    -> "System.Boolean",
    Byte    -> "System.Byte",
    SByte   -> "System.SByte",
    Char    -> "System.Char",
    Decimal -> "System.Decimal",
    Double  -> "System.Double",
    Float   -> "System.Single",
    Int     -> "System.Int32",
    UInt    -> "System.UInt32",
    NInt    -> "System.IntPtr",
    NUInt   -> "System.UIntPtr",
    Long    -> "System.Int64",
    ULong   -> "System.UInt64",
    Short   -> "System.Int16",
    UShort  -> "System.UInt16",
    Object  -> "System.Object",
    String  -> "System.String",
    Dynamic -> "System.Object",
    Null    -> Null,
    Void    -> Void
  )
}
