package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetNodeInfo, ParserKeys}
import io.joern.csharpsrc2cpg.utils.Utils.{withoutSignature}
import io.joern.csharpsrc2cpg.{CSharpDefines, Constants, astcreation}
import io.joern.x2cpg.utils.IntervalKeyPool
import io.joern.x2cpg.{Ast, Defines, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.*
import io.shiftleft.codepropertygraph.generated.{Operators, PropertyNames}
import ujson.Value

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val anonymousTypeKeyPool = new IntervalKeyPool(first = 0, last = Long.MaxValue)

  def nextAnonymousTypeName(): String = s"${CSharpDefines.AnonymousTypePrefix}${anonymousTypeKeyPool.next}"

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
      case Some(fullName) => s"${withoutSignature(fullName)}.${nameFromNode(node)}"
      case _              => nameFromNode(node)
  }

  protected def getTypeFullNameFromAstNode(ast: Seq[Ast]): String = {
    ast.headOption.map(getTypeFullNameFromAstNode).getOrElse(Defines.Any)
  }

  protected def getTypeFullNameFromAstNode(ast: Ast): String = {
    ast.root
      .flatMap(_.properties.get(PropertyNames.TYPE_FULL_NAME))
      .map(_.toString)
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
        logger.warn(s"Unhandled declaration type '${x.label}' for ${x.name}")
        identifierNode(dotNetNode.orNull, x.name, x.name, Defines.Any)
  }

  protected val fixedTypeOperators: Map[String, String] = Map(
    Operators.equals            -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.notEquals         -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.logicalAnd        -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.logicalOr         -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.greaterThan       -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.greaterEqualsThan -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.lessThan          -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool),
    Operators.lessEqualsThan    -> BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool)
  )

  protected def nodeTypeFullName(node: DotNetNodeInfo): String = {
    node.node match {
      case NumericLiteralExpression if node.code.matches("^\\d+$") => // e.g. 200
        BuiltinTypes.DotNetTypeMap(BuiltinTypes.Int)
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[d|D]?$") => // e.g 2.1 or 2d
        BuiltinTypes.DotNetTypeMap(BuiltinTypes.Double)
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[f|F]?$") => // e.g. 2f or 2.1F
        BuiltinTypes.DotNetTypeMap(BuiltinTypes.Float)
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[m|M]?$") => // e.g. 2m or 2.1M
        BuiltinTypes.DotNetTypeMap(BuiltinTypes.Decimal)
      case StringLiteralExpression                        => BuiltinTypes.DotNetTypeMap(BuiltinTypes.String)
      case TrueLiteralExpression | FalseLiteralExpression => BuiltinTypes.DotNetTypeMap(BuiltinTypes.Bool)
      case NullLiteralExpression                          => BuiltinTypes.DotNetTypeMap(BuiltinTypes.Null)
      case ObjectCreationExpression =>
        val typeName = nameFromNode(createDotNetNodeInfo(node.json(ParserKeys.Type)))
        scope
          .tryResolveTypeReference(typeName)
          .map(_.name)
          .getOrElse(typeName)
      case ThisExpression =>
        scope.surroundingTypeDeclFullName.getOrElse(Defines.Any)
      case PredefinedType | SimpleBaseType =>
        BuiltinTypes.DotNetTypeMap.getOrElse(node.code, Defines.Any)
      case ArrayType =>
        val elementTypeNode = createDotNetNodeInfo(node.json(ParserKeys.ElementType))
        s"${nodeTypeFullName(elementTypeNode)}[]"
      case GenericName =>
        val typeName = nameFromNode(node)
        scope
          .tryResolveTypeReference(typeName)
          .map(_.name)
          .getOrElse(typeName)
      case NullableType =>
        val elementTypeNode = createDotNetNodeInfo(node.json(ParserKeys.ElementType))
        nodeTypeFullName(elementTypeNode)
      case IdentifierName =>
        val typeString = nameFromNode(node)
        scope
          .tryResolveTypeReference(typeString)
          .map(_.name)
          .orElse(BuiltinTypes.DotNetTypeMap.get(typeString))
          .getOrElse(typeString)
      case Attribute =>
        val typeString = s"${nameFromNode(node)}Attribute"
        scope
          .tryResolveTypeReference(typeString)
          .map(_.name)
          .orElse(BuiltinTypes.DotNetTypeMap.get(typeString))
          .getOrElse(typeString)
      case _ =>
        Try(node.json(ParserKeys.Type)).map(createDotNetNodeInfo) match
          case Success(typeNode) =>
            nodeTypeFullName(typeNode)
          case Failure(e) =>
            logger.debug(e.getMessage)
            Defines.Any
    }
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
    val ln       = metaData(ParserKeys.LineStart).numOpt.map(_.toInt)
    val cn       = metaData(ParserKeys.ColumnStart).numOpt.map(_.toInt)
    val lnEnd    = metaData(ParserKeys.LineEnd).numOpt.map(_.toInt)
    val cnEnd    = metaData(ParserKeys.ColumnEnd).numOpt.map(_.toInt)
    val node     = nodeType(metaData, relativeFileName)
    val c = node.toString match
      case "Attribute" =>
        metaData(ParserKeys.Code).strOpt.map(x => x.takeWhile(x => x != '\n')).getOrElse("<empty>").strip()
      case _ =>
        metaData(ParserKeys.Code).strOpt.map(x => x.takeWhile(x => x != '\n' && x != '{')).getOrElse("<empty>").strip()
    DotNetNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def nodeType(node: Value, relativeFileName: Option[String] = None): DotNetParserNode =
    DotNetJsonAst.fromString(node(ParserKeys.Kind).str, relativeFileName)

  @tailrec
  def nameFromNode(node: DotNetNodeInfo): String = {
    node.node match
      case NamespaceDeclaration | UsingDirective | FileScopedNamespaceDeclaration => nameFromNamespaceDeclaration(node)
      case IdentifierName | Parameter | _: DeclarationExpr | GenericName | SingleVariableDesignation =>
        nameFromIdentifier(node)
      case QualifiedName => nameFromQualifiedName(node)
      case SimpleMemberAccessExpression | MemberBindingExpression | SuppressNullableWarningExpression | Attribute =>
        nameFromIdentifier(createDotNetNodeInfo(node.json(ParserKeys.Name)))
      case ObjectCreationExpression | CastExpression => nameFromNode(createDotNetNodeInfo(node.json(ParserKeys.Type)))
      case ThisExpression                            => Constants.This
      case _                                         => "<empty>"
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

  def elementTypesFromCollectionType(collType: String): Seq[String] = {
    val genericRegex = "^\\w+<([\\w, ]+)>$".r
    collType match {
      case genericRegex(elementTypes) => elementTypes.split("[,]").map(_.trim).toSeq
      case t if t.endsWith("[]")      => t.stripSuffix("[]") :: Nil
      case t                          => t :: Nil
    }
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
    Void    -> "System.Void"
  )
}
