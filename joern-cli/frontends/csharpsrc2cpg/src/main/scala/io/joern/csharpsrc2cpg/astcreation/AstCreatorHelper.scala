package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.astcreation
import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewTypeDecl}
import ujson.Value

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected def createDotNetNodeInfo(json: Value): DotNetNodeInfo = {
    val metaData = json(ParserKeys.MetaData)
    val ln       = metaData(ParserKeys.LineStart).numOpt.map(_.toInt.asInstanceOf[Integer])
    val cn       = metaData(ParserKeys.ColumnStart).numOpt.map(_.toInt.asInstanceOf[Integer])
    val lnEnd    = metaData(ParserKeys.LineEnd).numOpt.map(_.toInt.asInstanceOf[Integer])
    val cnEnd    = metaData(ParserKeys.ColumnEnd).numOpt.map(_.toInt.asInstanceOf[Integer])
    val c =
      metaData(ParserKeys.Code).strOpt.map(x => x.takeWhile(x => x != '\n' && x != '{')).getOrElse("<empty>").strip()
    val node = nodeType(metaData)
    DotNetNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  protected def notHandledYet(node: DotNetNodeInfo): Seq[Ast] = {
    val text =
      s"""Node type '${node.node}' not handled yet!
         |  Code: '${node.code}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${node.lineNumber.getOrElse(-1)}
         |  Column: ${node.columnNumber.getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Seq(Ast(unknownNode(node, node.code)))
  }

  private def nodeType(node: Value): DotNetParserNode =
    DotNetJsonAst.fromString(node(ParserKeys.Kind).str, this.relativeFileName)

  protected def astFullName(node: DotNetNodeInfo): String = {
    methodAstParentStack.headOption match
      case Some(head: NewNamespaceBlock) => s"${head.fullName}.${nameFromNode(node)}"
      case Some(head: NewMethod)         => s"${head.fullName}.${nameFromNode(node)}"
      case Some(head: NewTypeDecl)       => s"${head.fullName}.${nameFromNode(node)}"
      case _                             => nameFromNode(node)
  }

  protected def nameFromNode(identifierNode: DotNetNodeInfo): String = {
    identifierNode.node match
      case IdentifierName | Parameter => nameFromIdentifier(identifierNode)
      case QualifiedName              => nameFromQualifiedName(identifierNode)
      case _: DeclarationExpr         => nameFromDeclaration(identifierNode)
      case _                          => "<empty>"
  }

  protected def nameFromIdentifier(identifier: DotNetNodeInfo): String = {
    identifier.json(ParserKeys.Identifier).obj(ParserKeys.Value).str
  }

  protected def nameFromDeclaration(node: DotNetNodeInfo): String = {
    node.json(ParserKeys.Identifier).obj(ParserKeys.Value).str
  }

  protected def nameFromQualifiedName(qualifiedName: DotNetNodeInfo): String = {
    val rhs = nameFromNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Right)))
    val lhs = nameFromNode(createDotNetNodeInfo(qualifiedName.json(ParserKeys.Left)))
    s"$lhs.$rhs"
  }

  // TODO: Use type map to try resolve full name
  protected def nodeTypeFullName(node: DotNetNodeInfo): String = {
    node.node match
      case NumericLiteralExpression if node.code.matches("^\\d+$") => // e.g. 200
        BuiltinTypes.Int
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[d|D]?$") => // e.g 2.1 or 2d
        BuiltinTypes.Double
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[f|F]?$") => // e.g. 2f or 2.1F
        BuiltinTypes.Float
      case NumericLiteralExpression if node.code.matches("^\\d+\\.?\\d*[m|M]?$") => // e.g. 2m or 2.1M
        BuiltinTypes.Decimal
      case _ =>
        val typeNode     = createDotNetNodeInfo(node.json(ParserKeys.Type))
        val isArrayType  = typeNode.code.endsWith("[]")
        val rawType      = typeNode.code.stripSuffix("[]")
        val resolvedType = BuiltinTypes.DotNetTypeMap.getOrElse(rawType, rawType)

        if (isArrayType) s"$resolvedType[]"
        else resolvedType
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
