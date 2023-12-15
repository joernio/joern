package io.joern.csharpsrc2cpg.astcreation

import io.joern.csharpsrc2cpg.parser.DotNetJsonAst.*
import io.joern.csharpsrc2cpg.parser.{DotNetJsonAst, DotNetNodeInfo, ParserKeys}
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewMethod, NewNamespaceBlock, NewTypeDecl}
import ujson.Value

import scala.util.Try

trait AstCreatorHelper(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  protected val systemLineSep: String = System.getProperty("line.separator")

  protected def createDotNetNodeInfo(json: Value): DotNetNodeInfo = {
    val metaData = json(ParserKeys.MetaData)
    val ln       = jsonLine(metaData)
    val cn       = jsonColumn(metaData)
    val lnEnd    = jsonLineEnd(metaData)
    val cnEnd    = jsonColumnEnd(metaData)
    val c        = jsonCode(metaData)
    val node     = nodeType(metaData)
    DotNetNodeInfo(node, json, c, ln, cn, lnEnd, cnEnd)
  }

  private def jsonCode(json: Value): String = Try {
    val ln         = json(ParserKeys.LineStart).num.toInt
    val lnEnd      = json(ParserKeys.LineEnd).num.toInt
    lazy val cn    = json(ParserKeys.ColumnStart).num.toInt
    lazy val cnEnd = json(ParserKeys.ColumnEnd).num.toInt
    val resultList = this.parserResult.fileContent.linesIterator.zipWithIndex
      .filter(x => x._2 >= ln && x._2 <= lnEnd)
      .map(_._1)
      .toList
    resultList match
      case head :: Nil => head.substring(cn, cnEnd)
      case xs          => xs.mkString(systemLineSep)
  }.getOrElse("<empty>")

  private def jsonLine(json: Value): Option[Integer] = json(ParserKeys.LineStart).numOpt.map(_.toInt)

  private def jsonLineEnd(json: Value): Option[Integer] = json(ParserKeys.LineEnd).numOpt.map(_.toInt)

  private def jsonColumn(json: Value): Option[Integer] = json(ParserKeys.ColumnStart).numOpt.map(_.toInt)

  private def jsonColumnEnd(json: Value): Option[Integer] = json(ParserKeys.ColumnEnd).numOpt.map(_.toInt)

  protected def notHandledYet(node: DotNetNodeInfo): Ast = {
    val text =
      s"""Node type '${node.node}' not handled yet!
         |  Code: '${node.code}'
         |  File: '${parserResult.fullPath}'
         |  Line: ${node.lineNumber.getOrElse(-1)}
         |  Column: ${node.columnNumber.getOrElse(-1)}
         |  """.stripMargin
    logger.info(text)
    Ast(unknownNode(node, node.code))
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

}
