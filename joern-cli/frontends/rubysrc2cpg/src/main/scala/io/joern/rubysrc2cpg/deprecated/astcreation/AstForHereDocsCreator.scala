package io.joern.rubysrc2cpg.deprecated.astcreation

import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.{HereDocArgumentContext, HereDocLiteralContext}
import io.joern.rubysrc2cpg.deprecated.parser.HereDocHandling
import io.joern.rubysrc2cpg.deprecated.passes.Defines
import io.joern.x2cpg.{Ast, ValidationMode}
import io.shiftleft.codepropertygraph.generated.nodes.{NewCall, NewIdentifier, NewLiteral}

import scala.collection.immutable.Seq
import scala.collection.mutable

trait AstForHereDocsCreator(implicit withSchemaValidation: ValidationMode) { this: AstCreator =>

  private val hereDocTokens = mutable.Stack[(String, NewLiteral)]()

  protected def astForHereDocLiteral(ctx: HereDocLiteralContext): Ast = {
    val delimiter  = HereDocHandling.getHereDocDelimiter(ctx.HERE_DOC().getText).getOrElse("")
    val hereDoc    = ctx.HERE_DOC().getText.replaceFirst("<<[~-]", "")
    val hereDocTxt = hereDoc.stripPrefix(delimiter).stripSuffix(delimiter).strip()
    val literal = NewLiteral()
      .code(hereDocTxt)
      .typeFullName(Defines.String)
      .lineNumber(line(ctx))
      .columnNumber(column(ctx))
    Ast(literal)
  }

  protected def astForHereDocArgument(ctx: HereDocArgumentContext): Seq[Ast] =
    HereDocHandling.getHereDocDelimiter(ctx.HERE_DOC_IDENTIFIER().getText) match
      case Some(delimiter) =>
        val literal = NewLiteral()
          .code("") // build code from the upcoming statements
          .typeFullName(Defines.String)
          .lineNumber(line(ctx))
          .columnNumber(column(ctx))
        hereDocTokens.push((delimiter, literal))
        Seq(Ast(literal))
      case None => Seq.empty

  /** Will determine, if we have recently met a here doc initializer, if this statement should be converted to a here
    * doc literal or returned as-is.
    * @param stmt
    *   the statement AST.
    * @return
    *   the statement AST or nothing if this is determined to be a here doc body.
    */
  protected def scanStmtForHereDoc(stmt: Seq[Ast]): Seq[Ast] = {
    if (stmt.nonEmpty && hereDocTokens.nonEmpty) {
      val (delimiter, literalNode) = hereDocTokens.head
      val stmtAst                  = stmt.head
      val atHereDocInitializer = stmt.flatMap(_.nodes).exists {
        case x: NewLiteral => hereDocTokens.exists(_._2 == x)
        case _             => false
      }
      if (atHereDocInitializer) {
        // We are at the start of the here doc, do nothing
        stmt
      } else {
        // We are in the middle of the here doc, convert statements to here doc body + look out for delimiter
        val txt = stmtAst.root match
          case Some(x: NewCall)       => x.code
          case Some(x: NewIdentifier) => x.code
          case _                      => ""

        if (txt == delimiter) hereDocTokens.pop()
        else literalNode.code(s"${literalNode.code}\n$txt".trim)
        Seq.empty[Ast]
      }
    } else {
      stmt
    }
  }

}
