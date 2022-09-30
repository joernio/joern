package ammonite.foo

import java.util.Map

import ammonite.compiler.iface.{Compiler => _, Parser => IParser, _}
import ammonite.compiler.internal.CompilerHelper
import ammonite.util.ImportTree
import ammonite.util.Util.CodeSource

import dotty.tools.dotc
import dotc.ast.untpd
import dotc.CompilationUnit
import dotc.core.Contexts.{ctx, Context, ContextBase}
import dotc.parsing.Tokens
import dotc.util.SourceFile

import scala.collection.mutable

class Parsers extends IParser {

  // FIXME Get via Compiler?
  private lazy val initCtx: Context =
    (new ContextBase).initialCtx

  // From
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ParseResult.scala/#L115-L120
  private def parseStats(using Context): List[untpd.Tree] = {
    val parser = new DottyParser(ctx.source)
    val stats = parser.blockStatSeq()
    parser.accept(Tokens.EOF)
    stats
  }

  // Adapted from
  // https://github.com/lampepfl/dotty/blob/3.0.0-M3/
  //   compiler/src/dotty/tools/repl/ParseResult.scala/#L163-L184
  /** Check if the input is incomplete.
   *
   *  This can be used in order to check if a newline can be inserted without
   *  having to evaluate the expression.
   */
  private def isComplete(sourceCode: String)(using Context): Boolean =
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<incomplete-handler>", sourceCode, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }
    reporter.hasErrors || !needsMore

  private val BlockPat = """(?s)^\s*\{(.*)\}\s*$""".r

  def split(
    code: String,
    ignoreIncomplete: Boolean = true,
    fileName: String = "(console)"
  ): Option[Either[String, Seq[String]]] =
    doSplit(code, ignoreIncomplete, fileName)
      .map(_.map(_.map(_._2)))

  private def doSplit(
    code: String,
    ignoreIncomplete: Boolean,
    fileName: String
  ): Option[Either[String, Seq[(Int, String)]]] = {
    val code0 = code match {
      case BlockPat(wrapped) => wrapped
      case _ => code
    }

    given Context = initCtx
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<splitter>", code0, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    val stats = reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }

    val nl = System.lineSeparator
    def errors = reporter
      .removeBufferedMessages
      .map { e =>
        val maybeMsg = scala.util.Try {
          CompilerHelper.messageAndPos(Compiler.messageRenderer, e)
        }
        Compiler.messageRenderer.stripColor(maybeMsg.getOrElse("???"))
      }
      .mkString(nl)

    if (reporter.hasErrors)
      Some(Left(s"$fileName$nl$errors"))
    else if (needsMore)
      None
    else {
      val startIndices = stats.toArray.map(_.startPos(using localCtx).point)
      def startEndIndices = startIndices.iterator
        .zip(startIndices.iterator.drop(1) ++ Iterator(code0.length))
      val stmts = startEndIndices.map {
        case (start, end) =>
          code0.substring(start, end)
      }.toVector
      val statsAndStmts = stats.zip(stmts).zip(startIndices).iterator

      val stmts0 = new mutable.ListBuffer[(Int, String)]
      var current = Option.empty[(untpd.Tree, String, Int)]
      while (statsAndStmts.hasNext) {
        val next = statsAndStmts.next()
        val ((nextStat, nextStmt), nextIdx) = next
        (current, nextStat) match {
          case (Some((_: untpd.Import, stmt, idx)), _: untpd.Import)
              if stmt.startsWith("import ") && !nextStmt.startsWith("import ") =>
            current = Some((nextStat, stmt + nextStmt, idx))
          case _ =>
            current.foreach { case (_, stmt, idx) => stmts0.+=((idx, stmt)) }
            current = Some((nextStat, nextStmt, nextIdx))
        }
      }
      current.foreach { case (_, stmt, idx) => stmts0.+=((idx, stmt)) }

      Some(Right(stmts0.toList))
    }
  }

  private def importExprs(i: untpd.Import): Seq[String] = {
    def exprs(t: untpd.Tree): List[String] =
      t match {
        case untpd.Ident(name) => name.decode.toString :: Nil
        case untpd.Select(qual, name) => name.decode.toString :: exprs(qual)
        case _ => Nil // ???
      }
    exprs(i.expr).reverse
  }

  def importHooks(statement: String): (String, Seq[ImportTree]) = {

    given Context = initCtx
    val reporter = Compiler.newStoreReporter()
    val source   = SourceFile.virtual("<import-hooks-parser>", statement, maybeIncomplete = true)
    val unit     = CompilationUnit(source, mustExist = false)
    val localCtx = ctx.fresh
                      .setCompilationUnit(unit)
                      .setReporter(reporter)
    var needsMore = false
    val stats = reporter.withIncompleteHandler((_, _) => needsMore = true) {
      parseStats(using localCtx)
    }

    if (reporter.hasErrors || needsMore)
      (statement, Nil)
    else {
      var updatedStatement = statement
      var importTrees = Array.newBuilder[ImportTree]
      stats.foreach {
        case i: untpd.Import =>
          val exprs = importExprs(i)
          if (exprs.headOption.exists(_.startsWith("$"))) {
            val start = i.startPos.point
            val end = {
              var initialEnd = i.endPos.point
              // kind of meh
              // In statements like 'import $file.foo.{a, b}', endPos points at 'b' rather than '}',
              // so we work around that here.
              if (updatedStatement.iterator.drop(start).take(initialEnd - start).contains('{')) {
                while (updatedStatement.length > initialEnd &&
                        updatedStatement.charAt(initialEnd).isWhitespace)
                  initialEnd = initialEnd + 1
                if (updatedStatement.length > initialEnd &&
                      updatedStatement.charAt(initialEnd) == '}')
                  initialEnd = initialEnd + 1
              }
              initialEnd
            }
            val selectors = i.selectors.map { sel =>
              val from = sel.name.decode.toString
              val to = sel.rename.decode.toString
              from -> Some(to).filter(_ != from)
            }
            val updatedImport = updatedStatement.substring(start, end).takeWhile(_ != '.') + ".$"
            updatedStatement = updatedStatement.patch(
              start,
              updatedImport + (" ") * (end - start - updatedImport.length),
              end - start
            )

            val prefixLen = if (updatedStatement.startsWith("import ")) "import ".length else 0
            importTrees += ImportTree(
              exprs,
              Some(selectors).filter(_.nonEmpty),
              start + prefixLen, end
            )
          }
        case _ =>
      }
      (updatedStatement, importTrees.result)
    }
  }

  def parseImportHooksWithIndices(
    source: CodeSource,
    statements: Seq[(Int, String)]
  ): (Seq[String], Seq[ImportTree]) = {

    val (updatedStatements, trees) = statements.map {
      case (startIdx, stmt) =>
        val (hookedStmts, parsedTrees) = importHooks(stmt)

        val updatedParsedTrees = parsedTrees.map { importTree =>
          importTree.copy(
            start = startIdx + importTree.start,
            end = startIdx + importTree.end
          )
        }

        (hookedStmts, updatedParsedTrees)
    }.unzip

    (updatedStatements, trees.flatten)
  }

  private val scriptSplitPattern = "(?m)^\\s*@[\\s\\n\\r]+".r

  def splitScript(
    rawCode: String,
    fileName: String
  ): Either[String, IndexedSeq[(String, Seq[String])]] =
    scriptBlocksWithStartIndices(rawCode, fileName)
      .left.map(_.getMessage)
      .map(_.map(b => (b.ncomment, b.codeWithStartIndices.map(_._2))).toVector)

  def scriptBlocksWithStartIndices(
    rawCode: String,
    fileName: String
  ): Either[IParser.ScriptSplittingError, Seq[IParser.ScriptBlock]] = {

    val bounds = {
      def allBounds = Iterator(0) ++ scriptSplitPattern.findAllMatchIn(rawCode).flatMap { m =>
        Iterator(m.start, m.end)
      } ++ Iterator(rawCode.length)
      allBounds
        .grouped(2)
        .map { case Seq(start, end) => (start, end) }
        .toVector
    }

    val blocks = bounds.zipWithIndex.map {
      case ((start, end), idx) =>
        val blockCode = rawCode.substring(start, end)
        doSplit(blockCode, false, fileName) match {
          case None =>
            Right(
              IParser.ScriptBlock(
                start,
                "",
                Seq((start, blockCode))
              )
            )
          case Some(Left(err)) => Left(err)
          case Some(Right(stmts)) =>
            Right(
              IParser.ScriptBlock(
                start,
                blockCode.take(stmts.headOption.fold(0)(_._1)),
                stmts.map { case (idx, stmt) => (idx + start, stmt) }
              )
            )
        }
    }

    val errors = blocks.collect { case Left(err) => err }
    if (errors.isEmpty)
      Right(blocks.collect { case Right(elem) => elem })
    else
      Left(new IParser.ScriptSplittingError(errors.mkString(System.lineSeparator)))
  }

  def defaultHighlight(buffer: Vector[Char],
                       comment: fansi.Attrs,
                       `type`: fansi.Attrs,
                       literal: fansi.Attrs,
                       keyword: fansi.Attrs,
                       notImplemented: fansi.Attrs,
                       reset: fansi.Attrs): Vector[Char] = {
    val valDef = reset
    val annotation = reset
    new SyntaxHighlighting(
      reset,
      comment,
      keyword,
      valDef,
      literal,
      `type`,
      annotation,
      notImplemented
    ).highlight(buffer.mkString)(using initCtx).toVector
  }

  def isObjDef(code: String): Boolean =
    false // TODO

  def highlightIndices[T](buffer: Vector[Char],
                          ruleColors: PartialFunction[String, T],
                          endColor: T): Seq[(Int, T)] =
    // Not available in Scala 3
    Seq((0, endColor), (999999999, endColor))
}

object Parsers extends Parsers
