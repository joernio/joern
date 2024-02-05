package io.joern.rubysrc2cpg.deprecated

import io.joern.rubysrc2cpg.RubySrc2Cpg
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.deprecated.utils.PackageTable
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval
import org.slf4j.LoggerFactory

import java.io.File as JFile
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Try}

class ParseInternalStructures(
  parsedFiles: List[(String, DeprecatedRubyParser.ProgramContext)],
  projectRoot: Option[String] = None
) {

  private val logger = LoggerFactory.getLogger(getClass)

  def populatePackageTable(): Unit = {
    val tasks = parsedFiles.map { case (fileName, programCtx) =>
      () => {
        val relativeFilename: String =
          projectRoot.map(fileName.stripPrefix).map(_.stripPrefix(JFile.separator)).getOrElse(fileName)
        implicit val classStack: mutable.Stack[String] = mutable.Stack[String]()
        parseForStructures(relativeFilename, programCtx)
      }
    }.iterator
    ConcurrentTaskUtil.runUsingThreadPool(tasks).foreach {
      case Failure(exception) =>
        logger.warn("Exception encountered while scanning for internal structures", exception)
      case _ => // do nothing
    }
  }

  private def parseForStructures(relativeFilename: String, programCtx: ProgramContext)(implicit
    classStack: mutable.Stack[String]
  ): Unit = {
    val name     = ":program"
    val fullName = s"$relativeFilename:$name"
    classStack.push(fullName)
    if (
      programCtx.compoundStatement() != null &&
      programCtx.compoundStatement().statements() != null
    ) {
      programCtx.compoundStatement().statements().statement().asScala.foreach(parseStatement)
    }
    classStack.pop()
  }

  private def parseStatement(ctx: StatementContext)(implicit classStack: mutable.Stack[String]): Unit = ctx match {
    case ctx: ExpressionOrCommandStatementContext => parseExpressionOrCommand(ctx.expressionOrCommand())
    case _                                        =>
  }

  private def parseExpressionOrCommand(
    ctx: ExpressionOrCommandContext
  )(implicit classStack: mutable.Stack[String]): Unit = ctx match {
    case ctx: ExpressionExpressionOrCommandContext => parseExpressionContext(ctx.expression())
    case _                                         =>
  }

  private def parseExpressionContext(ctx: ExpressionContext)(implicit classStack: mutable.Stack[String]): Unit =
    ctx match {
      case ctx: PrimaryExpressionContext => parsePrimaryContext(ctx.primary())
      case _                             =>
    }

  private def parsePrimaryContext(ctx: PrimaryContext)(implicit classStack: mutable.Stack[String]): Unit = ctx match {
    case ctx: MethodDefinitionPrimaryContext => parseMethodDefinitionContext(ctx.methodDefinition())
    case _                                   =>
  }

  private def parseMethodDefinitionContext(
    ctx: MethodDefinitionContext
  )(implicit classStack: mutable.Stack[String]): Unit = {
    val maybeMethodName = Option(ctx.methodNamePart()) match
      case Some(ctxMethodNamePart) =>
        readMethodNamePart(ctxMethodNamePart)
      case None =>
        readMethodIdentifier(ctx.methodIdentifier())

    maybeMethodName.foreach { methodName =>
      val classType = if (classStack.isEmpty) "Standalone" else classStack.top
      val classPath = classStack.reverse.toList.mkString(".")
      RubySrc2Cpg.packageTableInfo.addPackageMethod(PackageTable.InternalModule, methodName, classPath, classType)
    }
  }

  private def readMethodNamePart(ctx: MethodNamePartContext): Option[String] = {
    ctx match
      case context: SimpleMethodNamePartContext =>
        Option(context.definedMethodName().methodName()) match
          case Some(methodNameCtx) => Try(methodNameCtx.methodIdentifier().getText).toOption
          case None                => None
      case context: SingletonMethodNamePartContext =>
        Option(context.definedMethodName().methodName()) match
          case Some(methodNameCtx) => Try(methodNameCtx.methodIdentifier().getText).toOption
          case None                => None
      case _ => None
  }

  private def readMethodIdentifier(ctx: MethodIdentifierContext): Option[String] = {
    if (ctx.methodOnlyIdentifier() != null) {
      readMethodOnlyIdentifier(ctx.methodOnlyIdentifier())
    } else if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null) {
      Option(ctx.LOCAL_VARIABLE_IDENTIFIER().getSymbol.getText)
    } else {
      None
    }
  }

  private def readMethodOnlyIdentifier(ctx: MethodOnlyIdentifierContext): Option[String] = {
    if (ctx.LOCAL_VARIABLE_IDENTIFIER() != null || ctx.CONSTANT_IDENTIFIER() != null) {
      text(ctx)
    } else {
      None
    }
  }

  private def text(ctx: ParserRuleContext): Option[String] = Try {
    val a     = ctx.getStart.getStartIndex
    val b     = ctx.getStop.getStopIndex
    val intv  = new Interval(a, b)
    val input = ctx.getStart.getInputStream
    input.getText(intv)
  }.toOption

}
