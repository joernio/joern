package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.passes.{AstCreationPass, ConfigFileCreationPass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, ExternalCommand}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import org.antlr.v4.runtime.ParserRuleContext
import org.antlr.v4.runtime.misc.Interval
import org.slf4j.LoggerFactory

import java.io.File as JFile
import java.nio.file.{Files, Paths}
import scala.collection.mutable
import scala.jdk.CollectionConverters.*
import scala.util.{Failure, Success, Try, Using}
class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger                                = LoggerFactory.getLogger(this.getClass)
  private val RubySourceFileExtensions: Set[String] = Set(".rb")

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      if (config.useDeprecatedFrontend) {
        deprecatedCreateCpgAction(cpg, config)
      } else {
        newCreateCpgAction(cpg, config)
      }
    }
  }

  private def newCreateCpgAction(cpg: Cpg, config: Config): Unit = {
    Using.resource(new parser.ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
      // TODO: enableDependencyDownload
      val astCreationPass = new AstCreationPass(cpg, parser, config)
      astCreationPass.createAndApply()
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
    }
  }

  private def deprecatedCreateCpgAction(cpg: Cpg, config: Config): Unit = {
    Using.resource(new deprecated.astcreation.ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
      if (config.enableDependencyDownload && !scala.util.Properties.isWin) {
        val tempDir = File.newTemporaryDirectory()
        try {
          downloadDependency(config.inputPath, tempDir.toString())
          new deprecated.passes.AstPackagePass(
            cpg,
            tempDir.toString(),
            parser,
            RubySrc2Cpg.packageTableInfo,
            config.inputPath
          )(config.schemaValidation).createAndApply()
        } finally {
          tempDir.delete()
        }
      }
      val parsedFiles = {
        val tasks = SourceFiles
          .determine(
            config.inputPath,
            RubySourceFileExtensions,
            ignoredFilesRegex = Option(config.ignoredFilesRegex),
            ignoredFilesPath = Option(config.ignoredFiles)
          )
          .map(x =>
            () =>
              parser.parse(x) match
                case Failure(exception) =>
                  logger.warn(s"Could not parse file: $x, skipping", exception); throw exception
                case Success(ast) => x -> ast
          )
          .iterator
        ConcurrentTaskUtil.runUsingThreadPool(tasks).flatMap(_.toOption)
      }

      new ParseInternalStructures(parsedFiles, cpg.metaData.root.headOption).populatePackageTable()
      val astCreationPass =
        new deprecated.passes.AstCreationPass(cpg, parsedFiles, RubySrc2Cpg.packageTableInfo, config)
      astCreationPass.createAndApply()
    }
  }

  private def downloadDependency(inputPath: String, tempPath: String): Unit = {
    if (Files.isRegularFile(Paths.get(s"${inputPath}${java.io.File.separator}Gemfile"))) {
      ExternalCommand.run(s"bundle config set --local path ${tempPath}", inputPath) match {
        case Success(configOutput) =>
          logger.info(s"Gem config successfully done: $configOutput")
        case Failure(exception) =>
          logger.error(s"Error while configuring Gem Path: ${exception.getMessage}")
      }
      val command = s"bundle install"
      ExternalCommand.run(command, inputPath) match {
        case Success(bundleOutput) =>
          logger.info(s"Dependency installed successfully: $bundleOutput")
        case Failure(exception) =>
          logger.error(s"Error while downloading dependency: ${exception.getMessage}")
      }
    }
  }
}

object RubySrc2Cpg {

  val packageTableInfo = new deprecated.utils.PackageTable()

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = {
    if (config.useDeprecatedFrontend) {
      List(new deprecated.passes.RubyImportResolverPass(cpg, packageTableInfo))
        ++ new deprecated.passes.RubyTypeRecoveryPassGenerator(cpg).generate() ++ List(
          new deprecated.passes.RubyTypeHintCallLinker(cpg),
          new NaiveCallLinker(cpg),

          // Some of passes above create new methods, so, we
          // need to run the ASTLinkerPass one more time
          new AstLinkerPass(cpg)
        )
    } else {
      List()
    }
  }

}

class ParseInternalStructures(
  parsedFiles: List[(String, DeprecatedRubyParser.ProgramContext)],
  projectRoot: Option[String] = None
) {

  private val logger                            = LoggerFactory.getLogger(getClass)
  private val classStack: mutable.Stack[String] = mutable.Stack[String]()

  def populatePackageTable(): Unit = {
    val tasks = parsedFiles.map { case (fileName, programCtx) =>
      () => {
        val relativeFilename: String =
          projectRoot.map(fileName.stripPrefix).map(_.stripPrefix(JFile.separator)).getOrElse(fileName)
        parseForStructures(relativeFilename, programCtx)
      }
    }.iterator
    ConcurrentTaskUtil.runUsingThreadPool(tasks).foreach {
      case Failure(exception) =>
        logger.warn("Exception encountered while scanning for internal structures", exception)
      case _ => // do nothing
    }
  }

  private def parseForStructures(relativeFilename: String, programCtx: ProgramContext): Unit = {
    val name     = ":program"
    val fullName = s"$relativeFilename:$name"
    classStack.push(fullName)
    if (
      programCtx.compoundStatement() != null &&
      programCtx.compoundStatement().statements() != null
    ) {
      programCtx.compoundStatement().statements().statement().asScala.foreach(parseStatement)
    }
  }

  private def parseStatement(ctx: StatementContext): Unit = ctx match {
    case ctx: ExpressionOrCommandStatementContext => parseExpressionOrCommand(ctx.expressionOrCommand())
    case _                                        =>
  }

  private def parseExpressionOrCommand(ctx: ExpressionOrCommandContext): Unit = ctx match {
    case ctx: ExpressionExpressionOrCommandContext => parseExpressionContext(ctx.expression())
    case _                                         =>
  }

  private def parsePrimaryContext(ctx: PrimaryContext): Unit = ctx match {
    case ctx: MethodDefinitionPrimaryContext => parseMethodDefinitionContext(ctx.methodDefinition())
    case _                                   =>
  }

  private def parseExpressionContext(ctx: ExpressionContext): Unit = ctx match {
    case ctx: PrimaryExpressionContext => parsePrimaryContext(ctx.primary())
    case _                             =>
  }

  private def parseMethodDefinitionContext(ctx: MethodDefinitionContext): Unit = {
    val maybeMethodName = Option(ctx.methodNamePart()) match
      case Some(ctxMethodNamePart) =>
        readMethodNamePart(ctxMethodNamePart)
      case None =>
        readMethodIdentifier(ctx.methodIdentifier())

    maybeMethodName.foreach { methodName =>
      val methodFullName = classStack.reverse :+ methodName mkString "."
      // TODO: Insert this into the package table
      RubySrc2Cpg.packageTableInfo.addPackageMethod()
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
