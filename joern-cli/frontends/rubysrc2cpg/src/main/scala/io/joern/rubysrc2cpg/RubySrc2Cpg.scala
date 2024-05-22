package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser
import io.joern.rubysrc2cpg.deprecated.parser.DeprecatedRubyParser.*
import io.joern.rubysrc2cpg.parser.RubyParser
import io.joern.rubysrc2cpg.passes.{
  AstCreationPass,
  ConfigFileCreationPass,
  DependencyPass,
  ImplicitRequirePass,
  ImportsPass
}
import io.joern.rubysrc2cpg.utils.DependencyDownloader
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
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Paths}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      new DependencyPass(cpg).createAndApply()
      if (config.useDeprecatedFrontend) {
        deprecatedCreateCpgAction(cpg, config)
      } else {
        newCreateCpgAction(cpg, config)
      }
    }
  }

  private def newCreateCpgAction(cpg: Cpg, config: Config): Unit = {
    Using.resource(new parser.ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
      val astCreators = ConcurrentTaskUtil
        .runUsingThreadPool(RubySrc2Cpg.generateParserTasks(parser, config, cpg.metaData.root.headOption))
        .flatMap {
          case Failure(exception)  => logger.warn(s"Could not parse file, skipping - ", exception); None
          case Success(astCreator) => Option(astCreator)
        }
      // Pre-parse the AST creators for high level structures
      val internalProgramSummary = ConcurrentTaskUtil
        .runUsingThreadPool(astCreators.map(x => () => x.summarize()).iterator)
        .flatMap {
          case Failure(exception) => logger.warn(s"Unable to pre-parse Ruby file, skipping - ", exception); None
          case Success(summary)   => Option(summary)
        }
        .foldLeft(RubyProgramSummary(RubyProgramSummary.BuiltinTypes(config.typeStubMetaData)))(_ ++ _)

      val programSummary = if (config.downloadDependencies) {
        DependencyDownloader(cpg, internalProgramSummary).download()
      } else {
        internalProgramSummary
      }

      AstCreationPass(cpg, astCreators.map(_.withSummary(programSummary))).createAndApply()
      if (cpg.dependency.name.contains("zeitwerk")) ImplicitRequirePass(cpg, programSummary).createAndApply()
      ImportsPass(cpg).createAndApply()
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
    }
  }

  private def deprecatedCreateCpgAction(cpg: Cpg, config: Config): Unit = try {
    Using.resource(new deprecated.astcreation.ResourceManagedParser(config.antlrCacheMemLimit)) { parser =>
      if (config.downloadDependencies && !scala.util.Properties.isWin) {
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
            RubySrc2Cpg.RubySourceFileExtensions,
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

      new io.joern.rubysrc2cpg.deprecated.ParseInternalStructures(parsedFiles, cpg.metaData.root.headOption)
        .populatePackageTable()
      val astCreationPass =
        new deprecated.passes.AstCreationPass(cpg, parsedFiles, RubySrc2Cpg.packageTableInfo, config)
      astCreationPass.createAndApply()
    }
  } finally {
    RubySrc2Cpg.packageTableInfo.clear()
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

  // TODO: Global mutable state is bad and should be avoided in the next iteration of the Ruby frontend
  val packageTableInfo                              = new deprecated.utils.PackageTable()
  private val RubySourceFileExtensions: Set[String] = Set(".rb")

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

  def generateParserTasks(
    resourceManagedParser: parser.ResourceManagedParser,
    config: Config,
    projectRoot: Option[String]
  ): Iterator[() => AstCreator] = {
    SourceFiles
      .determine(
        config.inputPath,
        RubySourceFileExtensions,
        ignoredDefaultRegex = Option(config.defaultIgnoredFilesRegex),
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .map { fileName => () =>
        resourceManagedParser.parse(File(config.inputPath), fileName) match {
          case Failure(exception) => throw exception
          case Success(ctx)       => new AstCreator(fileName, ctx, projectRoot)(config.schemaValidation)
        }
      }
      .iterator
  }

}
