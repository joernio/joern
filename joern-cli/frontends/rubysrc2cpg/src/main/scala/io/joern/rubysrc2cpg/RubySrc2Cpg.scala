package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.StatementList
import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.rubysrc2cpg.parser.{
  RubyAstGenRunner,
  RubyJsonParser,
  RubyJsonToNodeCreator,
  RubyNodeCreator,
  RubyParser
}
import io.joern.rubysrc2cpg.passes.{
  AstCreationPass,
  ConfigFileCreationPass,
  DependencyPass,
  DependencySummarySolverPass
}
import io.joern.rubysrc2cpg.utils.DependencyDownloader
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.frontendspecific.rubysrc2cpg.*
import io.joern.x2cpg.passes.base.AstLinkerPass
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass, XTypeRecoveryConfig}
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, ExternalCommand}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.nio.file.{Files, Paths}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger = LoggerFactory.getLogger(this.getClass)

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg).createAndApply()
      new DependencyPass(cpg).createAndApply()
      createCpgAction(cpg, config)
    }
  }

  private def createCpgAction(cpg: Cpg, config: Config): Unit = {
    File.usingTemporaryDirectory("rubysrc2cpgOut") { tmpDir =>
      val astGenResult = RubyAstGenRunner(config).execute(tmpDir)
      /*val astCreators  = */
      RubySrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config)
    }
    Using.resource(
      new parser.ResourceManagedParser(config.antlrCacheMemLimit, config.antlrDebug, config.antlrProfiling)
    ) { parser =>
      val astCreators = ConcurrentTaskUtil
        .runUsingThreadPool(RubySrc2Cpg.generateParserTasks(parser, config, cpg.metaData.root.headOption))
        .flatMap {
          case Failure(exception)  => logger.warn(s"Could not parse file, skipping - ", exception); None
          case Success(astCreator) => Option(astCreator)
        }
        .filter(x => {
          if x.fileContent.isBlank then logger.info(s"File content empty, skipping - ${x.fileName}")

          !x.fileContent.isBlank
        })

      // Pre-parse the AST creators for high level structures
      val internalProgramSummary = ConcurrentTaskUtil
        .runUsingThreadPool(astCreators.map(x => () => x.summarize()).iterator)
        .flatMap {
          case Failure(exception) => logger.warn(s"Unable to pre-parse Ruby file, skipping - ", exception); None
          case Success(summary)   => Option(summary)
        }
        .foldLeft(RubyProgramSummary(RubyProgramSummary.BuiltinTypes(config.typeStubMetaData)))(_ ++= _)

      val dependencySummary = if (config.downloadDependencies) {
        DependencyDownloader(cpg).download()
      } else {
        RubyProgramSummary()
      }

      val programSummary = internalProgramSummary ++= dependencySummary

      AstCreationPass(cpg, astCreators.map(_.withSummary(programSummary))).createAndApply()
      if config.downloadDependencies then {
        DependencySummarySolverPass(cpg, dependencySummary).createAndApply()
      }
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
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

  private val RubySourceFileExtensions: Set[String] = Set(".rb")

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = {
    val implicitRequirePass = if (cpg.dependency.name.contains("zeitwerk")) ImplicitRequirePass(cpg) :: Nil else Nil
    implicitRequirePass ++ List(ImportsPass(cpg), RubyImportResolverPass(cpg)) ++
      new RubyTypeRecoveryPassGenerator(cpg, config = XTypeRecoveryConfig(iterations = 4))
        .generate() ++ List(new RubyTypeHintCallLinker(cpg), new NaiveCallLinker(cpg), new AstLinkerPass(cpg))
  }

  /** Parses the generated AST Gen files in parallel and produces AstCreators from each.
    */
  def processAstGenRunnerResults(astFiles: List[String], config: Config): Unit /*Seq[AstCreator]*/ = {
    Await.result(
      Future.sequence(
        astFiles
          .map(fileName =>
            Future {
              val parserResult     = RubyJsonParser.readFile(Paths.get(fileName))
              val relativeFileName = SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
              val rubyProgram      = new RubyJsonToNodeCreator().visitProgram(parserResult.json)
              val fileContent      = (File(config.inputPath) / fileName).contentAsString
//              new AstCreator(
//                fileName,
//                ctx,
//                projectRoot,
//                enableFileContents = !config.disableFileContent,
//                fileContent = fileContent,
//                rootNode = Option(new RubyNodeCreator().visit(ctx).asInstanceOf[StatementList])
//              )(config.schemaValidation)
            }
          )
      ),
      Duration.Inf
    )
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
          case Success(ctx) =>
            val fileContent = (File(config.inputPath) / fileName).contentAsString
            new AstCreator(
              fileName,
              ctx,
              projectRoot,
              enableFileContents = !config.disableFileContent,
              fileContent = fileContent,
              rootNode = Option(new RubyNodeCreator().visit(ctx).asInstanceOf[StatementList])
            )(config.schemaValidation)
        }
      }
      .iterator
  }

}
