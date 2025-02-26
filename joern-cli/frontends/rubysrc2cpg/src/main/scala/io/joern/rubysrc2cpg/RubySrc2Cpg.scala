package io.joern.rubysrc2cpg

import better.files.File
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.StatementList
import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.rubysrc2cpg.parser.*
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
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.passes.CpgPassBase
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory
import upickle.default.*

import java.nio.file.{Files, Paths}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

class RubySrc2Cpg extends X2CpgFrontend[Config] {

  private val logger                                     = LoggerFactory.getLogger(this.getClass)
  private var rubyAstGenRunner: Option[RubyAstGenRunner] = None

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
      val astGenResult = rubyAstGenRunner match {
        case Some(astGenRunner) =>
          astGenRunner.execute(tmpDir, config)
        case None =>
          val astGenRunner = RubyAstGenRunner(config)
          rubyAstGenRunner = Option(astGenRunner)
          astGenRunner.execute(tmpDir, config)
      }

      val astCreators = ConcurrentTaskUtil
        .runUsingThreadPool(
          RubySrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config, cpg.metaData.root.headOption)
        )
        .flatMap {
          case Failure(exception)  => logger.warn(s"Unable to parse Ruby file, skipping -", exception); None
          case Success(astCreator) => Option(astCreator)
        }
        .filter { x =>
          if x.fileContent.isBlank then logger.info(s"File content empty, skipping - ${x.fileName}")
          !x.fileContent.isBlank
        }

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

  override def close(): Unit = {
    val closeTask = Try(rubyAstGenRunner.foreach(_.close))
    if (closeTask.isFailure) {
      logger.error("Error occurred while cleaning up RubyAstGenRunner!", closeTask.failed.get)
    }
  }

}

object RubySrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = {
    val implicitRequirePass = if (cpg.dependency.name.contains("zeitwerk")) ImplicitRequirePass(cpg) :: Nil else Nil
    implicitRequirePass ++ List(ImportsPass(cpg), RubyImportResolverPass(cpg)) ++
      new RubyTypeRecoveryPassGenerator(cpg, config = XTypeRecoveryConfig(iterations = 4))
        .generate() ++ List(new RubyTypeHintCallLinker(cpg), new NaiveCallLinker(cpg), new AstLinkerPass(cpg))
  }

  /** Parses the generated AST Gen files in parallel and produces AstCreators from each.
    */
  def processAstGenRunnerResults(
    astFiles: List[String],
    config: Config,
    projectRoot: Option[String]
  ): Iterator[() => AstCreator] = {
    astFiles.map { fileName => () =>
      val parserResult   = RubyJsonParser.readFile(Paths.get(fileName))
      val rubyProgram    = new RubyJsonToNodeCreator().visitProgram(parserResult.json)
      val sourceFileName = parserResult.fullPath
      val fileContent    = File(sourceFileName).contentAsString
      new AstCreator(
        sourceFileName,
        projectRoot,
        enableFileContents = !config.disableFileContent,
        fileContent = fileContent,
        rootNode = rubyProgram
      )(config.schemaValidation)
    }.iterator
  }

}
