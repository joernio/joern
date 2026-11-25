package io.joern.rubysrc2cpg

import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.astcreation.RubyIntermediateAst.StatementList
import io.joern.rubysrc2cpg.datastructures.RubyProgramSummary
import io.joern.rubysrc2cpg.parser.*
import io.joern.rubysrc2cpg.parser.RubyAstGenRunner.JRubyEnvironment
import io.joern.rubysrc2cpg.passes.{AstCreationPass, ConfigFileCreationPass, DependencyPass}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.report.{Category, ScanReport}
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.{Cpg, Languages}
import io.shiftleft.semanticcpg.language.*
import io.shiftleft.semanticcpg.utils.FileUtil
import org.slf4j.LoggerFactory
import upickle.default.*

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try, Using}

class RubySrc2Cpg(sharedJRubyEnv: Option[JRubyEnvironment] = None) extends X2CpgFrontend {
  override type ConfigType = Config
  override val defaultConfig: Config = Config()

  private val logger                                  = LoggerFactory.getLogger(this.getClass)
  private var ownedJRubyEnv: Option[JRubyEnvironment] = None

  /** The JRuby environment is expensive to set up. It is shared by all runs of this frontend, but every run gets its
    * own [[RubyAstGenRunner]], which reports to the [[ScanReport]] of that run.
    */
  private def jrubyEnv: JRubyEnvironment = sharedJRubyEnv.getOrElse {
    synchronized {
      ownedJRubyEnv.getOrElse {
        val environment = JRubyEnvironment()
        ownedJRubyEnv = Option(environment)
        environment
      }
    }
  }

  override def createCpg(config: Config): Try[Cpg] = createCpg(config, new ScanReport())

  override def createCpg(config: Config, report: ScanReport): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config: Config) { (cpg, config) =>
      new MetaDataPass(cpg, Languages.RUBYSRC, config.inputPath).createAndApply()
      new ConfigFileCreationPass(cpg, config).createAndApply()
      new DependencyPass(cpg).createAndApply()
      createCpgAction(cpg, config, report)
    }
  }

  private def createCpgAction(cpg: Cpg, config: Config, report: ScanReport): Unit = {
    FileUtil.usingTemporaryDirectory("rubysrc2cpgOut") { tmpDir =>
      val astGenResult = RubyAstGenRunner(config, Option(jrubyEnv), report).execute(tmpDir, config)

      val astCreators = astGenResult.parsedFiles
        .zip(
          ConcurrentTaskUtil.runUsingThreadPool(
            RubySrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config, cpg.metaData.root.headOption)
          )
        )
        .flatMap {
          case (astFile, Failure(exception)) =>
            // astgen writes `<tmpDir>/<relative source file>.json`
            val sourceFile = astFile.stripSuffix(".json").replace(tmpDir.toString, config.inputPath)
            report.warn(
              Category.FileParse,
              "Unable to parse Ruby file, skipping",
              Some(SourceFiles.toRelativePathIfBelow(sourceFile, config.inputPath)),
              Some(exception),
              logger
            )
            None
          case (_, Success(astCreator)) => Option(astCreator)
        }
        .filter { x =>
          if (x.fileContent.isBlank) { logger.info(s"File content empty, skipping - ${x.fileName}") }
          !x.fileContent.isBlank || x.fileName.endsWith(".html.erb")
        }

      val internalProgramSummary = astCreators
        .zip(ConcurrentTaskUtil.runUsingThreadPool(astCreators.map(creator => () => creator.summarize()).iterator))
        .flatMap {
          case (astCreator, Failure(exception)) =>
            report.warn(
              Category.FileProcessing,
              "Unable to pre-parse Ruby file, skipping",
              Some(astCreator.relativeFileName),
              Some(exception),
              logger
            )
            None
          case (_, Success(summary)) => Option(summary)
        }
        .foldLeft(RubyProgramSummary(RubyProgramSummary.BuiltinTypes(config)))(_ ++= _)

      AstCreationPass(cpg, astCreators.map(_.withSummary(internalProgramSummary)), report).createAndApply()
      TypeNodePass.withTypesFromCpg(cpg).createAndApply()
    }

  }

  override def close(): Unit = {
    val closeTask = Try(ownedJRubyEnv.foreach(_.close()))
    if (closeTask.isFailure) {
      logger.error("Error occurred while cleaning up the JRuby environment!", closeTask.failed.get)
    }
  }

}

object RubySrc2Cpg {

  /** Parses the generated AST Gen files in parallel and produces AstCreators from each.
    */
  def processAstGenRunnerResults(
    astFiles: List[String],
    config: Config,
    projectRoot: Option[String]
  ): Iterator[() => AstCreator] = {
    astFiles.map { fileName => () =>
      val parserResult   = RubyJsonParser.readFile(Paths.get(fileName))
      val rubyProgram    = new RubyJsonToNodeCreator(fileName = fileName).visitProgram(parserResult.json)
      val sourceFileName = parserResult.fullPath
      val fileContent    = new String(Files.readAllBytes(Paths.get(sourceFileName)), StandardCharsets.UTF_8)
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
