package io.joern.csharpsrc2cpg

import better.files.File
import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.csharpsrc2cpg.parser.DotNetJsonParser
import io.joern.csharpsrc2cpg.passes.{AstCreationPass, DependencyPass}
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.astgen.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.MetaDataPass
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, Environment, HashUtil, Report}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {

  private val logger         = LoggerFactory.getLogger(getClass)
  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("csharpsrc2cpgOut") { tmpDir =>
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = CSharpSrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config)
        // Pre-parse the AST creators for high level structures
        val programSummary = ConcurrentTaskUtil
          .runUsingThreadPool(astCreators.map(x => () => x.summarize()).iterator)
          .flatMap {
            case Failure(exception) => logger.warn(s"Unable to pre-parse C# file, skipping - ", exception); None
            case Success(summary)   => Option(summary)
          }
          .foldLeft(CSharpProgramSummary(CSharpProgramSummary.BuiltinTypes :: Nil))(_ ++ _)

        val hash = HashUtil.sha256(astCreators.map(_.parserResult).map(x => Paths.get(x.fullPath)))
        new MetaDataPass(cpg, Languages.CSHARPSRC, config.inputPath, Option(hash)).createAndApply()
        new DependencyPass(cpg, buildFiles(config)).createAndApply()
        // TODO: Enable download dependencies option
        new AstCreationPass(cpg, astCreators.map(_.withSummary(programSummary)), report).createAndApply()
        report.print()
      }
    }
  }

  private def buildFiles(config: Config): List[String] = {
    SourceFiles.determine(
      config.inputPath,
      Set(".csproj"),
      Option(config.defaultIgnoredFilesRegex),
      Option(config.ignoredFilesRegex),
      Option(config.ignoredFiles)
    )
  }

}

object CSharpSrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

  /** Parses the generated AST Gen files in parallel and produces AstCreators from each.
    */
  def processAstGenRunnerResults(astFiles: List[String], config: Config): Seq[AstCreator] = {
    Await.result(
      Future.sequence(
        astFiles
          .map(file =>
            Future {
              val parserResult = DotNetJsonParser.readFile(Paths.get(file))
              val relativeFileName = if (Environment.operatingSystem == Environment.OperatingSystemType.Windows) {
                /*
                Addresses behaviour in Windows where a user-specific temp folder is used:
                  parserResult.fullPath = C:\Users\runneradmin\AppData\Local\Temp\...
                  config.inputPath = C:\Users\RUNNER~1\AppData\Local\Temp\...
                 */
                SourceFiles.toRelativePath(
                  parserResult.fullPath.substring(parserResult.fullPath.indexOf("Temp")),
                  config.inputPath.substring(config.inputPath.indexOf("Temp"))
                )
              } else {
                SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
              }
              new AstCreator(relativeFileName, parserResult)(config.schemaValidation)
            }
          )
      ),
      Duration.Inf
    )
  }

}
