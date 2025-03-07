package io.joern.csharpsrc2cpg

import io.joern.csharpsrc2cpg.CSharpSrc2Cpg.findBuildFiles
import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.csharpsrc2cpg.parser.DotNetJsonParser
import io.joern.csharpsrc2cpg.passes.{AstCreationPass, DependencyPass}
import io.joern.csharpsrc2cpg.utils.{
  DependencyDownloader,
  DotNetAstGenRunner,
  ImplicitUsingsCollector,
  ProgramSummaryCreator
}
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.astgen.AstGenRunner.AstGenRunnerResult
import io.joern.x2cpg.astgen.ParserResult
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.passes.frontend.{MetaDataPass, TypeNodePass}
import io.joern.x2cpg.utils.{ConcurrentTaskUtil, Environment, FileUtil, HashUtil, Report}
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.Languages
import io.shiftleft.passes.CpgPassBase
import org.slf4j.LoggerFactory

import java.nio.file.Paths
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {

  private val logger         = LoggerFactory.getLogger(getClass)
  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      FileUtil.usingTemporaryDirectory("csharpsrc2cpgOut") { tmpDir =>
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = CSharpSrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config)
        val buildFiles   = findBuildFiles(config)
        val localSummary = ProgramSummaryCreator
          .from(astCreators, config)
          .addGlobalImports(ImplicitUsingsCollector.collect(buildFiles).toSet)

        val hash = HashUtil.sha256(astCreators.map(_.parserResult).map(x => Paths.get(x.fullPath)))
        new MetaDataPass(cpg, Languages.CSHARPSRC, config.inputPath, Option(hash)).createAndApply()

        val packageIds = mutable.HashSet.empty[String]
        new DependencyPass(cpg, buildFiles, packageIds.add).createAndApply()
        // If "download dependencies" is enabled, then fetch dependencies and resolve their symbols for additional types
        val programSummary = if (config.downloadDependencies) {
          DependencyDownloader(cpg, config, localSummary, packageIds.toSet).download()
        } else {
          localSummary
        }
        new AstCreationPass(cpg, astCreators.map(_.withSummary(programSummary)), report).createAndApply()
        TypeNodePass.withTypesFromCpg(cpg).createAndApply()
        report.print()
      }
    }
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
                handleWinUserTemp(parserResult.fullPath, config.inputPath)
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

  def findBuildFiles(config: Config): List[String] = {
    SourceFiles.determine(
      config.inputPath,
      Set(".csproj"),
      Option(config.defaultIgnoredFilesRegex),
      Option(config.ignoredFilesRegex),
      Option(config.ignoredFiles)
    )
  }

  /** Addresses behaviour in Windows where a user-specific temp folder is used: parserResult.fullPath =
    * C:\Users\runneradmin\AppData\Local\Temp\... config.inputPath = C:\Users\RUNNER~1\AppData\Local\Temp\...
    *
    * @param inputPath
    *   the user-specified input path.
    * @param parserResultFullPath
    *   the full path according to the parser result.
    * @return
    *   the relative file, robust to user-specific temporary folders are used, as is the case with GitHub runners.
    */
  private def handleWinUserTemp(parserResultFullPath: String, inputPath: String): String = {
    if (parserResultFullPath.contains("Temp") && inputPath.contains("Temp")) {
      SourceFiles.toRelativePath(
        parserResultFullPath.substring(parserResultFullPath.indexOf("Temp")),
        inputPath.substring(inputPath.indexOf("Temp"))
      )
    } else {
      SourceFiles.toRelativePath(parserResultFullPath, inputPath)
    }
  }

}
