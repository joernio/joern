package io.joern.csharpsrc2cpg

import better.files.File
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.X2CpgFrontend
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase
import io.joern.x2cpg.utils.Report
import io.joern.x2cpg.SourceFiles
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.csharpsrc2cpg.parser.DotNetJsonParser
import io.joern.csharpsrc2cpg.passes.AstCreationPass
import io.joern.csharpsrc2cpg.astcreation.AstCreator

import java.nio.file.Paths
import scala.util.Try
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("csharpsrc2cpgOut") { tmpDir =>
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = processAstGenRunnerResults(astGenResult.parsedFiles, config)
        new AstCreationPass(cpg, astCreators, report).createAndApply()
        report.print()
      }
    }
  }

  /** Parses the generated AST Gen files in parallel and produces AstCreators from each.
    */
  private def processAstGenRunnerResults(astFiles: List[String], config: Config): Seq[AstCreator] = {
    Await.result(
      Future.sequence(
        astFiles
          .map(file =>
            Future {
              val parserResult    = DotNetJsonParser.readFile(Paths.get(file))
              val relPathFileName = SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
              new AstCreator(relPathFileName, parserResult)(config.schemaValidation)
            }
          )
      ),
      Duration.Inf
    )
  }

}

object CSharpSrc2Cpg {

  def postProcessingPasses(cpg: Cpg, config: Config): List[CpgPassBase] = List(new NaiveCallLinker(cpg))

}
