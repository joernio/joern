package io.joern.csharpsrc2cpg

import better.files.File
import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.csharpsrc2cpg.parser.DotNetJsonParser
import io.joern.csharpsrc2cpg.passes.AstCreationPass
import io.joern.csharpsrc2cpg.utils.DotNetAstGenRunner
import io.joern.x2cpg.X2Cpg.withNewEmptyCpg
import io.joern.x2cpg.{SourceFiles, X2CpgFrontend}
import io.joern.x2cpg.passes.callgraph.NaiveCallLinker
import io.joern.x2cpg.utils.Report
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.CpgPassBase

import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.util.Try

class CSharpSrc2Cpg extends X2CpgFrontend[Config] {
  private val report: Report = new Report()

  override def createCpg(config: Config): Try[Cpg] = {
    withNewEmptyCpg(config.outputPath, config) { (cpg, config) =>
      File.usingTemporaryDirectory("csharpsrc2cpgOut") { tmpDir =>
        val astGenResult = new DotNetAstGenRunner(config).execute(tmpDir)
        val astCreators  = CSharpSrc2Cpg.processAstGenRunnerResults(astGenResult.parsedFiles, config)
        new AstCreationPass(cpg, astCreators, report).createAndApply()
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
              val parserResult     = DotNetJsonParser.readFile(Paths.get(file))
              val relativeFileName = SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
              println(s"""
                   |DEBUG
                   |
                   |  - ${parserResult.fullPath}
                   |  - ${config.inputPath}
                   |  - $relativeFileName
                   |
                   |""".stripMargin)
              new AstCreator(relativeFileName, parserResult)(config.schemaValidation)
            }
          )
      ),
      Duration.Inf
    )
  }

}
