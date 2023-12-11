package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder

import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class MethodAndTypeCacheBuilderPass(
  cpgOpt: Option[Cpg],
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal
) {
  def process(): Seq[AstCreator] = {
    val futures = astFiles
      .map(file => {
        Future {
          val parserResult    = GoAstJsonParser.readFile(Paths.get(file))
          val relPathFileName = SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
          val astCreator      = new AstCreator(relPathFileName, parserResult, goMod, goGlobal)(config.schemaValidation)
          val diffGraph       = astCreator.buildCache(cpgOpt)
          (astCreator, diffGraph)
        }
      })
    val allResults: Future[List[(AstCreator, DiffGraphBuilder)]] = Future.sequence(futures)
    val results                                                  = Await.result(allResults, Duration.Inf)
    val (astCreators, diffGraphs)                                = results.unzip
    cpgOpt.map { cpg =>
      diffGraphs.foreach { diffGraph =>
        overflowdb.BatchedUpdate
          .applyDiff(cpg.graph, diffGraph, null, null)
          .transitiveModifications()
      }
    }
    astCreators
  }
}
