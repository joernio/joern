package io.joern.gosrc2cpg.passes

import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.{AstCreator, CacheBuilder}
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.gosrc2cpg.parser.GoAstJsonParser
import io.joern.gosrc2cpg.parser.GoAstJsonParser.ParserResult
import io.joern.x2cpg.SourceFiles

import java.nio.file.Paths
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class MethodAndTypeCacheBuilderPass(astFiles: List[String], config: Config, goMod: GoModHelper) {
  def process(): Seq[AstCreator] = {
    val futures = astFiles
      .map(file => {
        Future {
          val parserResult    = GoAstJsonParser.readFile(Paths.get(file))
          val relPathFileName = SourceFiles.toRelativePath(parserResult.fullPath, config.inputPath)
          val astCreator      = new AstCreator(relPathFileName, parserResult, goMod)(config.schemaValidation)
          astCreator.buildCache()
          astCreator
        }
      })
    val allResults: Future[List[AstCreator]] = Future.sequence(futures)
    Await.result(allResults, Duration.Inf)
  }
}
