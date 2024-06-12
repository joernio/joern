package io.joern.gosrc2cpg.passes

import better.files.File
import io.joern.gosrc2cpg.Config
import io.joern.gosrc2cpg.astcreation.AstCreator
import io.joern.gosrc2cpg.datastructures.GoGlobal
import io.joern.gosrc2cpg.model.GoModHelper
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.codepropertygraph.generated.DiffGraphBuilder
import org.slf4j.LoggerFactory

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class MethodAndTypeCacheBuilderPass(
  cpgOpt: Option[Cpg],
  astFiles: List[String],
  config: Config,
  goMod: GoModHelper,
  goGlobal: GoGlobal,
  tmpDir: Option[File] = None
) {
  private val logger = LoggerFactory.getLogger(getClass)
  def process(): Seq[AstCreator] = {
    val futures = astFiles
      .map(file =>
        Future {
          try {
            val relFilePath = tmpDir.map(dir => {
              SourceFiles.toRelativePath(file, dir.pathAsString).replace(".json", "")
            })
            val astCreator =
              new AstCreator(file, relFilePath.getOrElse("dummyfile.go"), goMod, goGlobal, tmpDir)(
                config.schemaValidation
              )
            val diffGraph = astCreator.buildCache(cpgOpt)
            if (goGlobal.processingDependencies) {
              astCreator.cleanup()
            } else {
              astCreator.cacheSerializeAndStore()
            }
            Some(astCreator, diffGraph)
          } catch
            case exception: Exception =>
              logger.error(s"error while processing file $file", exception)
              None
        }
      )
    val allResults: Future[List[Option[(AstCreator, DiffGraphBuilder)]]] = Future.sequence(futures)
    val results                                                          = Await.result(allResults, Duration.Inf)
    results.flatMap(result =>
      result.flatMap((astCreator, diffGraph) => {
        cpgOpt.map { cpg =>
          overflowdb.BatchedUpdate
            .applyDiff(cpg.graph, diffGraph, null, null)
            .transitiveModifications()
        }
        Some(astCreator)
      })
    )
  }
}
