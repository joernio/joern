package io.joern.csharpsrc2cpg.utils

import io.joern.csharpsrc2cpg.Config
import io.joern.csharpsrc2cpg.astcreation.AstCreator
import io.joern.csharpsrc2cpg.datastructures.CSharpProgramSummary
import io.joern.x2cpg.utils.ConcurrentTaskUtil
import org.slf4j.LoggerFactory

import scala.util.{Failure, Success}

/** Builds a `CSharpProgramSummary` by pre-parsing AST creators for high level structures, taking into account related
  * frontend options.
  */
object ProgramSummaryCreator {

  private val logger = LoggerFactory.getLogger(getClass)

  def from(astCreators: Seq[AstCreator], config: Config): CSharpProgramSummary = {
    val internalSummary = summarizeAstCreators(astCreators)
    val externalSummary = buildExternalSummary(config.useBuiltinSummaries, config.externalSummaryPaths)
    internalSummary.appendImported(externalSummary)
  }

  private def summarizeAstCreators(astCreators: Seq[AstCreator]): CSharpProgramSummary = {
    ConcurrentTaskUtil
      .runUsingThreadPool(astCreators.map(x => () => x.summarize()).iterator)
      .flatMap {
        case Failure(exception) =>
          logger.warn(s"Unable to pre-parse C# file, skipping - ", exception)
          None
        case Success(summary) => Option(summary)
      }
      .foldLeft(CSharpProgramSummary(imports = CSharpProgramSummary.initialImports))(_ ++= _)
  }

  private def buildExternalSummary(withBuiltinTypes: Boolean, withJsonFiles: Set[String]): CSharpProgramSummary = {
    val builtin = if withBuiltinTypes then CSharpProgramSummary.builtinTypesSummary else CSharpProgramSummary()
    val fromJson =
      if withJsonFiles.nonEmpty then CSharpProgramSummary.externalTypesSummary(withJsonFiles)
      else CSharpProgramSummary()
    builtin ++= fromJson
  }
}
