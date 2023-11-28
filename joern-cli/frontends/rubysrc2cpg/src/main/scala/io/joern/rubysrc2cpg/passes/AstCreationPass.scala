package io.joern.rubysrc2cpg.passes

import io.joern.rubysrc2cpg.Config
import io.joern.rubysrc2cpg.astcreation.AstCreator
import io.joern.rubysrc2cpg.parser.ResourceManagedParser
import io.joern.x2cpg.SourceFiles
import io.shiftleft.codepropertygraph.Cpg
import io.shiftleft.passes.ConcurrentWriterCpgPass
import io.shiftleft.semanticcpg.language.*
import org.slf4j.LoggerFactory

class AstCreationPass(cpg: Cpg, parser: ResourceManagedParser, config: Config)
    extends ConcurrentWriterCpgPass[String](cpg) {

  private val logger                   = LoggerFactory.getLogger(getClass)
  private val RubySourceFileExtensions = Set(".rb")

  override def generateParts(): Array[String] = {
    SourceFiles
      .determine(
        config.inputPath,
        RubySourceFileExtensions,
        ignoredFilesRegex = Option(config.ignoredFilesRegex),
        ignoredFilesPath = Option(config.ignoredFiles)
      )
      .toArray
  }

  override def runOnPart(diffGraph: DiffGraphBuilder, fileName: String): Unit = {
    try {
      // TODO: Should the parsing step be moved here?
      val astCreator = new AstCreator(fileName, parser, cpg.metaData.root.headOption)(config.schemaValidation)
      val ast        = astCreator.createAst()
      diffGraph.absorb(ast)
    } catch {
      case ex: Exception =>
        logger.error(s"Error while processing AST for file - $fileName - ", ex)
    }
  }
}
