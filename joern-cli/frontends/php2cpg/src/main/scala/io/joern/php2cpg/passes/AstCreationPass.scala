package io.joern.php2cpg.passes

import io.joern.php2cpg.Config
import io.joern.php2cpg.astcreation.AstCreator
import io.joern.php2cpg.parser.{Domain, PhpParser}
import io.joern.php2cpg.passes.SymbolSummaryPass.SymbolSummary
import io.joern.x2cpg.ValidationMode
import io.shiftleft.codepropertygraph.generated.Cpg
import io.shiftleft.passes.ForkJoinParallelCpgPass
import io.shiftleft.semanticcpg.utils.FileUtil.*

import java.nio.file.Paths

class AstCreationPass(config: Config, cpg: Cpg, parser: PhpParser, summary: Seq[SymbolSummary])
    extends ForkJoinParallelCpgPass[Array[String]](cpg)
    with AstParsingPass(config, parser) {

  override protected def processPart(builder: DiffGraphBuilder, fileName: String, result: Domain.PhpFile): Unit = {
    val relativeFilename = if (fileName == config.inputPath) {
      Paths.get(fileName).fileName
    } else {
      Paths.get(config.inputPath).relativize(Paths.get(fileName)).toString
    }
    builder.absorb(
      new AstCreator(relativeFilename, fileName, result, config.disableFileContent)(config.schemaValidation)
        .createAst()
    )
  }
}
